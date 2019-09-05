{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Data.ByteArray.Builder
  ( -- * Bounded Primitives
    Builder(..)
  , construct
  , fromBounded
    -- * Evaluation
  , run
  , pasteST
  , pasteIO
  , pasteGrowST
  , pasteGrowIO
  , pasteArrayST
  , pasteArrayIO
    -- * Materialized Byte Sequences
  , bytes
  , bytearray
  , shortTextUtf8
  , shortTextJsonString
  , cstring
  , stringUtf8
    -- * Encode Integral Types
    -- ** Human-Readable
  , word64Dec
  , word32Dec
  , word16Dec
  , word8Dec
  , wordDec
  , int64Dec
  , int32Dec
  , int16Dec
  , int8Dec
  , intDec
    -- * Unsigned Words
    -- ** 64-bit
  , word64PaddedUpperHex
    -- ** 32-bit
  , word32PaddedUpperHex
    -- ** 16-bit
  , word16PaddedUpperHex
  , word16PaddedLowerHex
  , word16LowerHex
  , word16UpperHex
    -- ** 8-bit
  , word8PaddedUpperHex
  , word8LowerHex
  , ascii
  , char
    -- ** Machine-Readable
  , word64BE
  , word32BE
  , word16BE
  , word8
    -- * Encode Floating-Point Types
    -- ** Human-Readable
  , doubleDec
  ) where

import Control.Monad.Primitive (primitive_)
import Control.Monad.ST (ST,stToIO)
import Control.Monad.ST.Run (runByteArrayST)
import Data.ByteArray.Builder.Unsafe (Builder(Builder))
import Data.ByteArray.Builder.Unsafe (stringUtf8,cstring)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Bytes.Types (Bytes(Bytes),MutableBytes(MutableBytes))
import Data.Char (ord)
import Data.Int (Int64,Int32,Int16,Int8)
import Data.Primitive (ByteArray(..),MutableByteArray(..))
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))
import Data.Text.Short (ShortText)
import Data.Word (Word64,Word32,Word16,Word8)
import GHC.Exts (Int(I#),Char(C#),Int#,State#,ByteArray#,RealWorld,(>=#),(/=#))
import GHC.ST (ST(ST))

import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Types as Arithmetic
import qualified GHC.Exts as Exts
import qualified Data.Text.Short as TS
import qualified Data.Primitive as PM
import qualified Data.Vector as V
import qualified Data.ByteArray.Builder.Bounded as Bounded
import qualified Data.ByteArray.Builder.Bounded.Unsafe as UnsafeBounded

-- | Run a builder. An accurate size hint is important for good performance.
-- The size hint should be slightly larger than the actual size.
run ::
     Int -- ^ Hint for upper bound on size
  -> Builder -- ^ Builder
  -> ByteArray
run hint b = runByteArrayST $ do
  let go !n = do
        arr <- PM.newByteArray n
        pasteST b (MutableBytes arr 0 n) >>= \case
          Nothing -> go (n + 64)
          Just len -> do
            shrinkMutableByteArray arr len
            PM.unsafeFreezeByteArray arr
  go (max hint 1)

-- | Variant of 'pasteArrayST' that runs in 'IO'.
pasteArrayIO ::
     MutableBytes RealWorld -- ^ Buffer
  -> (a -> Builder) -- ^ Builder
  -> V.Vector a -- ^ Elements to serialize
  -> IO (V.Vector a, MutableBytes RealWorld) -- ^ Shifted vector, shifted buffer
pasteArrayIO !arr f !xs = stToIO (pasteArrayST arr f xs)

-- | Fold over a vector, applying the builder to each element until
-- the buffer cannot accomodate any more.
pasteArrayST ::
     MutableBytes s -- ^ Buffer
  -> (a -> Builder) -- ^ Builder
  -> V.Vector a -- ^ Elements to serialize
  -> ST s (V.Vector a, MutableBytes s) -- ^ Shifted vector, shifted buffer
pasteArrayST (MutableBytes arr off0 len0) f !xs0 = do
  let go !xs !ixBufA !lenBufA = if V.length xs > 0
        then do
          let a = V.unsafeHead xs
          pasteST (f a) (MutableBytes arr ixBufA lenBufA) >>= \case
            Nothing -> pure (xs,MutableBytes arr ixBufA lenBufA)
            Just ixBufB ->
              go (V.unsafeTail xs) ixBufB (lenBufA + (ixBufA - ixBufB))
        else pure (xs,MutableBytes arr ixBufA lenBufA)
  go xs0 off0 len0

-- | Paste the builder into the byte array starting at offset zero.
-- This repeatedly reallocates the byte array if it cannot accomodate
-- the builder, replaying the builder each time.
pasteGrowST ::
     Int -- ^ How many bytes to grow by at a time
  -> Builder
  -> MutableByteArrayOffset s
     -- ^ Initial buffer, used linearly. Do not reuse this argument.
  -> ST s (MutableByteArrayOffset s)
     -- ^ Final buffer that accomodated the builder.
pasteGrowST !n b !(MutableByteArrayOffset arr0 off0) = do
  let go !arr !sz = pasteST b (MutableBytes arr off0 (sz - off0)) >>= \case
        Nothing -> do
          let szNext = sz + n
          arrNext <- PM.resizeMutableByteArray arr szNext
          go arrNext szNext
        Just ix -> pure (MutableByteArrayOffset{array=arr,offset=ix})
  go arr0 =<< PM.getSizeofMutableByteArray arr0

-- | Variant of 'pasteGrowST' that runs in 'IO'.
pasteGrowIO ::
     Int -- ^ How many bytes to grow by at a time
  -> Builder
  -> MutableByteArrayOffset RealWorld
     -- ^ Initial buffer, used linearly. Do not reuse this argument.
  -> IO (MutableByteArrayOffset RealWorld)
     -- ^ Final buffer that accomodated the builder.
pasteGrowIO !n b !arr = stToIO (pasteGrowST n b arr)

-- | Execute the builder, pasting its contents into a buffer.
-- If the buffer is not large enough, this returns 'Nothing'.
-- Otherwise, it returns the index in the buffer that follows
-- the payload just written.
pasteST :: Builder -> MutableBytes s -> ST s (Maybe Int)
{-# inline pasteST #-}
pasteST (Builder f) (MutableBytes (MutableByteArray arr) (I# off) (I# len)) =
  ST $ \s0 -> case f arr off len s0 of
    (# s1, r #) -> if Exts.isTrue# (r /=# (-1#))
      then (# s1, Just (I# r) #)
      else (# s1, Nothing #)

-- | Variant of 'pasteST' that runs in 'IO'.
pasteIO :: Builder -> MutableBytes RealWorld -> IO (Maybe Int)
{-# inline pasteIO #-}
pasteIO b m = stToIO (pasteST b m)

-- | Constructor for 'Builder' that works on a function with lifted
-- arguments instead of unlifted ones. This is just as unsafe as the
-- actual constructor.
construct :: (forall s. MutableBytes s -> ST s (Maybe Int)) -> Builder
construct f = Builder
  $ \arr off len s0 ->
    case unST (f (MutableBytes (MutableByteArray arr) (I# off) (I# len))) s0 of
      (# s1, m #) -> case m of
        Nothing -> (# s1, (-1#) #)
        Just (I# n) -> (# s1, n #)

-- | Convert a bounded builder to an unbounded one. If the size
-- is a constant, use @Arithmetic.Nat.constant@ as the first argument
-- to let GHC conjure up this value for you.
fromBounded ::
     Arithmetic.Nat n
  -> Bounded.Builder n
  -> Builder
{-# inline fromBounded #-}
fromBounded n (UnsafeBounded.Builder f) = Builder $ \arr off len s0 ->
  let !(I# req) = Nat.demote n in
  case len >=# req of
    1# -> f arr off s0
    _ -> (# s0, (-1#) #)

-- | Create a builder from an unsliced byte sequence.
bytearray :: ByteArray -> Builder
bytearray a = bytes (Bytes a 0 (PM.sizeofByteArray a))

-- | Create a builder from a sliced byte sequence.
bytes :: Bytes -> Builder
bytes (Bytes src soff slen) = construct $ \(MutableBytes arr off len) -> if len >= slen
  then do
    PM.copyByteArray arr off src soff slen
    pure (Just (off + slen))
  else pure Nothing

-- Internal function. Precondition, the referenced slice of the
-- byte sequence is UTF-8 encoded text.
slicedUtf8TextJson :: ByteArray# -> Int# -> Int# -> Builder
{-# inline slicedUtf8TextJson #-}
slicedUtf8TextJson !src# !soff0# !slen0# = construct $ \(MutableBytes dst doff0 dlen0) ->
  let slen0 = I# slen0#
   in if dlen0 > (2 * slen0) + 2
        then do
          PM.writeByteArray dst doff0 (c2w '"')
          let go !soff !slen !doff = if slen > 0
                then case indexChar8Array (ByteArray src#) soff of
                  '\\' -> write2 dst doff '\\' '\\' *> go (soff + 1) (slen - 1) (doff + 2)
                  '\"' -> write2 dst doff '\\' '\"' *> go (soff + 1) (slen - 1) (doff + 2)
                  '\n' -> write2 dst doff '\\' 'n' *> go (soff + 1) (slen - 1) (doff + 2)
                  '\r' -> write2 dst doff '\\' 'r' *> go (soff + 1) (slen - 1) (doff + 2)
                  '\t' -> write2 dst doff '\\' 't' *> go (soff + 1) (slen - 1) (doff + 2)
                  c -> if c >= '\x20'
                    then PM.writeByteArray dst doff (c2w c) *> go (soff + 1) (slen - 1) (doff + 1)
                    else do
                      write2 dst doff '\\' 'u'
                      doff' <- UnsafeBounded.pasteST
                        (Bounded.word16PaddedUpperHex (fromIntegral (c2w c)))
                        dst (doff + 2)
                      go (soff + 1) (slen - 1) doff'
                else pure doff
          doffRes <- go (I# soff0#) (I# slen0#) (doff0 + 1)
          PM.writeByteArray dst doffRes (c2w '"')
          pure (Just (doffRes + 1))
        else pure Nothing

-- Internal. Write two characters in the ASCII plane to a byte array.
write2 :: MutableByteArray s -> Int -> Char -> Char -> ST s ()
write2 marr ix a b = do
  PM.writeByteArray marr ix (c2w a)
  PM.writeByteArray marr (ix + 1) (c2w b)

-- | Create a builder from text. The text will be UTF-8 encoded.
shortTextUtf8 :: ShortText -> Builder
shortTextUtf8 a =
  let ba = shortTextToByteArray a
   in bytes (Bytes ba 0 (PM.sizeofByteArray ba))

-- | Create a builder from text. The text will be UTF-8 encoded,
-- and JSON special characters will be escaped. Additionally, the
-- result is surrounded by double quotes. For example:
--
-- * @foo ==> "foo"@
-- * @\_"_/ ==> "\\_\"_/"@
-- * @hello<ESC>world ==> "hello\u001Bworld"@ (where <LF> is code point 0x1B)
shortTextJsonString :: ShortText -> Builder
shortTextJsonString a =
  let !(ByteArray ba) = shortTextToByteArray a
      !(I# len) = PM.sizeofByteArray (ByteArray ba)
   in slicedUtf8TextJson ba 0# len

-- | Encodes an unsigned 64-bit integer as decimal.
-- This encoding never starts with a zero unless the
-- argument was zero.
word64Dec :: Word64 -> Builder
word64Dec w = fromBounded Nat.constant (Bounded.word64Dec w)

-- | Encodes an unsigned 16-bit integer as decimal.
-- This encoding never starts with a zero unless the
-- argument was zero.
word32Dec :: Word32 -> Builder
word32Dec w = fromBounded Nat.constant (Bounded.word32Dec w)

-- | Encodes an unsigned 16-bit integer as decimal.
-- This encoding never starts with a zero unless the
-- argument was zero.
word16Dec :: Word16 -> Builder
word16Dec w = fromBounded Nat.constant (Bounded.word16Dec w)

-- | Encodes an unsigned 8-bit integer as decimal.
-- This encoding never starts with a zero unless the
-- argument was zero.
word8Dec :: Word8 -> Builder
word8Dec w = fromBounded Nat.constant (Bounded.word8Dec w)

-- | Encodes an unsigned machine-sized integer as decimal.
-- This encoding never starts with a zero unless the
-- argument was zero.
wordDec :: Word -> Builder
wordDec w = fromBounded Nat.constant (Bounded.wordDec w)

-- | Encode a double-floating-point number, using decimal notation or
-- scientific notation depending on the magnitude. This has undefined
-- behavior when representing @+inf@, @-inf@, and @NaN@. It will not
-- crash, but the generated numbers will be nonsense.
doubleDec :: Double -> Builder
doubleDec w = fromBounded Nat.constant (Bounded.doubleDec w)

-- | Encodes a signed 64-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
-- Negative numbers are preceded by a minus sign. Positive numbers
-- are not preceded by anything.
int64Dec :: Int64 -> Builder
int64Dec w = fromBounded Nat.constant (Bounded.int64Dec w)

-- | Encodes a signed 32-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
-- Negative numbers are preceded by a minus sign. Positive numbers
-- are not preceded by anything.
int32Dec :: Int32 -> Builder
int32Dec w = fromBounded Nat.constant (Bounded.int32Dec w)

-- | Encodes a signed 16-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
-- Negative numbers are preceded by a minus sign. Positive numbers
-- are not preceded by anything.
int16Dec :: Int16 -> Builder
int16Dec w = fromBounded Nat.constant (Bounded.int16Dec w)

-- | Encodes a signed 8-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
-- Negative numbers are preceded by a minus sign. Positive numbers
-- are not preceded by anything.
int8Dec :: Int8 -> Builder
int8Dec w = fromBounded Nat.constant (Bounded.int8Dec w)

-- | Encodes a signed machine-sized integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
-- Negative numbers are preceded by a minus sign. Positive numbers
-- are not preceded by anything.
intDec :: Int -> Builder
intDec w = fromBounded Nat.constant (Bounded.intDec w)

-- | Encode a 64-bit unsigned integer as hexadecimal, zero-padding
-- the encoding to 16 digits. This uses uppercase for the alphabetical
-- digits. For example, this encodes the number 1022 as @00000000000003FE@.
word64PaddedUpperHex :: Word64 -> Builder
word64PaddedUpperHex w =
  fromBounded Nat.constant (Bounded.word64PaddedUpperHex w)

-- | Encode a 32-bit unsigned integer as hexadecimal, zero-padding
-- the encoding to 8 digits. This uses uppercase for the alphabetical
-- digits. For example, this encodes the number 1022 as @000003FE@.
word32PaddedUpperHex :: Word32 -> Builder
word32PaddedUpperHex w =
  fromBounded Nat.constant (Bounded.word32PaddedUpperHex w)

-- | Encode a 16-bit unsigned integer as hexadecimal, zero-padding
-- the encoding to 4 digits. This uses uppercase for the alphabetical
-- digits. For example, this encodes the number 1022 as @03FE@.
word16PaddedUpperHex :: Word16 -> Builder
word16PaddedUpperHex w =
  fromBounded Nat.constant (Bounded.word16PaddedUpperHex w)

-- | Encode a 16-bit unsigned integer as hexadecimal, zero-padding
-- the encoding to 4 digits. This uses lowercase for the alphabetical
-- digits. For example, this encodes the number 1022 as @03fe@.
word16PaddedLowerHex :: Word16 -> Builder
word16PaddedLowerHex w =
  fromBounded Nat.constant (Bounded.word16PaddedLowerHex w)

-- | Encode a 16-bit unsigned integer as hexadecimal without leading
-- zeroes. This uses lowercase for the alphabetical digits. For
-- example, this encodes the number 1022 as @3fe@.
word16LowerHex :: Word16 -> Builder
word16LowerHex w =
  fromBounded Nat.constant (Bounded.word16LowerHex w)

-- | Encode a 16-bit unsigned integer as hexadecimal without leading
-- zeroes. This uses uppercase for the alphabetical digits. For
-- example, this encodes the number 1022 as @3FE@.
word16UpperHex :: Word16 -> Builder
word16UpperHex w =
  fromBounded Nat.constant (Bounded.word16UpperHex w)

-- | Encode a 16-bit unsigned integer as hexadecimal without leading
-- zeroes. This uses lowercase for the alphabetical digits. For
-- example, this encodes the number 1022 as @3FE@.
word8LowerHex :: Word8 -> Builder
word8LowerHex w =
  fromBounded Nat.constant (Bounded.word8LowerHex w)

-- | Encode a 8-bit unsigned integer as hexadecimal, zero-padding
-- the encoding to 2 digits. This uses uppercase for the alphabetical
-- digits. For example, this encodes the number 11 as @0B@.
word8PaddedUpperHex :: Word8 -> Builder
word8PaddedUpperHex w =
  fromBounded Nat.constant (Bounded.word8PaddedUpperHex w)

-- | Encode an ASCII char.
-- Precondition: Input must be an ASCII character. This is not checked.
ascii :: Char -> Builder
ascii c = fromBounded Nat.constant (Bounded.char c)

-- | Encode an UTF8 char. This only uses as much space as is required.
char :: Char -> Builder
char c = fromBounded Nat.constant (Bounded.char c)

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

shrinkMutableByteArray :: MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  primitive_ (Exts.shrinkMutableByteArray# arr sz)

-- | Requires exactly 8 bytes. Dump the octets of a 64-bit
-- word in a big-endian fashion.
word64BE :: Word64 -> Builder
word64BE w = fromBounded Nat.constant (Bounded.word64BE w)

-- | Requires exactly 4 bytes. Dump the octets of a 32-bit
-- word in a big-endian fashion.
word32BE :: Word32 -> Builder
word32BE w = fromBounded Nat.constant (Bounded.word32BE w)

-- | Requires exactly 2 bytes. Dump the octets of a 16-bit
-- word in a big-endian fashion.
word16BE :: Word16 -> Builder
word16BE w = fromBounded Nat.constant (Bounded.word16BE w)

word8 :: Word8 -> Builder
word8 w = fromBounded Nat.constant (Bounded.word8 w)

-- ShortText is already UTF-8 encoded. This is a no-op.
shortTextToByteArray :: ShortText -> ByteArray
shortTextToByteArray x = case TS.toShortByteString x of
  SBS a -> ByteArray a

indexChar8Array :: ByteArray -> Int -> Char
indexChar8Array (ByteArray b) (I# i) = C# (Exts.indexCharArray# b i)

c2w :: Char -> Word8
c2w = fromIntegral . ord
