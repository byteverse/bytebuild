{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Data.ByteArray.Builder
  ( -- * Bounded Primitives
    Builder
  , fromBounded
    -- * Evaluation
  , run
    -- * Materialized Byte Sequences
  , bytes
  , copy
  , insert
  , byteArray
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
    -- *** One
  , word8
    -- **** Big Endian
  , word64BE
  , word32BE
  , word16BE
  , int64BE
    -- **** Little Endian
  , word64LE
  , word32LE
  , word16LE
  , int64LE
    -- *** Many
  , word8Array
    -- **** Little Endian
  , word16ArrayLE
    -- ** Prefixing with Length
  , consLength32BE
  , consLength64BE
    -- * Encode Floating-Point Types
    -- ** Human-Readable
  , doubleDec
    -- * Control
  , flush
  ) where

import Control.Monad.Primitive (primitive_)
import Control.Monad.ST (ST,runST)
import Data.ByteArray.Builder.Unsafe (Builder(Builder))
import Data.ByteArray.Builder.Unsafe (Commits(Initial,Mutable,Immutable))
import Data.ByteArray.Builder.Unsafe (stringUtf8,cstring)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Bytes.Types (Bytes(Bytes))
import Data.Char (ord)
import Data.Int (Int64,Int32,Int16,Int8)
import Data.Primitive (ByteArray(..),MutableByteArray(..),PrimArray(..))
import Data.Text.Short (ShortText)
import Data.Word (Word64,Word32,Word16,Word8)
import GHC.Exts (Int(I#),Char(C#),Int#,State#,ByteArray#,(>=#))
import GHC.Exts (MutableByteArray#,(+#),(-#),(<#))
import GHC.ST (ST(ST))
import Data.Bytes.Chunks (Chunks(..))
import GHC.ByteOrder (ByteOrder(BigEndian,LittleEndian),targetByteOrder)

import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Types as Arithmetic
import qualified GHC.Exts as Exts
import qualified Data.Text.Short as TS
import qualified Data.Primitive as PM
import qualified Data.ByteArray.Builder.Bounded as Bounded
import qualified Data.ByteArray.Builder.Bounded.Unsafe as UnsafeBounded

-- | Run a builder.
run ::
     Int -- ^ Size of initial chunk (use 4080 if uncertain)
  -> Builder -- ^ Builder
  -> Chunks
run hint@(I# hint# ) (Builder f) = runST $ do
  MutableByteArray buf0 <- PM.newByteArray hint
  cs <- ST $ \s0 -> case f buf0 0# hint# Initial s0 of
    (# s1, bufX, offX, _, csX #) ->
      (# s1, Mutable bufX offX csX #)
  commitsOntoChunks ChunksNil cs

-- Internal. This freezes all the mutable byte arrays in-place,
-- so be careful. It also reverses the chunks since everything
-- is backwards.
commitsOntoChunks :: Chunks -> Commits s -> ST s Chunks
commitsOntoChunks !xs Initial = pure xs
commitsOntoChunks !xs (Immutable arr off len cs) =
  commitsOntoChunks (ChunksCons (Bytes (ByteArray arr) (I# off) (I# len)) xs) cs
commitsOntoChunks !xs (Mutable buf len cs) = case len of
  0# -> commitsOntoChunks xs cs
  _ -> do
    shrinkMutableByteArray (MutableByteArray buf) (I# len)
    arr <- PM.unsafeFreezeByteArray (MutableByteArray buf)
    commitsOntoChunks (ChunksCons (Bytes arr 0 (I# len)) xs) cs

-- | Convert a bounded builder to an unbounded one. If the size
-- is a constant, use @Arithmetic.Nat.constant@ as the first argument
-- to let GHC conjure up this value for you.
fromBounded ::
     Arithmetic.Nat n
  -> Bounded.Builder n
  -> Builder
{-# inline fromBounded #-}
fromBounded n (UnsafeBounded.Builder f) = Builder $ \buf0 off0 len0 cs0 s0 ->
  let !(I# req) = Nat.demote n
      !(# s1, buf1, off1, len1, cs1 #) = case len0 >=# req of
        1# -> (# s0, buf0, off0, len0, cs0 #)
        _ -> let !(I# lenX) = max 4080 (I# req) in
          case Exts.newByteArray# lenX s0 of
            (# sX, bufX #) ->
              (# sX, bufX, 0#, lenX, Mutable buf0 off0 cs0 #)
   in case f buf1 off1 s1 of
        (# s2, off2 #) -> (# s2, buf1, off2, len1 -# (off2 -# off1), cs1 #)

-- This is a micro-optimization that uses an equality check instead
-- of an inequality check when the required number of bytes is one.
-- Use this instead of fromBounded (where possible) leads to marginally
-- better results in benchmarks.
fromBoundedOne ::
     Bounded.Builder 1
  -> Builder
{-# inline fromBoundedOne #-}
fromBoundedOne (UnsafeBounded.Builder f) = Builder $ \buf0 off0 len0 cs0 s0 ->
  let !(# s1, buf1, off1, len1, cs1 #) = case len0 of
        0# -> case Exts.newByteArray# 4080# s0 of
          (# sX, bufX #) ->
            (# sX, bufX, 0#, 4080#, Mutable buf0 off0 cs0 #)
        _ -> (# s0, buf0, off0, len0, cs0 #)
   in case f buf1 off1 s1 of
        (# s2, off2 #) -> (# s2, buf1, off2, len1 -# (off2 -# off1), cs1 #)

-- | Create a builder from an unsliced byte sequence.
byteArray :: ByteArray -> Builder
byteArray a = bytes (Bytes a 0 (PM.sizeofByteArray a))

-- | Create a builder from a sliced byte sequence. The variants
-- 'copy' and 'insert' provide more control over whether or not
-- the byte sequence is copied or aliased. This function is preferred
-- when the user does not know the size of the byte sequence.
bytes :: Bytes -> Builder
bytes (Bytes (ByteArray src# ) (I# soff# ) (I# slen# )) = Builder
  -- There are three cases to consider: (1) there is not enough
  -- space and (1a) the chunk is not small or (1b) the chunk is
  -- small; (2) There is enough space for a copy.
  (\buf0 off0 len0 cs0 s0 -> case len0 <# slen# of
    1# -> case slen# >=# 256# of
      1# -> case Exts.newByteArray# 0# s0 of
        (# s1, buf1 #) -> (# s1, buf1, 0#, 0#, Immutable src# soff# slen# (Mutable buf0 off0 cs0) #)
      _ -> case Exts.newByteArray# 4080# s0 of
        (# s1, buf1 #) -> case Exts.copyByteArray# src# soff# buf1 0# slen# s1 of
          s2 -> (# s2, buf1, slen#, 4080# -# slen#, Mutable buf0 off0 cs0 #)
    _ -> let !s1 = Exts.copyByteArray# src# soff# buf0 off0 slen# s0 in
      (# s1, buf0, off0 +# slen#, len0 -# slen#, cs0 #)
  )

-- | Create a builder from a byte sequence. This always results in a
-- call to @memcpy@. This is beneficial when the byte sequence is
-- known to be small (less than 256 bytes).
copy :: Bytes -> Builder
copy (Bytes (ByteArray src# ) (I# soff# ) (I# slen# )) = Builder
  (\buf0 off0 len0 cs0 s0 -> case len0 <# slen# of
    1# -> case Exts.newByteArray# newSz s0 of
        (# s1, buf1 #) -> case Exts.copyByteArray# src# soff# buf1 0# slen# s1 of
          s2 -> (# s2, buf1, slen#, newSz -# slen#, Mutable buf0 off0 cs0 #)
    _ -> let !s1 = Exts.copyByteArray# src# soff# buf0 off0 slen# s0 in
      (# s1, buf0, off0 +# slen#, len0 -# slen#, cs0 #)
  )
  where
  !(I# newSz) = max (I# slen#) 4080

-- | Create a builder from a byte sequence. This never calls @memcpy@.
-- Instead, it pushes a chunk that references the argument byte sequence.
-- This wastes the remaining space in the active chunk, so it may adversely
-- affect performance if used carelessly. See 'flush' for a way to mitigate
-- this problem. This functions is most beneficial when the byte sequence
-- is known to be large (more than 8192 bytes).
insert :: Bytes -> Builder
insert (Bytes (ByteArray src# ) (I# soff# ) (I# slen# )) = Builder
  (\buf0 off0 _ cs0 s0 -> case Exts.newByteArray# 0# s0 of
    (# s1, buf1 #) ->
      (# s1, buf1, 0#, 0#, Immutable src# soff# slen# (Mutable buf0 off0 cs0) #)
  )

-- | Create a builder from a slice of an array of 'Word8'. There is the same
-- as 'bytes' but is provided as a convenience for users working with different
-- types.
word8Array :: PrimArray Word8 -> Int -> Int -> Builder
word8Array (PrimArray arr) off len = bytes (Bytes (ByteArray arr) off len)

word16ArrayLE :: PrimArray Word16 -> Int -> Int -> Builder
word16ArrayLE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  LittleEndian -> bytes (Bytes (ByteArray arr) (soff0 * 2) (slen0 * 2))
  BigEndian -> fromFunction (slen0 * 2) (go soff0 (soff0 + slen0))
  where
  go :: Int -> Int -> MutableByteArray s -> Int -> ST s Int
  go !soff !send !dst !doff = if soff < send
    then do
      doff' <- UnsafeBounded.pasteST
        (Bounded.word16LE (PM.indexPrimArray src soff))
        dst doff
      go (soff + 1) send dst doff'
    else pure doff

-- Internal function. Precondition, the referenced slice of the
-- byte sequence is UTF-8 encoded text.
slicedUtf8TextJson :: ByteArray# -> Int# -> Int# -> Builder
{-# inline slicedUtf8TextJson #-}
slicedUtf8TextJson !src# !soff0# !slen0# = fromFunction reqLen $ \dst doff0 -> do
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
  pure (doffRes + 1)
  where
  slen0 = I# slen0#
  reqLen = (2 * slen0) + 2

-- | Constructor for 'Builder' that works on a function with lifted
-- arguments instead of unlifted ones. This is just as unsafe as the
-- actual constructor.
fromFunction :: Int -> (forall s. MutableByteArray s -> Int -> ST s Int) -> Builder
fromFunction (I# req) f = Builder $ \buf0 off0 len0 cs0 s0 ->
  let !(# s1, buf1, off1, len1, cs1 #) = case len0 >=# req of
        1# -> (# s0, buf0, off0, len0, cs0 #)
        _ -> let !(I# lenX) = max 4080 (I# req) in
          case Exts.newByteArray# lenX s0 of
            (# sX, bufX #) ->
              (# sX, bufX, 0#, lenX, Mutable buf0 off0 cs0 #)
   in case unST (f (MutableByteArray buf1) (I# off1)) s1 of
        (# s2, I# off2 #) -> (# s2, buf1, off2, len1 -# (off2 -# off1), cs1 #)

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
ascii c = fromBoundedOne (Bounded.ascii c)

-- | Encode an UTF8 char. This only uses as much space as is required.
char :: Char -> Builder
char c = fromBounded Nat.constant (Bounded.char c)

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

shrinkMutableByteArray :: MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  primitive_ (Exts.shrinkMutableByteArray# arr sz)

-- | Requires exactly 8 bytes. Dump the octets of a 64-bit
-- signed integer in a little-endian fashion.
int64LE :: Int64 -> Builder
int64LE w = fromBounded Nat.constant (Bounded.int64LE w)

-- | Requires exactly 8 bytes. Dump the octets of a 64-bit
-- signed integer in a big-endian fashion.
int64BE :: Int64 -> Builder
int64BE w = fromBounded Nat.constant (Bounded.int64BE w)

-- | Requires exactly 8 bytes. Dump the octets of a 64-bit
-- word in a little-endian fashion.
word64LE :: Word64 -> Builder
word64LE w = fromBounded Nat.constant (Bounded.word64LE w)

-- | Requires exactly 4 bytes. Dump the octets of a 32-bit
-- word in a little-endian fashion.
word32LE :: Word32 -> Builder
word32LE w = fromBounded Nat.constant (Bounded.word32LE w)

-- | Requires exactly 2 bytes. Dump the octets of a 16-bit
-- word in a little-endian fashion.
word16LE :: Word16 -> Builder
word16LE w = fromBounded Nat.constant (Bounded.word16LE w)

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

-- | Requires exactly 1 byte.
word8 :: Word8 -> Builder
word8 w = fromBoundedOne (Bounded.word8 w)

-- | Prefix a builder with its size in bytes. This size is
-- presented as a big-endian 32-bit word. The need to prefix
-- a builder with its length shows up a numbers of wire protocols
-- including those of PostgreSQL and Apache Kafka. Note the
-- equivalence:
-- 
-- > forall (n :: Int) (x :: Builder).
-- >   let sz = sizeofByteArray (run n (consLength32BE x))
-- >   consLength32BE x === word32BE (fromIntegral sz) <> x
--
-- However, using 'consLength32BE' is much more efficient here
-- since it only materializes the 'ByteArray' once.
consLength32BE :: Builder -> Builder
consLength32BE (Builder f) = Builder $ \buf0 off0 len0 cs0 s0 ->
  let !(# s1, buf1, off1, len1, cs1 #) = case len0 >=# 4# of
        1# -> (# s0, buf0, off0, len0, cs0 #)
        _ -> case Exts.newByteArray# 4080# s0 of
          (# sX, bufX #) ->
            (# sX, bufX, 0#, 4080#, Mutable buf0 off0 cs0 #)
   in case f buf1 (off1 +# 4# ) (len1 -# 4# ) cs1 s1 of
        (# s2, buf2, off2, len2, cs2 #) ->
          let !dist = case Exts.sameMutableByteArray# buf1 buf2 of
                1# -> off2 -# off1
                _ -> commitDistance buf1 off2 cs2 -# off1
              ST g = UnsafeBounded.pasteST
                (Bounded.word32BE (fromIntegral (I# (dist -# 4# ))))
                (MutableByteArray buf1)
                (I# off1)
           in case g s2 of
                (# s3, _ #) -> (# s3, buf2, off2, len2, cs2 #)


-- | Prefix a builder with its size in bytes. This size is
-- presented as a big-endian 64-bit word. See 'consLength32BE'.
consLength64BE :: Builder -> Builder
consLength64BE (Builder f) = Builder $ \buf0 off0 len0 cs0 s0 ->
  let !(# s1, buf1, off1, len1, cs1 #) = case len0 >=# 8# of
        1# -> (# s0, buf0, off0, len0, cs0 #)
        _ -> case Exts.newByteArray# 4080# s0 of
          (# sX, bufX #) ->
            (# sX, bufX, 0#, 4080#, Mutable buf0 off0 cs0 #)
   in case f buf1 (off1 +# 8# ) (len1 -# 8# ) cs1 s1 of
        (# s2, buf2, off2, len2, cs2 #) ->
          let !dist = case Exts.sameMutableByteArray# buf1 buf2 of
                1# -> off2 -# off1
                _ -> commitDistance buf1 off2 cs2 -# off1
              ST g = UnsafeBounded.pasteST
                (Bounded.word64BE (fromIntegral (I# (dist -# 8# ))))
                (MutableByteArray buf1)
                (I# off1)
           in case g s2 of
                (# s3, _ #) -> (# s3, buf2, off2, len2, cs2 #)

commitDistance :: MutableByteArray# s -> Int# -> Commits s -> Int#
commitDistance _ !_ Initial = error "chunkDistance: chunk not found"
commitDistance target !n (Immutable _ _ len cs) =
  commitDistance target (n +# len) cs
commitDistance target !n (Mutable buf len cs) =
  case Exts.sameMutableByteArray# target buf of
    1# -> n +# len
    _ -> commitDistance target (n +# len) cs

-- | Push the buffer currently being filled onto the chunk list,
-- allocating a new active buffer of the requested size. This is
-- helpful when a small builder is sandwhiched between two large
-- zero-copy builders:
--
-- > insert bigA <> flush 1 <> word8 0x42 <> insert bigB
--
-- Without @flush 1@, @word8 0x42@ would see the zero-byte active
-- buffer that 'insert' returned, decide that it needed more space,
-- and allocate a 4080-byte buffer to which only a single byte
-- would be written.
flush :: Int -> Builder
flush !reqSz = Builder $ \buf0 off0 _ cs0 s0 ->
  case Exts.newByteArray# sz# s0 of
    (# sX, bufX #) ->
      (# sX, bufX, 0#, sz#, Mutable buf0 off0 cs0 #)
  where
  !(I# sz# ) = max reqSz 0

-- ShortText is already UTF-8 encoded. This is a no-op.
shortTextToByteArray :: ShortText -> ByteArray
shortTextToByteArray x = case TS.toShortByteString x of
  SBS a -> ByteArray a

indexChar8Array :: ByteArray -> Int -> Char
indexChar8Array (ByteArray b) (I# i) = C# (Exts.indexCharArray# b i)

c2w :: Char -> Word8
c2w = fromIntegral . ord
