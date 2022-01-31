{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NumericUnderscores #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Bytes.Builder
  ( -- * Bounded Primitives
    Builder
  , fromBounded
    -- * Evaluation
  , run
  , runOnto
  , reversedOnto
  , putMany
  , putManyConsLength
    -- * Materialized Byte Sequences
  , bytes
  , copy
  , copy2
  , insert
  , byteArray
  , shortByteString
  , shortTextUtf8
  , shortTextJsonString
  , cstring
  , cstring#
  , cstringLen
  , stringUtf8
    -- * Byte Sequence Encodings
  , sevenEightRight
  , sevenEightSmile
    -- * Encode Integral Types
    -- ** Human-Readable
  , word64Dec
  , word32Dec
  , word16Dec
  , word8Dec
  , wordDec
  , naturalDec
  , int64Dec
  , int32Dec
  , int16Dec
  , int8Dec
  , intDec
  , integerDec
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
  , ascii2
  , ascii3
  , ascii4
  , ascii5
  , ascii6
  , ascii7
  , ascii8
  , char
    -- ** Machine-Readable
    -- *** One
  , word8
    -- **** Big Endian
  , word256BE
  , word128BE
  , word64BE
  , word32BE
  , word16BE
  , int64BE
  , int32BE
  , int16BE
    -- **** Little Endian
  , word256LE
  , word128LE
  , word64LE
  , word32LE
  , word16LE
  , int64LE
  , int32LE
  , int16LE
    -- **** LEB128
  , intLEB128
  , int32LEB128
  , int64LEB128
  , wordLEB128
  , word32LEB128
  , word64LEB128
    -- *** Many
  , word8Array
    -- **** Big Endian
  , word16ArrayBE
  , word32ArrayBE
  , word64ArrayBE
  , word128ArrayBE
  , word256ArrayBE
  , int64ArrayBE
  , int32ArrayBE
  , int16ArrayBE
    -- **** Little Endian
  , word16ArrayLE
  , word32ArrayLE
  , word64ArrayLE
  , word128ArrayLE
  , word256ArrayLE
  , int64ArrayLE
  , int32ArrayLE
  , int16ArrayLE
    -- ** Prefixing with Length
  , consLength
  , consLength32LE
  , consLength32BE
  , consLength64BE
    -- * Encode Floating-Point Types
    -- ** Human-Readable
  , doubleDec
    -- * Replication
  , replicate
    -- * Control
  , flush
  ) where

import Prelude hiding (replicate)

import Control.Exception (SomeException,toException)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.ST (ST,runST)
import Data.Bits ((.&.),(.|.),unsafeShiftL,unsafeShiftR)
import Data.Bytes.Builder.Unsafe (addCommitsLength,copyReverseCommits)
import Data.Bytes.Builder.Unsafe (Builder(Builder),commitDistance1)
import Data.Bytes.Builder.Unsafe (BuilderState(BuilderState),pasteIO)
import Data.Bytes.Builder.Unsafe (Commits(Initial,Mutable,Immutable))
import Data.Bytes.Builder.Unsafe (commitsOntoChunks)
import Data.Bytes.Builder.Unsafe (reverseCommitsOntoChunks)
import Data.Bytes.Builder.Unsafe (stringUtf8,cstring,fromEffect)
import Data.Bytes.Chunks (Chunks(ChunksNil))
import Data.Bytes.Types (Bytes(Bytes),MutableBytes(MutableBytes))
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Char (ord)
import Data.Foldable (foldlM)
import Data.Int (Int64,Int32,Int16,Int8)
import Data.Primitive (ByteArray(..),MutableByteArray(..),PrimArray(..))
import Data.Text.Short (ShortText)
import Data.WideWord (Word128,Word256)
import Data.Word (Word64,Word32,Word16,Word8)
import Data.Word.Zigzag (toZigzagNative,toZigzag32,toZigzag64)
import Foreign.C.String (CStringLen)
import GHC.ByteOrder (ByteOrder(BigEndian,LittleEndian),targetByteOrder)
import GHC.Exts (Addr#,(*#))
import GHC.Exts (Int(I#),Char(C#),Int#,State#,ByteArray#,(>=#))
import GHC.Exts (RealWorld,(+#),(-#),(<#))
import GHC.Integer.Logarithms.Compat (integerLog2#)
import GHC.IO (IO(IO),stToIO)
import GHC.Natural (naturalFromInteger,naturalToInteger)
import GHC.ST (ST(ST))
import GHC.Word (Word(W#),Word8(W8#))
import Numeric.Natural (Natural)

import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Types as Arithmetic
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder.Bounded as Bounded
import qualified Data.Bytes.Builder.Bounded.Unsafe as UnsafeBounded
import qualified Data.Primitive as PM
import qualified Data.Text.Short as TS
import qualified GHC.Exts as Exts

-- | Run a builder.
run ::
     Int -- ^ Size of initial chunk (use 4080 if uncertain)
  -> Builder -- ^ Builder
  -> Chunks
run !hint bldr = runOnto hint bldr ChunksNil

-- | Run a builder. The resulting chunks are consed onto the
-- beginning of an existing sequence of chunks.
runOnto ::
     Int -- ^ Size of initial chunk (use 4080 if uncertain)
  -> Builder -- ^ Builder
  -> Chunks
  -> Chunks
runOnto hint@(I# hint# ) (Builder f) cs0 = runST $ do
  MutableByteArray buf0 <- PM.newByteArray hint
  cs <- ST $ \s0 -> case f buf0 0# hint# Initial s0 of
    (# s1, bufX, offX, _, csX #) ->
      (# s1, Mutable bufX offX csX #)
  reverseCommitsOntoChunks cs0 cs

-- | Variant of 'runOnto' that conses the additional chunks
-- in reverse order. 
reversedOnto :: 
     Int -- ^ Size of initial chunk (use 4080 if uncertain)
  -> Builder -- ^ Builder
  -> Chunks
  -> Chunks
reversedOnto hint@(I# hint# ) (Builder f) cs0 = runST $ do
  MutableByteArray buf0 <- PM.newByteArray hint
  cs <- ST $ \s0 -> case f buf0 0# hint# Initial s0 of
    (# s1, bufX, offX, _, csX #) ->
      (# s1, Mutable bufX offX csX #)
  commitsOntoChunks cs0 cs

-- | Run a builder against lots of elements. This fills the same
-- underlying buffer over and over again. Do not let the argument to
-- the callback escape from the callback (i.e. do not write it to an
-- @IORef@). Also, do not @unsafeFreezeByteArray@ any of the mutable
-- byte arrays in the callback. The intent is that the callback will
-- write the buffer out.
putMany :: Foldable f
  => Int -- ^ Size of shared chunk (use 8176 if uncertain)
  -> (a -> Builder) -- ^ Value builder
  -> f a -- ^ Collection of values
  -> (MutableBytes RealWorld -> IO b) -- ^ Consume chunks.
  -> IO ()
{-# inline putMany #-}
putMany hint0 g xs cb = do
  MutableByteArray buf0 <- PM.newByteArray hint
  BuilderState bufZ offZ _ cmtsZ <- foldlM
    (\st0 a -> do
      st1@(BuilderState buf off _ cmts) <- pasteIO (g a) st0
      case cmts of
        Initial -> if I# off < threshold
          then pure st1
          else do
            _ <- cb (MutableBytes (MutableByteArray buf) 0 (I# off))
            pure (BuilderState buf0 0# hint# Initial)
        _ -> do
          let total = addCommitsLength (I# off) cmts
              doff0 = total - I# off
          large <- PM.newByteArray total
          stToIO (PM.copyMutableByteArray large doff0 (MutableByteArray buf) 0 (I# off))
          r <- stToIO (copyReverseCommits large doff0 cmts)
          case r of
            0 -> do
              _ <- cb (MutableBytes large 0 total)
              pure (BuilderState buf0 0# hint# Initial)
            _ -> IO (\s0 -> Exts.raiseIO# putManyError s0)
    ) (BuilderState buf0 0# hint# Initial) xs
  _ <- case cmtsZ of
    Initial -> cb (MutableBytes (MutableByteArray bufZ) 0 (I# offZ))
    _ -> IO (\s0 -> Exts.raiseIO# putManyError s0)
  pure ()
  where
  !hint@(I# hint#) = max hint0 8
  !threshold = div (hint * 3) 4

putManyError :: SomeException
{-# noinline putManyError #-}
putManyError = toException
  (userError "bytebuild: putMany implementation error")

-- | Variant of 'putMany' that prefixes each pushed array of chunks
-- with the number of bytes that the chunks in each batch required.
-- (This excludes the bytes required to encode the length itself.)
-- This is useful for chunked HTTP encoding.
putManyConsLength :: (Foldable f, MonadIO m)
  => Arithmetic.Nat n -- ^ Number of bytes used by the serialization of the length
  -> (Int -> Bounded.Builder n) -- ^ Length serialization function
  -> Int -- ^ Size of shared chunk (use 8176 if uncertain)
  -> (a -> Builder) -- ^ Value builder
  -> f a -- ^ Collection of values
  -> (MutableBytes RealWorld -> m b) -- ^ Consume chunks.
  -> m ()
{-# inline putManyConsLength #-}
putManyConsLength n buildSize hint g xs cb = do
  let !(I# n# ) = Nat.demote n
  let !(I# actual# ) = max hint (I# n# )
  let !threshold = div (I# actual# * 3) 4
  MutableByteArray buf0 <- liftIO (PM.newByteArray (I# actual# ))
  BuilderState bufZ offZ _ cmtsZ <- foldlM
    (\st0 a -> do
      st1@(BuilderState buf off _ cmts) <- liftIO (pasteIO (g a) st0)
      case cmts of
        Initial -> if I# off < threshold
          then pure st1
          else do
            let !dist = off -# n#
            _ <- liftIO $ stToIO $ UnsafeBounded.pasteST
              (buildSize (fromIntegral (I# dist)))
              (MutableByteArray buf0) 0
            _ <- cb (MutableBytes (MutableByteArray buf) 0 (I# off))
            pure (BuilderState buf0 n# (actual# -# n# ) Initial)
        _ -> do
          let !dist = commitDistance1 buf0 n# buf off cmts
          _ <- liftIO $ stToIO $ UnsafeBounded.pasteST
            (buildSize (fromIntegral (I# dist)))
            (MutableByteArray buf0) 0
          let total = addCommitsLength (I# off) cmts
              doff0 = total - I# off
          large <- liftIO (PM.newByteArray total)
          liftIO (stToIO (PM.copyMutableByteArray large doff0 (MutableByteArray buf) 0 (I# off)))
          r <- liftIO (stToIO (copyReverseCommits large doff0 cmts))
          case r of
            0 -> do
              _ <- cb (MutableBytes large 0 total)
              pure (BuilderState buf0 n# (actual# -# n# ) Initial)
            _ -> liftIO (IO (\s0 -> Exts.raiseIO# putManyError s0))
    ) (BuilderState buf0 n# (actual# -# n# ) Initial) xs
  _ <- case cmtsZ of
    Initial -> do
      let !distZ = offZ -# n#
      _ <- liftIO $ stToIO $ UnsafeBounded.pasteST
        (buildSize (fromIntegral (I# distZ)))
        (MutableByteArray buf0)
        0
      cb (MutableBytes (MutableByteArray bufZ) 0 (I# offZ))
    _ -> liftIO (IO (\s0 -> Exts.raiseIO# putManyError s0))
  pure ()

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
        (# s2, _ #) -> (# s2, buf1, off1 +# 1#, len1 -# 1#, cs1 #)

-- | Create a builder from an unsliced byte sequence. Implemented with 'bytes'.
byteArray :: ByteArray -> Builder
byteArray a = bytes (Bytes a 0 (PM.sizeofByteArray a))

-- | Create a builder from a short bytestring. Implemented with 'bytes'.
shortByteString :: ShortByteString -> Builder
shortByteString (SBS x) = bytes (Bytes a 0 (PM.sizeofByteArray a))
  where a = ByteArray x

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

cstring# :: Addr# -> Builder
{-# inline cstring# #-}
cstring# x = cstring (Exts.Ptr x)

-- | Create a builder from a C string with explicit length. The builder
-- must be executed before the C string is freed.
cstringLen :: CStringLen -> Builder
cstringLen (Exts.Ptr src#, I# slen# ) = Builder
  (\buf0 off0 len0 cs0 s0 -> case len0 <# slen# of
    1# -> case Exts.newByteArray# newSz s0 of
        (# s1, buf1 #) -> case Exts.copyAddrToByteArray# src# buf1 0# slen# s1 of
          s2 -> (# s2, buf1, slen#, newSz -# slen#, Mutable buf0 off0 cs0 #)
    _ -> let !s1 = Exts.copyAddrToByteArray# src# buf0 off0 slen# s0 in
      (# s1, buf0, off0 +# slen#, len0 -# slen#, cs0 #)
  )
  where
  !(I# newSz) = max (I# slen#) 4080

-- | Encode seven bytes into eight so that the encoded form is eight-bit clean.
-- Specifically segment the input bytes inot 7-bit groups (lowest-to-highest
-- index byte, most-to-least significant bit within a byte), pads the last group
-- with trailing zeros, and forms octects by prepending a zero to each group.
--
-- The name was chosen because this pads the input bits with zeros on the right,
-- and also because this was likely the originally-indended behavior of the
-- SMILE standard (see 'sevenEightSmile'). Right padding the input bits to a
-- multiple of seven, as in this variant, is consistent with base64 encodings
-- (which encodes 3 bytes in 4) and base85 (which encodes 4 bytes in 5).
sevenEightRight :: Bytes -> Builder
sevenEightRight bs0 = case toWord 0 0 bs0 of
  (0, _) -> mempty
  (len, w) -> go (len * 8) w <> sevenEightSmile (Bytes.unsafeDrop len bs0)
  where
  go :: Int -> Word64 -> Builder
  go !nBits !_ | nBits <= 0 = mempty
  go !nBits !w =
    let octet = (fromIntegral $ unsafeShiftR w (8*7+1)) .&. 0x7f
     in word8 octet <> go (nBits - 7) (unsafeShiftL w 7)
  toWord :: Int -> Word64 -> Bytes -> (Int, Word64)
  toWord !i !acc !bs
    | Bytes.length bs == 0 = (i, acc)
    | otherwise =
        let b = fromIntegral @Word8 @Word64 $ Bytes.unsafeIndex bs 0
            acc' = acc .|. unsafeShiftL b (fromIntegral $ 8 * (7 - i))
         in if i < 7
            then toWord (i + 1) acc' (Bytes.unsafeDrop 1 bs)
            else (i, acc)

-- | Encode seven bytes into eight so that the encoded form is eight-bit clean.
-- Specifically segment the input bytes inot 7-bit groups (lowest-to-highest
-- index byte, most-to-least significant bit within a byte), then pad each group
-- with zeros on the left until each group is an octet.
--
-- The name was chosen because this is the implementation that is used (probably
-- unintentionally) in the reference SMILE implementation, and so is expected tp
-- be accepted by existing SMILE consumers.
sevenEightSmile :: Bytes -> Builder
sevenEightSmile bs0 = case toWord 0 0 bs0 of
  (0, _) -> mempty
  (len, w) -> go (len * 8) w <> sevenEightSmile (Bytes.unsafeDrop len bs0)
  where
  go :: Int -> Word64 -> Builder
  go !nBits !w
    | nBits == 0 = mempty
    | nBits < 7 = go 7 (unsafeShiftR w (7 - nBits))
  go !nBits !w =
    let octet = (fromIntegral $ unsafeShiftR w (8*7+1)) .&. 0x7f
     in word8 octet <> go (nBits - 7) (unsafeShiftL w 7)
  toWord :: Int -> Word64 -> Bytes -> (Int, Word64)
  toWord !i !acc !bs
    | Bytes.length bs == 0 = (i, acc)
    | otherwise =
        let b = fromIntegral @Word8 @Word64 $ Bytes.unsafeIndex bs 0
            acc' = acc .|. unsafeShiftL b (fromIntegral $ 8 * (7 - i))
         in if i < 7
            then toWord (i + 1) acc' (Bytes.unsafeDrop 1 bs)
            else (i, acc)

-- | Create a builder from two byte sequences. This always results in two
-- calls to @memcpy@. This is beneficial when the byte sequences are
-- known to be small (less than 256 bytes).
copy2 :: Bytes -> Bytes -> Builder
copy2 (Bytes (ByteArray srcA# ) (I# soffA# ) (I# slenA# ))
      (Bytes (ByteArray srcB# ) (I# soffB# ) (I# slenB# )) = Builder
  (\buf0 off0 len0 cs0 s0 -> case len0 <# slen# of
    1# -> case Exts.newByteArray# newSz s0 of
        (# s1, buf1 #) -> case Exts.copyByteArray# srcA# soffA# buf1 0# slenA# s1 of
          s2 -> case Exts.copyByteArray# srcB# soffB# buf1 slenA# slenB# s2 of
            s3 -> (# s3, buf1, slen#, newSz -# slen#, Mutable buf0 off0 cs0 #)
    _ -> let !s1 = Exts.copyByteArray# srcA# soffA# buf0 off0 slenA# s0
             !s2 = Exts.copyByteArray# srcB# soffB# buf0 (off0 +# slenA# ) slenB# s1 in
      (# s2, buf0, off0 +# slen#, len0 -# slen#, cs0 #)
  )
  where
  !slen# = slenA# +# slenB#
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

int64ArrayLE :: PrimArray Int64 -> Int -> Int -> Builder
int64ArrayLE (PrimArray x) = word64ArrayLE (PrimArray x)

int64ArrayBE :: PrimArray Int64 -> Int -> Int -> Builder
int64ArrayBE (PrimArray x) = word64ArrayBE (PrimArray x)

int32ArrayLE :: PrimArray Int32 -> Int -> Int -> Builder
int32ArrayLE (PrimArray x) = word32ArrayLE (PrimArray x)

int32ArrayBE :: PrimArray Int32 -> Int -> Int -> Builder
int32ArrayBE (PrimArray x) = word32ArrayBE (PrimArray x)

int16ArrayLE :: PrimArray Int16 -> Int -> Int -> Builder
int16ArrayLE (PrimArray x) = word16ArrayLE (PrimArray x)

int16ArrayBE :: PrimArray Int16 -> Int -> Int -> Builder
int16ArrayBE (PrimArray x) = word16ArrayBE (PrimArray x)

word128ArrayLE :: PrimArray Word128 -> Int -> Int -> Builder
word128ArrayLE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  LittleEndian -> bytes (Bytes (ByteArray arr) (soff0 * 16) (slen0 * 16))
  BigEndian -> word128ArraySwap src soff0 slen0

word128ArrayBE :: PrimArray Word128 -> Int -> Int -> Builder
word128ArrayBE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  BigEndian -> bytes (Bytes (ByteArray arr) (soff0 * 16) (slen0 * 16))
  LittleEndian -> word128ArraySwap src soff0 slen0

word256ArrayLE :: PrimArray Word256 -> Int -> Int -> Builder
word256ArrayLE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  LittleEndian -> bytes (Bytes (ByteArray arr) (soff0 * 32) (slen0 * 32))
  BigEndian -> word256ArraySwap src soff0 slen0

word256ArrayBE :: PrimArray Word256 -> Int -> Int -> Builder
word256ArrayBE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  BigEndian -> bytes (Bytes (ByteArray arr) (soff0 * 32) (slen0 * 32))
  LittleEndian -> word256ArraySwap src soff0 slen0

word64ArrayLE :: PrimArray Word64 -> Int -> Int -> Builder
word64ArrayLE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  LittleEndian -> bytes (Bytes (ByteArray arr) (soff0 * 8) (slen0 * 8))
  BigEndian -> word64ArraySwap src soff0 slen0

word64ArrayBE :: PrimArray Word64 -> Int -> Int -> Builder
word64ArrayBE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  BigEndian -> bytes (Bytes (ByteArray arr) (soff0 * 8) (slen0 * 8))
  LittleEndian -> word64ArraySwap src soff0 slen0

word32ArrayLE :: PrimArray Word32 -> Int -> Int -> Builder
word32ArrayLE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  LittleEndian -> bytes (Bytes (ByteArray arr) (soff0 * 4) (slen0 * 4))
  BigEndian -> word32ArraySwap src soff0 slen0

word32ArrayBE :: PrimArray Word32 -> Int -> Int -> Builder
word32ArrayBE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  BigEndian -> bytes (Bytes (ByteArray arr) (soff0 * 4) (slen0 * 4))
  LittleEndian -> word32ArraySwap src soff0 slen0

word16ArrayLE :: PrimArray Word16 -> Int -> Int -> Builder
word16ArrayLE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  LittleEndian -> bytes (Bytes (ByteArray arr) (soff0 * 2) (slen0 * 2))
  BigEndian -> word16ArraySwap src soff0 slen0

word16ArrayBE :: PrimArray Word16 -> Int -> Int -> Builder
word16ArrayBE src@(PrimArray arr) soff0 slen0 = case targetByteOrder of
  BigEndian -> bytes (Bytes (ByteArray arr) (soff0 * 2) (slen0 * 2))
  LittleEndian -> word16ArraySwap src soff0 slen0

word16ArraySwap :: PrimArray Word16 -> Int -> Int -> Builder
word16ArraySwap src soff0 slen0 =
  fromFunction (slen0 * 2) (go (soff0 * 2) ((soff0 + slen0) * 2))
  where
  go :: Int -> Int -> MutableByteArray s -> Int -> ST s Int
  go !soff !send !dst !doff = if soff < send
    then do
      let v0 = PM.indexPrimArray (asWord8s src) soff
          v1 = PM.indexPrimArray (asWord8s src) (soff + 1)
      PM.writeByteArray dst doff v1
      PM.writeByteArray dst (doff + 1) v0
      go (soff + 2) send dst (doff + 2)
    else pure doff

word32ArraySwap :: PrimArray Word32 -> Int -> Int -> Builder
word32ArraySwap src soff0 slen0 =
  fromFunction (slen0 * 4) (go (soff0 * 4) ((soff0 + slen0) * 4))
  where
  go :: Int -> Int -> MutableByteArray s -> Int -> ST s Int
  go !soff !send !dst !doff = if soff < send
    then do
      let v0 = PM.indexPrimArray (asWord8s src) soff
          v1 = PM.indexPrimArray (asWord8s src) (soff + 1)
          v2 = PM.indexPrimArray (asWord8s src) (soff + 2)
          v3 = PM.indexPrimArray (asWord8s src) (soff + 3)
      PM.writeByteArray dst doff v3
      PM.writeByteArray dst (doff + 1) v2
      PM.writeByteArray dst (doff + 2) v1
      PM.writeByteArray dst (doff + 3) v0
      go (soff + 4) send dst (doff + 4)
    else pure doff

word64ArraySwap :: PrimArray Word64 -> Int -> Int -> Builder
word64ArraySwap src soff0 slen0 =
  fromFunction (slen0 * 8) (go (soff0 * 8) ((soff0 + slen0) * 8))
  where
  go :: Int -> Int -> MutableByteArray s -> Int -> ST s Int
  go !soff !send !dst !doff = if soff < send
    then do
      let v0 = PM.indexPrimArray (asWord8s src) soff
          v1 = PM.indexPrimArray (asWord8s src) (soff + 1)
          v2 = PM.indexPrimArray (asWord8s src) (soff + 2)
          v3 = PM.indexPrimArray (asWord8s src) (soff + 3)
          v4 = PM.indexPrimArray (asWord8s src) (soff + 4)
          v5 = PM.indexPrimArray (asWord8s src) (soff + 5)
          v6 = PM.indexPrimArray (asWord8s src) (soff + 6)
          v7 = PM.indexPrimArray (asWord8s src) (soff + 7)
      PM.writeByteArray dst doff v7
      PM.writeByteArray dst (doff + 1) v6
      PM.writeByteArray dst (doff + 2) v5
      PM.writeByteArray dst (doff + 3) v4
      PM.writeByteArray dst (doff + 4) v3
      PM.writeByteArray dst (doff + 5) v2
      PM.writeByteArray dst (doff + 6) v1
      PM.writeByteArray dst (doff + 7) v0
      go (soff + 8) send dst (doff + 8)
    else pure doff

word128ArraySwap :: PrimArray Word128 -> Int -> Int -> Builder
word128ArraySwap src soff0 slen0 =
  fromFunction (slen0 * 16) (go (soff0 * 16) ((soff0 + slen0) * 16))
  where
  -- TODO: Perhaps we could put byteswapping functions to use
  -- rather than indexing tons of Word8s. This could be done
  -- both here and in the other swap functions. There are a
  -- decent number of tests for these array-swapping functions,
  -- which makes changing this less scary.
  go :: Int -> Int -> MutableByteArray s -> Int -> ST s Int
  go !soff !send !dst !doff = if soff < send
    then do
      let v0 = PM.indexPrimArray (asWord8s src) soff
          v1 = PM.indexPrimArray (asWord8s src) (soff + 1)
          v2 = PM.indexPrimArray (asWord8s src) (soff + 2)
          v3 = PM.indexPrimArray (asWord8s src) (soff + 3)
          v4 = PM.indexPrimArray (asWord8s src) (soff + 4)
          v5 = PM.indexPrimArray (asWord8s src) (soff + 5)
          v6 = PM.indexPrimArray (asWord8s src) (soff + 6)
          v7 = PM.indexPrimArray (asWord8s src) (soff + 7)
          v8 = PM.indexPrimArray (asWord8s src) (soff + 8)
          v9 = PM.indexPrimArray (asWord8s src) (soff + 9)
          v10 = PM.indexPrimArray (asWord8s src) (soff + 10)
          v11 = PM.indexPrimArray (asWord8s src) (soff + 11)
          v12 = PM.indexPrimArray (asWord8s src) (soff + 12)
          v13 = PM.indexPrimArray (asWord8s src) (soff + 13)
          v14 = PM.indexPrimArray (asWord8s src) (soff + 14)
          v15 = PM.indexPrimArray (asWord8s src) (soff + 15)
      PM.writeByteArray dst doff v15
      PM.writeByteArray dst (doff + 1) v14
      PM.writeByteArray dst (doff + 2) v13
      PM.writeByteArray dst (doff + 3) v12
      PM.writeByteArray dst (doff + 4) v11
      PM.writeByteArray dst (doff + 5) v10
      PM.writeByteArray dst (doff + 6) v9
      PM.writeByteArray dst (doff + 7) v8
      PM.writeByteArray dst (doff + 8) v7
      PM.writeByteArray dst (doff + 9) v6
      PM.writeByteArray dst (doff + 10) v5
      PM.writeByteArray dst (doff + 11) v4
      PM.writeByteArray dst (doff + 12) v3
      PM.writeByteArray dst (doff + 13) v2
      PM.writeByteArray dst (doff + 14) v1
      PM.writeByteArray dst (doff + 15) v0
      go (soff + 16) send dst (doff + 16)
    else pure doff

word256ArraySwap :: PrimArray Word256 -> Int -> Int -> Builder
word256ArraySwap src soff0 slen0 =
  fromFunction (slen0 * 32) (go (soff0 * 32) ((soff0 + slen0) * 32))
  where
  -- TODO: Perhaps we could put byteswapping functions to use
  -- rather than indexing tons of Word8s. This could be done
  -- both here and in the other swap functions. There are a
  -- decent number of tests for these array-swapping functions,
  -- which makes changing this less scary.
  go :: Int -> Int -> MutableByteArray s -> Int -> ST s Int
  go !soff !send !dst !doff = if soff < send
    then do
      let loop !i
            | i < 32 = do
              let v = PM.indexPrimArray (asWord8s src) (soff + i)
              PM.writeByteArray dst (doff + (31 - i)) v
              loop (i + 1)
            | otherwise = pure ()
      loop 0
      go (soff + 32) send dst (doff + 32)
    else pure doff

asWord8s :: PrimArray a -> PrimArray Word8
asWord8s (PrimArray x) = PrimArray x

-- Internal function. Precondition, the referenced slice of the
-- byte sequence is UTF-8 encoded text.
slicedUtf8TextJson :: ByteArray# -> Int# -> Int# -> Builder
{-# noinline slicedUtf8TextJson #-}
slicedUtf8TextJson !src# !soff0# !slen0# = fromFunction reqLen $ \dst doff0 -> do
  PM.writeByteArray dst doff0 (c2w '"')
  let go !soff !slen !doff = if slen > 0
        then case indexChar8Array (ByteArray src#) soff of
          '\\' -> write2 dst doff '\\' '\\' *> go (soff + 1) (slen - 1) (doff + 2)
          '\"' -> write2 dst doff '\\' '\"' *> go (soff + 1) (slen - 1) (doff + 2)
          c -> if c >= '\x20'
            then PM.writeByteArray dst doff (c2w c) *> go (soff + 1) (slen - 1) (doff + 1)
            else case c of
              '\n' -> write2 dst doff '\\' 'n' *> go (soff + 1) (slen - 1) (doff + 2)
              '\r' -> write2 dst doff '\\' 'r' *> go (soff + 1) (slen - 1) (doff + 2)
              '\t' -> write2 dst doff '\\' 't' *> go (soff + 1) (slen - 1) (doff + 2)
              _ -> do
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
  -- We multiply by 6 because, in the worst case, everything might be in the
  -- unprintable ASCII range. The plus 2 is for the quotes on the ends.
  reqLen = (6 * slen0) + 2

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
-- * @foo ==\> "foo"@ (no escape sequences)
-- * @\\_"_\/ ==\> "\\\\_\\"_\/"@ (escapes backslashes and quotes)
-- * @hello\<ESC\>world ==> "hello\\u001Bworld"@ (where @\<ESC\>@ is code point 0x1B)
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

-- | Encode two ASCII characters.
-- Precondition: Must be an ASCII characters. This is not checked.
ascii2 :: Char -> Char -> Builder
ascii2 a b = fromBounded Nat.constant (Bounded.ascii2 a b)

-- | Encode three ASCII characters.
-- Precondition: Must be an ASCII characters. This is not checked.
ascii3 :: Char -> Char -> Char -> Builder
ascii3 a b c = fromBounded Nat.constant (Bounded.ascii3 a b c)

-- | Encode four ASCII characters.
-- Precondition: Must be an ASCII characters. This is not checked.
ascii4 :: Char -> Char -> Char -> Char -> Builder
ascii4 a b c d = fromBounded Nat.constant (Bounded.ascii4 a b c d)

-- | Encode five ASCII characters.
-- Precondition: Must be an ASCII characters. This is not checked.
ascii5 :: Char -> Char -> Char -> Char -> Char -> Builder
ascii5 a b c d e = fromBounded Nat.constant (Bounded.ascii5 a b c d e)

-- | Encode six ASCII characters.
-- Precondition: Must be an ASCII characters. This is not checked.
ascii6 :: Char -> Char -> Char -> Char -> Char -> Char -> Builder
ascii6 a b c d e f = fromBounded Nat.constant (Bounded.ascii6 a b c d e f)

-- | Encode seven ASCII characters.
-- Precondition: Must be an ASCII characters. This is not checked.
ascii7 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Builder
ascii7 a b c d e f g = fromBounded Nat.constant (Bounded.ascii7 a b c d e f g)

-- | Encode eight ASCII characters.
-- Precondition: Must be an ASCII characters. This is not checked.
ascii8 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Builder
ascii8 a b c d e f g h = fromBounded Nat.constant (Bounded.ascii8 a b c d e f g h)

-- | Encode a UTF-8 char. This only uses as much space as is required.
char :: Char -> Builder
char c = fromBounded Nat.constant (Bounded.char c)

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

-- | Requires exactly 8 bytes. Dump the octets of a 64-bit
-- signed integer in a little-endian fashion.
int64LE :: Int64 -> Builder
int64LE w = fromBounded Nat.constant (Bounded.int64LE w)

-- | Requires exactly 4 bytes. Dump the octets of a 32-bit
-- signed integer in a little-endian fashion.
int32LE :: Int32 -> Builder
int32LE w = fromBounded Nat.constant (Bounded.int32LE w)

-- | Requires exactly 2 bytes. Dump the octets of a 16-bit
-- signed integer in a little-endian fashion.
int16LE :: Int16 -> Builder
int16LE w = fromBounded Nat.constant (Bounded.int16LE w)

-- | Requires exactly 8 bytes. Dump the octets of a 64-bit
-- signed integer in a big-endian fashion.
int64BE :: Int64 -> Builder
int64BE w = fromBounded Nat.constant (Bounded.int64BE w)

-- | Requires exactly 4 bytes. Dump the octets of a 32-bit
-- signed integer in a big-endian fashion.
int32BE :: Int32 -> Builder
int32BE w = fromBounded Nat.constant (Bounded.int32BE w)

-- | Requires exactly 2 bytes. Dump the octets of a 16-bit
-- signed integer in a big-endian fashion.
int16BE :: Int16 -> Builder
int16BE w = fromBounded Nat.constant (Bounded.int16BE w)

-- | Requires exactly 32 bytes. Dump the octets of a 256-bit
-- word in a little-endian fashion.
word256LE :: Word256 -> Builder
word256LE w = fromBounded Nat.constant (Bounded.word256LE w)

-- | Requires exactly 16 bytes. Dump the octets of a 128-bit
-- word in a little-endian fashion.
word128LE :: Word128 -> Builder
word128LE w = fromBounded Nat.constant (Bounded.word128LE w)

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

-- | Requires exactly 32 bytes. Dump the octets of a 256-bit
-- word in a big-endian fashion.
word256BE :: Word256 -> Builder
word256BE w = fromBounded Nat.constant (Bounded.word256BE w)

-- | Requires exactly 16 bytes. Dump the octets of a 128-bit
-- word in a big-endian fashion.
word128BE :: Word128 -> Builder
word128BE w = fromBounded Nat.constant (Bounded.word128BE w)

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

-- | Prefix a builder with the number of bytes that it requires.
consLength ::
     Arithmetic.Nat n -- ^ Number of bytes used by the serialization of the length
  -> (Int -> Bounded.Builder n) -- ^ Length serialization function
  -> Builder -- ^ Builder whose length is measured
  -> Builder
{-# inline consLength #-}
consLength !n buildSize (Builder f) = Builder $ \buf0 off0 len0 cs0 s0 ->
  -- There is actually a little bit of unsoundness here. If the number of
  -- bytes required to encode the length is greater than 4080, this will
  -- write outside the array, leading to a crash.
  let !(I# lenSz) = Nat.demote n
      !(# s1, buf1, off1, len1, cs1 #) = case len0 >=# lenSz of
        1# -> (# s0, buf0, off0, len0, cs0 #)
        _ -> case Exts.newByteArray# 4080# s0 of
          (# sX, bufX #) ->
            (# sX, bufX, 0#, 4080#, Mutable buf0 off0 cs0 #)
   in case f buf1 (off1 +# lenSz) (len1 -# lenSz) cs1 s1 of
        (# s2, buf2, off2, len2, cs2 #) ->
          let !dist = commitDistance1 buf1 (off1 +# lenSz) buf2 off2 cs2
              ST g = UnsafeBounded.pasteST
                (buildSize (fromIntegral (I# dist)))
                (MutableByteArray buf1)
                (I# off1)
           in case g s2 of
                (# s3, _ #) -> (# s3, buf2, off2, len2, cs2 #)

-- | Variant of 'consLength32BE' the encodes the length in
-- a little-endian fashion.
consLength32LE :: Builder -> Builder
consLength32LE = consLength Nat.constant (\x -> Bounded.word32LE (fromIntegral x))

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
consLength32BE = consLength Nat.constant (\x -> Bounded.word32BE (fromIntegral x))

-- | Prefix a builder with its size in bytes. This size is
-- presented as a big-endian 64-bit word. See 'consLength32BE'.
consLength64BE :: Builder -> Builder
consLength64BE = consLength Nat.constant (\x -> Bounded.word64BE (fromIntegral x))

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

-- | Encode a signed machine-sized integer with LEB-128. This uses
-- zig-zag encoding.
intLEB128 :: Int -> Builder
intLEB128 = wordLEB128 . toZigzagNative

-- | Encode a 32-bit signed integer with LEB-128. This uses zig-zag encoding.
int32LEB128 :: Int32 -> Builder
int32LEB128 = word32LEB128 . toZigzag32

-- | Encode a 64-bit signed integer with LEB-128. This uses zig-zag encoding.
int64LEB128 :: Int64 -> Builder
int64LEB128 = word64LEB128 . toZigzag64

-- | Encode a machine-sized word with LEB-128.
wordLEB128 :: Word -> Builder
wordLEB128 w = fromBounded Nat.constant (Bounded.wordLEB128 w)

-- | Encode a 32-bit word with LEB-128.
word32LEB128 :: Word32 -> Builder
word32LEB128 w = fromBounded Nat.constant (Bounded.word32LEB128 w)

-- | Encode a 64-bit word with LEB-128.
word64LEB128 :: Word64 -> Builder
word64LEB128 w = fromBounded Nat.constant (Bounded.word64LEB128 w)

-- | Encode a signed arbitrary-precision integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
-- Negative numbers are preceded by a minus sign. Positive numbers
-- are not preceded by anything.
integerDec :: Integer -> Builder
integerDec !i
  | i < 0 = ascii '-' <> naturalDec (naturalFromInteger (negate i))
  | otherwise = naturalDec (naturalFromInteger i)

-- | Encodes an unsigned arbitrary-precision integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
naturalDec :: Natural -> Builder
naturalDec !n0 = fromEffect
  (I# (11# +# (3# *# integerLog2# (naturalToInteger n0))))
  (\marr off -> case n0 of
    0 -> do
      PM.writeByteArray marr off (0x30 :: Word8)
      pure (off + 1)
    _ -> go n0 marr off off
  )
  where
  go :: forall s. Natural -> MutableByteArray s -> Int -> Int -> ST s Int
  go !n !buf !off0 !off = case quotRem n 1_000_000_000 of
    (q,r) -> case q of
      0 -> do
        off' <- backwardsWordLoop buf off (fromIntegral @Natural @Word r)
        reverseBytes buf off0 (off' - 1)
        pure off'
      _ -> do
        off' <- backwardsPasteWordPaddedDec9
          (fromIntegral @Natural @Word r) buf off
        go q buf off0 off'

-- Reverse the bytes in the designated slice. This takes
-- an inclusive start offset and an inclusive end offset.
reverseBytes :: MutableByteArray s -> Int -> Int -> ST s ()
{-# inline reverseBytes #-}
reverseBytes arr begin end = go begin end where
  go ixA ixB = if ixA < ixB
    then do
      a :: Word8 <- PM.readByteArray arr ixA
      b :: Word8 <- PM.readByteArray arr ixB
      PM.writeByteArray arr ixA b
      PM.writeByteArray arr ixB a
      go (ixA + 1) (ixB - 1)
    else pure ()

backwardsPasteWordPaddedDec9 ::
  Word -> MutableByteArray s -> Int -> ST s Int
backwardsPasteWordPaddedDec9 !w !arr !off = do
  backwardsPutRem10
    (backwardsPutRem10 $ backwardsPutRem10 $ backwardsPutRem10 $
     backwardsPutRem10 $ backwardsPutRem10 $ backwardsPutRem10 $
     backwardsPutRem10 $ backwardsPutRem10
     (\_ _ _ -> pure ())
    ) arr off w
  pure (off + 9)

backwardsPutRem10 ::
     (MutableByteArray s -> Int -> Word -> ST s a)
  -> MutableByteArray s -> Int -> Word -> ST s a
{-# inline backwardsPutRem10 #-}
backwardsPutRem10 andThen arr off dividend = do
  let quotient = approxDiv10 dividend
      remainder = dividend - (10 * quotient)
  PM.writeByteArray arr off (unsafeWordToWord8 (remainder + 48))
  andThen arr (off + 1) quotient

backwardsWordLoop :: MutableByteArray s -> Int -> Word -> ST s Int
{-# inline backwardsWordLoop #-}
backwardsWordLoop arr off0 x0 = go off0 x0 where
  go !off !(x :: Word) = if x > 0
    then do
      let (y,z) = quotRem x 10
      PM.writeByteArray arr off (fromIntegral (z + 0x30) :: Word8)
      go (off + 1) y
    else pure off

-- | Replicate a byte the given number of times.
replicate ::
     Int -- ^ Number of times to replicate the byte
  -> Word8 -- ^ Byte to replicate
  -> Builder
replicate !len !w = fromEffect len
  (\marr off -> do
    PM.setByteArray marr off len w
    pure (off + len)
  )

-- Based on C code from https://stackoverflow.com/a/5558614
-- For numbers less than 1073741829, this gives a correct answer.
approxDiv10 :: Word -> Word
approxDiv10 !n = unsafeShiftR (0x1999999A * n) 32
  
-- -- A weird beast useful for rewrite rules. Not yet used. This will
-- -- ultimately replace fromEffect and fromBounded.
-- require :: Int -> Builder
-- require !n = Builder $ \buf0 off0 len0 cs0 s0 ->
--   let !(I# req) = n
--    in case len0 >=# req of
--         1# -> (# s0, buf0, off0, len0, cs0 #)
--         _ -> let !(I# lenX) = max 4080 (I# req) in
--           case Exts.newByteArray# lenX s0 of
--             (# sX, bufX #) ->
--               (# sX, bufX, 0#, lenX, Mutable buf0 off0 cs0 #)

unsafeWordToWord8 :: Word -> Word8
unsafeWordToWord8 (W# w) = W8# w
