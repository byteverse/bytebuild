{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language TypeApplications #-}

-- | The functions in this module do not check to
-- see if there is enough space in the buffer.
module Data.ByteArray.Builder.Small.Unsafe
  ( -- * Builder
    Builder(..)
  , construct
    -- * Execute
  , run
  , pasteST
  , pasteIO
    -- * Combine
  , append
    -- * Encode Integral Types
    -- ** Human-Readable
  , word64Dec
  , int64Dec
  , word64PaddedUpperHex
  , word32PaddedUpperHex
  , word16PaddedUpperHex
  , word8PaddedUpperHex
    -- ** Machine-Readable
  , word64BE
  , word32BE
  , word16BE
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Char (ord)
import Data.Primitive
import GHC.Exts
import GHC.ST
import GHC.Word
import GHC.Int
import Data.Kind
import GHC.TypeLits (KnownNat,Nat,type (+),natVal')

-- | A builder parameterized by the maximum number of bytes it uses
-- when executed.
newtype Builder :: Nat -> Type where
   Builder ::
        (forall s. MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #))
     -> Builder n

-- | Execute the builder. This function is safe.
run :: forall n. KnownNat n
  => Builder n -- ^ Builder
  -> ByteArray
{-# inline run #-}
run b = runST $ do
  arr <- newByteArray (fromIntegral (natVal' (proxy# :: Proxy# n)))
  len <- pasteST b arr 0
  shrinkMutableByteArray arr len
  unsafeFreezeByteArray arr

-- | This function does not enforce the known upper bound on the
-- size. It is up to the user to do this.
pasteST :: Builder n -> MutableByteArray s -> Int -> ST s Int
{-# inline pasteST #-}
pasteST (Builder f) (MutableByteArray arr) (I# off) =
  ST $ \s0 -> case f arr off s0 of
    (# s1, r #) -> (# s1, (I# r) #)

-- | This function does not enforce the known upper bound on the
-- size. It is up to the user to do this.
pasteIO :: Builder n -> MutableByteArray RealWorld -> Int -> IO Int
{-# inline pasteIO #-}
pasteIO b m off = stToIO (pasteST b m off)

-- | Constructor for 'Builder' that works on a function with lifted
-- arguments instead of unlifted ones. This is just as unsafe as the
-- actual constructor.
construct :: (forall s. MutableByteArray s -> Int -> ST s Int) -> Builder n
{-# inline construct #-}
construct f = Builder
  $ \arr off s0 ->
    case unST (f (MutableByteArray arr) (I# off)) s0 of
      (# s1, (I# n) #) -> (# s1, n #)

-- | Concatenate two builders.
append :: Builder n -> Builder m -> Builder (n + m)
append (Builder f) (Builder g) =
  Builder $ \arr off0 s0 -> case f arr off0 s0 of
    (# s1, r #) -> g arr r s1

-- | Requires up to 19 bytes. Encodes an unsigned 64-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
word64Dec :: Word64 -> Builder 19
word64Dec (W64# w) = word64Dec# w

-- | Requires up to 20 bytes. Encodes a signed 64-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
-- Negative numbers are preceded by a minus sign. Positive numbers
-- are not preceded by anything.
int64Dec :: Int64 -> Builder 20
int64Dec (I64# w) = int64Dec# w

-- | Requires up to 19 bytes.
word64Dec# :: Word# -> Builder 19
{-# noinline word64Dec# #-}
word64Dec# w# = construct $ \arr off0 -> if w /= 0
  then internalWordLoop arr off0 (W# w#)
  else do
    writeByteArray arr off0 (c2w '0')
    pure (off0 + 1)
  where
  w = W64# w#

internalWordLoop :: MutableByteArray s -> Int -> Word -> ST s Int
{-# inline internalWordLoop #-}
internalWordLoop arr off0 x0 = go off0 x0 where
  go !off !(x :: Word) = if x > 0
    then do
      let (y,z) = quotRem x 10
      writeByteArray arr off (fromIntegral (z + 0x30) :: Word8)
      go (off + 1) y
    else do
      reverseBytes arr off0 (off - 1)
      pure off

-- | Requires up to 19 bytes.
int64Dec# :: Int# -> Builder 20
{-# noinline int64Dec# #-}
int64Dec# w# = construct $ \arr off0 -> case compare w 0 of
  GT -> internalWordLoop arr off0 (fromIntegral w)
  EQ -> do
    writeByteArray arr off0 (c2w '0')
    pure (off0 + 1)
  LT -> do
    writeByteArray arr off0 (c2w '-')
    internalWordLoop arr (off0 + 1) (fromIntegral (negate w))
  where
  w = I64# w#

-- Convert a number between 0 and 16 to the ASCII
-- representation of its hexadecimal character.
-- The use of fromIntegral causes us to incur an
-- unneeded bitmask. This actually needs a Word64
-- argument.
toHexUpper :: Word -> Word8
toHexUpper w' = fromIntegral
    $ (complement theMask .&. loSolved)
  .|. (theMask .&. hiSolved)
  where
  w = w' .&. 0xF
  -- This is all ones if the value was >= 10
  theMask = (1 .&. unsafeShiftR (w - 10) 63) - 1
  loSolved = w + 48
  hiSolved = w + 55

-- | Requires exactly 16 bytes. Encodes a 64-bit unsigned integer as
-- hexadecimal, zero-padding the encoding to 16 digits. This uses
-- uppercase for the alphabetical digits. For example, this encodes the
-- number 1022 as @00000000000003FE@.
word64PaddedUpperHex :: Word64 -> Builder 16
word64PaddedUpperHex (W64# w) = word64PaddedUpperHex# w

-- | Requires exactly 8 bytes. Encodes a 32-bit unsigned integer as
-- hexadecimal, zero-padding the encoding to 8 digits. This uses
-- uppercase for the alphabetical digits.
word32PaddedUpperHex :: Word32 -> Builder 8
word32PaddedUpperHex (W32# w) = word32PaddedUpperHex# w

-- | Requires exactly 4 bytes. Encodes a 16-bit unsigned integer as
-- hexadecimal, zero-padding the encoding to 4 digits. This uses
-- uppercase for the alphabetical digits.
word16PaddedUpperHex :: Word16 -> Builder 4
word16PaddedUpperHex (W16# w) = word16PaddedUpperHex# w

-- | Requires exactly 2 bytes. Encodes a 8-bit unsigned integer as
-- hexadecimal, zero-padding the encoding to 2 digits. This uses
-- uppercase for the alphabetical digits.
word8PaddedUpperHex :: Word8 -> Builder 2
word8PaddedUpperHex (W8# w) = word8PaddedUpperHex# w

-- TODO: Is it actually worth unrolling this loop. I suspect that it
-- might not be. Benchmark this.
word64PaddedUpperHex# :: Word# -> Builder 16
{-# noinline word64PaddedUpperHex# #-}
word64PaddedUpperHex# w# = construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 60))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 56))
  writeByteArray arr (off + 2) (toHexUpper (unsafeShiftR w 52))
  writeByteArray arr (off + 3) (toHexUpper (unsafeShiftR w 48))
  writeByteArray arr (off + 4) (toHexUpper (unsafeShiftR w 44))
  writeByteArray arr (off + 5) (toHexUpper (unsafeShiftR w 40))
  writeByteArray arr (off + 6) (toHexUpper (unsafeShiftR w 36))
  writeByteArray arr (off + 7) (toHexUpper (unsafeShiftR w 32))
  writeByteArray arr (off + 8) (toHexUpper (unsafeShiftR w 28))
  writeByteArray arr (off + 9) (toHexUpper (unsafeShiftR w 24))
  writeByteArray arr (off + 10) (toHexUpper (unsafeShiftR w 20))
  writeByteArray arr (off + 11) (toHexUpper (unsafeShiftR w 16))
  writeByteArray arr (off + 12) (toHexUpper (unsafeShiftR w 12))
  writeByteArray arr (off + 13) (toHexUpper (unsafeShiftR w 8))
  writeByteArray arr (off + 14) (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 15) (toHexUpper (unsafeShiftR w 0))
  pure (off + 16)
  where
  w = W# w#

word32PaddedUpperHex# :: Word# -> Builder 8
{-# noinline word32PaddedUpperHex# #-}
word32PaddedUpperHex# w# = construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 28))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 24))
  writeByteArray arr (off + 2) (toHexUpper (unsafeShiftR w 20))
  writeByteArray arr (off + 3) (toHexUpper (unsafeShiftR w 16))
  writeByteArray arr (off + 4) (toHexUpper (unsafeShiftR w 12))
  writeByteArray arr (off + 5) (toHexUpper (unsafeShiftR w 8))
  writeByteArray arr (off + 6) (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 7) (toHexUpper (unsafeShiftR w 0))
  pure (off + 8)
  where
  w = W# w#

-- Not sure if it is beneficial to inline this. We just let
-- GHC make the decision. Open an issue on github if this is
-- a problem.
word16PaddedUpperHex# :: Word# -> Builder 4
word16PaddedUpperHex# w# = construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 12))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 8))
  writeByteArray arr (off + 2) (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 3) (toHexUpper (unsafeShiftR w 0))
  pure (off + 4)
  where
  w = W# w#

-- Definitely want this to inline. It's maybe a dozen instructions total.
word8PaddedUpperHex# :: Word# -> Builder 2
{-# inline word8PaddedUpperHex #-}
word8PaddedUpperHex# w# = construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 0))
  pure (off + 2)
  where
  w = W# w#

-- | Requires exactly 8 bytes. Dump the octets of a 64-bit
-- word in a big-endian fashion.
word64BE :: Word64 -> Builder 8
word64BE w = construct $ \arr off -> do
  writeByteArray arr (off    ) (fromIntegral @Word64 @Word8 (unsafeShiftR w 56))
  writeByteArray arr (off + 1) (fromIntegral @Word64 @Word8 (unsafeShiftR w 48))
  writeByteArray arr (off + 2) (fromIntegral @Word64 @Word8 (unsafeShiftR w 40))
  writeByteArray arr (off + 3) (fromIntegral @Word64 @Word8 (unsafeShiftR w 32))
  writeByteArray arr (off + 4) (fromIntegral @Word64 @Word8 (unsafeShiftR w 24))
  writeByteArray arr (off + 5) (fromIntegral @Word64 @Word8 (unsafeShiftR w 16))
  writeByteArray arr (off + 6) (fromIntegral @Word64 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off + 7) (fromIntegral @Word64 @Word8 w)
  pure (off + 8)

-- | Requires exactly 4 bytes. Dump the octets of a 32-bit
-- word in a big-endian fashion.
word32BE :: Word32 -> Builder 4
word32BE w = construct $ \arr off -> do
  writeByteArray arr (off    ) (fromIntegral @Word32 @Word8 (unsafeShiftR w 24))
  writeByteArray arr (off + 1) (fromIntegral @Word32 @Word8 (unsafeShiftR w 16))
  writeByteArray arr (off + 2) (fromIntegral @Word32 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off + 3) (fromIntegral @Word32 @Word8 w)
  pure (off + 4)

-- | Requires exactly 2 bytes. Dump the octets of a 16-bit
-- word in a big-endian fashion.
word16BE :: Word16 -> Builder 2
word16BE w = construct $ \arr off -> do
  writeByteArray arr (off    ) (fromIntegral @Word16 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off + 1) (fromIntegral @Word16 @Word8 w)
  pure (off + 2)

-- Reverse the bytes in the designated slice. This takes
-- an inclusive start offset and an inclusive end offset.
reverseBytes :: MutableByteArray s -> Int -> Int -> ST s ()
{-# inline reverseBytes #-}
reverseBytes arr begin end = go begin end where
  go ixA ixB = if ixA < ixB
    then do
      a :: Word8 <- readByteArray arr ixA
      b :: Word8 <- readByteArray arr ixB
      writeByteArray arr ixA b
      writeByteArray arr ixB a
      go (ixA + 1) (ixB - 1)
    else pure ()

c2w :: Char -> Word8
c2w = fromIntegral . ord

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

shrinkMutableByteArray :: MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  primitive_ (shrinkMutableByteArray# arr sz)
