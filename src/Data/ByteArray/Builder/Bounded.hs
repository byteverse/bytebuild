{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language BinaryLiterals #-}
{-# language UnboxedTuples #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language TypeApplications #-}

-- | The functions in this module are explict in the amount of bytes they require.
module Data.ByteArray.Builder.Bounded
  ( -- * Builder
    Builder
    -- * Execute
  , run
  , pasteGrowST
    -- * Combine
  , empty
  , append
    -- * Bounds Manipulation
  , weaken
  , substitute
    -- * Encode Integral Types
    -- ** Human-Readable
  , word64Dec
  , word32Dec
  , word16Dec
  , int64Dec
  , word64PaddedUpperHex
  , word32PaddedUpperHex
  , word16PaddedUpperHex
  , word8PaddedUpperHex
  , ascii
  , char
    -- ** Machine-Readable
  , word64BE
  , word32BE
  , word16BE
  , word8
    -- * Encode Floating-Point Types
  , doubleDec
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Char (ord)
import Data.Primitive
import GHC.Exts
import GHC.ST (ST(ST))
import GHC.Word (Word8(W8#),Word16(W16#),Word32(W32#),Word64(W64#))
import GHC.Int (Int64(I64#))
import GHC.TypeLits (KnownNat,type (+),natVal')
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))
import Arithmetic.Types (type (<=), type (:=:))
import Data.ByteArray.Builder.Bounded.Unsafe (Builder(..))

import qualified Data.ByteArray.Builder.Bounded.Unsafe as Unsafe
import qualified Data.Primitive as PM

-- Used internally.
knownNat :: KnownNat n => Proxy# n -> Int
{-# inline knownNat #-}
knownNat p = fromIntegral (natVal' p)

-- | Execute the bounded builder.
run :: forall n. KnownNat n
  => Builder n -- ^ Builder
  -> ByteArray
{-# inline run #-}
run b = runST $ do
  arr <- newByteArray (knownNat (proxy# :: Proxy# n))
  len <- Unsafe.pasteST b arr 0
  shrinkMutableByteArray arr len
  unsafeFreezeByteArray arr

-- | Paste the builder into the byte array starting at offset zero.
-- This reallocates the byte array if it cannot accomodate the builder,
-- growing it by the minimum amount necessary.
pasteGrowST :: forall n s. KnownNat n
  => Builder n
  -> MutableByteArrayOffset s
     -- ^ Initial buffer, used linearly. Do not reuse this argument.
  -> ST s (MutableByteArrayOffset s)
     -- ^ Final buffer that accomodated the builder.
{-# inline pasteGrowST #-}
pasteGrowST b !(MutableByteArrayOffset{array=arr0,offset=off0}) = do
  sz0 <- PM.getSizeofMutableByteArray arr0
  let req = knownNat (proxy# :: Proxy# n)
  let sz1 = off0 + req
  if sz1 <= sz0
    then do
      off1 <- Unsafe.pasteST b arr0 off0
      pure (MutableByteArrayOffset arr0 off1)
    else do
      arr1 <- PM.resizeMutableByteArray arr0 sz1
      off1 <- Unsafe.pasteST b arr1 off0
      pure (MutableByteArrayOffset arr1 off1)

-- | The monoidal unit of `append`
empty :: Builder 0
empty = Builder $ \_ off0 s0 -> (# s0, off0 #)

infixr 9 `append`

-- | Concatenate two builders.
append :: Builder m -> Builder n -> Builder (m + n)
append (Builder f) (Builder g) =
  Builder $ \arr off0 s0 -> case f arr off0 s0 of
    (# s1, r #) -> g arr r s1

-- | Weaken the bound on the maximum number of bytes required. For example,
-- to use two builders with unequal bounds in a disjunctive setting:
--
-- > import qualified Arithmetic.Lte as Lte
-- >
-- > buildNumber :: Either Double Word64 -> Builder 32
-- > buildNumber = \case
-- >   Left d  -> doubleDec d
-- >   Right w -> weaken (Lte.constant @19 @32) (word64Dec w)
weaken :: forall m n. (m <= n) -> Builder m -> Builder n
weaken !_ (Builder f) = Builder f

-- | Replace the upper bound on size with an equal number.
substitute :: forall m n. (m :=: n) -> Builder m -> Builder n
substitute !_ (Builder f) = Builder f

-- | Encode a double-floating-point number, using decimal notation or
-- scientific notation depending on the magnitude. This has undefined
-- behavior when representing @+inf@, @-inf@, and @NaN@. It will not
-- crash, but the generated numbers will be nonsense.
doubleDec :: Double -> Builder 32
doubleDec (D# d) = Builder (\arr off0 s0 -> doubleDec# d arr off0 s0)

-- | Requires up to 19 bytes. Encodes an unsigned 64-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
word64Dec :: Word64 -> Builder 19
word64Dec (W64# w) = wordCommonDec# w

-- | Requires up to 10 bytes. Encodes an unsigned 32-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
word32Dec :: Word32 -> Builder 10
word32Dec (W32# w) = wordCommonDec# w

-- | Requires up to 5 bytes. Encodes an unsigned 16-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
word16Dec :: Word16 -> Builder 5
word16Dec (W16# w) = wordCommonDec# w

-- | Requires up to 20 bytes. Encodes a signed 64-bit integer as decimal.
-- This encoding never starts with a zero unless the argument was zero.
-- Negative numbers are preceded by a minus sign. Positive numbers
-- are not preceded by anything.
int64Dec :: Int64 -> Builder 20
int64Dec (I64# w) = int64Dec# w

-- Requires a number of bytes that is bounded by the size of
-- the word. This is only used internally.
wordCommonDec# :: Word# -> Builder n
{-# noinline wordCommonDec# #-}
wordCommonDec# w# = Unsafe.construct $ \arr off0 -> if w /= 0
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
int64Dec# w# = Unsafe.construct $ \arr off0 -> case compare w 0 of
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
word64PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
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
word32PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
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
word16PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
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
word8PaddedUpperHex# w# = Unsafe.construct $ \arr off -> do
  writeByteArray arr off (toHexUpper (unsafeShiftR w 4))
  writeByteArray arr (off + 1) (toHexUpper (unsafeShiftR w 0))
  pure (off + 2)
  where
  w = W# w#

-- | Encode an ASCII char.
-- Precondition: Input must be an ASCII character. This is not checked.
ascii :: Char -> Builder 1
ascii c = word8 (fromIntegral @Int @Word8 (ord c))

-- | Encode a character as UTF-8. This only uses as much space as is required.
char :: Char -> Builder 4
char c
  | codepoint < 0x80 = Unsafe.construct $ \arr off -> do
      writeByteArray arr off (unsafeWordToWord8 codepoint)
      pure (off + 1)
  | codepoint < 0x800 = Unsafe.construct $ \arr off -> do
      writeByteArray arr off       (unsafeWordToWord8 (byteTwoOne codepoint))
      writeByteArray arr (off + 1) (unsafeWordToWord8 (byteTwoTwo codepoint))
      return (off + 2)
  | codepoint >= 0xD800 && codepoint < 0xE000 = Unsafe.construct $ \arr off -> do
      -- Codepoint U+FFFD
      writeByteArray arr off       (0xEF :: Word8)
      writeByteArray arr (off + 1) (0xBF :: Word8)
      writeByteArray arr (off + 2) (0xBD :: Word8)
      return (off + 3)
  | codepoint < 0x10000 = Unsafe.construct $ \arr off -> do
      writeByteArray arr off       (unsafeWordToWord8 (byteThreeOne codepoint))
      writeByteArray arr (off + 1) (unsafeWordToWord8 (byteThreeTwo codepoint))
      writeByteArray arr (off + 2) (unsafeWordToWord8 (byteThreeThree codepoint))
      return (off + 3)
  | otherwise = Unsafe.construct $ \arr off -> do
      writeByteArray arr off       (unsafeWordToWord8 (byteFourOne codepoint))
      writeByteArray arr (off + 1) (unsafeWordToWord8 (byteFourTwo codepoint))
      writeByteArray arr (off + 2) (unsafeWordToWord8 (byteFourThree codepoint))
      writeByteArray arr (off + 3) (unsafeWordToWord8 (byteFourFour codepoint))
      return (off + 4)

  where
    codepoint :: Word
    codepoint = fromIntegral (ord c)

    unsafeWordToWord8 :: Word -> Word8
    unsafeWordToWord8 (W# w) = W8# w

    -- precondition: codepoint is less than 0x800
    byteTwoOne :: Word -> Word
    byteTwoOne w = unsafeShiftR w 6 .|. 0b11000000

    byteTwoTwo :: Word -> Word
    byteTwoTwo w = (w .&. 0b00111111) .|. 0b10000000

    -- precondition: codepoint is less than 0x1000
    byteThreeOne :: Word -> Word
    byteThreeOne w = unsafeShiftR w 12 .|. 0b11100000

    byteThreeTwo :: Word -> Word
    byteThreeTwo w = (0b00111111 .&. unsafeShiftR w 6) .|. 0b10000000

    byteThreeThree :: Word -> Word
    byteThreeThree w = (w .&. 0b00111111) .|. 0b10000000

    -- precondition: codepoint is less than 0x110000
    byteFourOne :: Word -> Word
    byteFourOne w = unsafeShiftR w 18 .|. 0b11110000

    byteFourTwo :: Word -> Word
    byteFourTwo w = (0b00111111 .&. unsafeShiftR w 12) .|. 0b10000000

    byteFourThree :: Word -> Word
    byteFourThree w = (0b00111111 .&. unsafeShiftR w 6) .|. 0b10000000

    byteFourFour :: Word -> Word
    byteFourFour w = (0b00111111 .&. w) .|. 0b10000000

-- | Requires exactly 8 bytes. Dump the octets of a 64-bit
-- word in a big-endian fashion.
word64BE :: Word64 -> Builder 8
word64BE w = Unsafe.construct $ \arr off -> do
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
word32BE w = Unsafe.construct $ \arr off -> do
  writeByteArray arr (off    ) (fromIntegral @Word32 @Word8 (unsafeShiftR w 24))
  writeByteArray arr (off + 1) (fromIntegral @Word32 @Word8 (unsafeShiftR w 16))
  writeByteArray arr (off + 2) (fromIntegral @Word32 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off + 3) (fromIntegral @Word32 @Word8 w)
  pure (off + 4)

-- | Requires exactly 2 bytes. Dump the octets of a 16-bit
-- word in a big-endian fashion.
word16BE :: Word16 -> Builder 2
word16BE w = Unsafe.construct $ \arr off -> do
  writeByteArray arr (off    ) (fromIntegral @Word16 @Word8 (unsafeShiftR w 8))
  writeByteArray arr (off + 1) (fromIntegral @Word16 @Word8 w)
  pure (off + 2)

word8 :: Word8 -> Builder 1
word8 w = Unsafe.construct $ \arr off -> do
  writeByteArray arr off w
  pure (off + 1)

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

shrinkMutableByteArray :: MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  primitive_ (shrinkMutableByteArray# arr sz)

-- This is adapted from androider's code in https://stackoverflow.com/a/7097567
-- The checks for infinity and NaN have been removed. Note that this is a little
-- inaccurate. This is very visible when encoding a number like 2.25, which
-- is perfectly represented as an IEEE 754 floating point number but is goofed
-- up by this function.
-- If you modify this function, please take a took at the resulting core.
-- It currently performs no boxing at all, and it would be nice to keep
-- it that way.
doubleDec# :: forall s.
  Double# -> MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
{-# noinline doubleDec# #-}
doubleDec# d# marr# off# s0 = unIntST s0 $ do
  let marr = MutableByteArray marr#
  let d0 = D# d#
  let off0 = I# off#
  if d0 == 0
    then do
      writeByteArray marr off0 (c2w '0')
      pure (off0 + 1)
    else do
      let neg = d0 < 0
      off1 <- if neg
        then do
          writeByteArray marr off0 (c2w '-')
          pure (off0 + 1)
        else pure off0
      let d1 = abs d0
      let mag0 = floor (logBase10 d1) :: Int
      let useExp = (mag0 >= 14 || (neg && mag0 >= 9) || mag0 <= (-9))
      -- This straightforward adaptation of the C code is awkward
      -- in Haskell. Binding the triple where mag1 might not even
      -- get used is strange.
      let !(!d2,!mag1,!mag0A) = if useExp
            then
              let mag0' = if mag0 < 0 then mag0 - 1 else mag0
               in (d1 / (10.0 ** fromIntegral @Int @Double mag0'), mag0', 0)
            else (d1,0,mag0)
      let mag0B = if mag0A < 1 then 0 else mag0A
      let goNum :: Double -> Int -> Int -> ST s Int
          goNum !dA0 !mag !offA0 = if (dA0 > doublePrecision || mag >= 0)
            then do
              let weight = 10.0 ** (fromIntegral @Int @Double mag)
              -- We should actually check weight with isinf here,
              -- but we do not.
              (dA1,offA1) <- if weight > 0
                then do
                  -- TODO: use a better floor function
                  let digit = ((floor :: Double -> Int) (dA0 / weight))
                  let discard = fromIntegral @Int @Double digit * weight
                  writeByteArray marr offA0
                    (fromIntegral @Int @Word8 (digit + ord '0'))
                  pure (dA0 - discard,offA0 + 1)
                else pure (dA0,offA0)
              offA2 <- if mag == 0 && dA1 > 0
                then do
                  writeByteArray marr offA1 (c2w '.')
                  pure (offA1 + 1)
                else pure offA1
              goNum dA1 (mag - 1) offA2
            else pure offA0
      !off2 <- goNum d2 mag0B off1
      off3 <- if useExp
        then do
          writeByteArray marr off2 (c2w 'e')
          !mag2 <- if mag1 > 0
            then do
              writeByteArray marr (off2 + 1) (c2w '+')
              pure mag1
            else do
              writeByteArray marr (off2 + 1) (c2w '-')
              pure (-mag1)
          let goMag !mag !off = if mag > 0
                then do
                  let (q,r) = quotRem mag 10
                  writeByteArray marr off (fromIntegral @Int @Word8 (ord '0' + r))
                  goMag q (off + 1)
                else pure off
          !off3 <- goMag mag2 (off2 + 2)
          reverseBytes marr (off2 + 2) (off3 - 1)
          pure off3
        else pure off2
      pure off3

doublePrecision :: Double
doublePrecision = 0.00000000000001

unIntST :: State# s -> ST s Int -> (# State# s, Int# #)
{-# inline unIntST #-}
unIntST s0 (ST f) = case f s0 of
  (# s1, I# i #) -> (# s1, i #)

-- This is slightly inaccurate. I think this can actually cause
-- problems in some situations. The log10 function from C would
-- be better. The inaccuracy here cause the logarithm to be slightly
-- larger than it should be. There might actually be a simple way to
-- fix this by just using recursion to compute it. We just floor the
-- result anyway. Hmm...
logBase10 :: Double -> Double
logBase10 d = log d / 2.30258509299
