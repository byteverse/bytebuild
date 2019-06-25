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

-- | The functions in this module do not check to
-- see if there is enough space in the buffer.
module Data.ByteArray.Builder.Small.Unsafe
  ( Builder(..)
  , run
  , pasteST
  , pasteIO
  , construct
  , append
  , word64Dec
  , word64PaddedUpperHex
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Bytes.Types
import Data.Char (ord)
import Data.Primitive
import GHC.Exts
import GHC.ST
import GHC.Word
import Data.Kind
import GHC.TypeLits (KnownNat,Nat,type (+),natVal')

newtype Builder :: Nat -> Type where
   Builder ::
        (forall s. MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #))
     -> Builder n

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

construct :: (forall s. MutableByteArray s -> Int -> ST s Int) -> Builder n
{-# inline construct #-}
construct f = Builder
  $ \arr off s0 ->
    case unST (f (MutableByteArray arr) (I# off)) s0 of
      (# s1, (I# n) #) -> (# s1, n #)

append :: Builder n -> Builder m -> Builder (n + m)
append (Builder f) (Builder g) =
  Builder $ \arr off0 s0 -> case f arr off0 s0 of
    (# s1, r #) -> g arr r s1

-- | Requires up to 19 bytes.
word64Dec :: Word64 -> Builder 19
word64Dec (W64# w) = word64Dec# w

-- | Requires up to 19 bytes.
word64Dec# :: Word# -> Builder 19
{-# noinline word64Dec# #-}
word64Dec# w# = construct $ \arr off0 -> if w /= 0
  then do
    let go off x = if x > 0
          then do
            let (y,z) = quotRem x 10
            writeByteArray arr off (fromIntegral (z + 0x30) :: Word8)
            go (off + 1) y
          else do
            reverseBytes arr off0 (off - 1)
            pure off
    go off0 w
  else do
    writeByteArray arr off0 (c2w '0')
    pure (off0 + 1)
  where
  w = W64# w#

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

-- | Requires up to 16 bytes.
word64PaddedUpperHex :: Word64 -> Builder 16
word64PaddedUpperHex (W64# w) = word64PaddedUpperHex# w

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
