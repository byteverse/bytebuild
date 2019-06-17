{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}

module Data.ByteArray.Builder.Small
  ( -- * Unsafe Primitives
    Builder(..)
  , construct
    -- * Evaluation
  , run
  , pasteST
    -- * Numbers
  , word64Dec
  ) where

import Control.Monad.Primitive
import Data.Char (ord)
import Data.Primitive
import GHC.Exts
import GHC.Word
import Data.Bytes.Types
import GHC.ST

-- | An unmaterialized sequence of bytes that may be pasted
-- into a mutable byte array.
newtype Builder = Builder
  (forall s. MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #))

instance Semigroup Builder where
  {-# inline (<>) #-}
  Builder f <> Builder g = Builder $ \arr off0 len0 s0 -> case f arr off0 len0 s0 of
    (# s1, r #) -> case r /=# (-1#) of
      1# -> g arr r (len0 +# (off0 -# r)) s1
      _ -> (# s1, (-1#) #)

-- | Run a builder. An accurate size hint is important for good performance.
run ::
     Int -- ^ Hint for upper bound on size
  -> Builder -- ^ Builder
  -> ByteArray
run hint b = runST $ do
  let go !n = do
        arr <- newByteArray n
        pasteST b (MutableBytes arr 0 n) >>= \case
          Nothing -> go (n + 64)
          Just len -> do
            shrinkMutableByteArray arr len
            unsafeFreezeByteArray arr
  go hint

pasteST :: Builder -> MutableBytes s -> ST s (Maybe Int)
{-# inline pasteST #-}
pasteST (Builder f) (MutableBytes (MutableByteArray arr) (I# off) (I# len)) =
  ST $ \s0 -> case f arr off len s0 of
    (# s1, r #) -> if isTrue# (r /=# (-1#))
      then (# s1, Just (I# r) #)
      else (# s1, Nothing #)

construct :: (forall s. MutableBytes s -> ST s (Maybe Int)) -> Builder
construct f = Builder
  $ \arr off len s0 ->
    case unST (f (MutableBytes (MutableByteArray arr) (I# off) (I# len))) s0 of
      (# s1, m #) -> case m of
        Nothing -> (# s1, (-1#) #)
        Just (I# n) -> (# s1, n #)

word64Dec :: Word64 -> Builder
word64Dec (W64# w) = word64Dec# w

word64Dec# :: Word# -> Builder
{-# noinline word64Dec# #-}
word64Dec# w# = construct $ \(MutableBytes arr off0 len) -> if len >= 19
  then if w /= 0
    then do
      let go off x = if x > 0
            then do
              let (y,z) = quotRem x 10
              writeByteArray arr off (fromIntegral (z + 0x30) :: Word8)
              go (off + 1) y
            else do
              reverseBytes arr off0 (off - 1)
              pure (Just off)
      go off0 w
    else do
      writeByteArray arr off0 (c2w '0')
      pure (Just (off0 + 1))
  else pure Nothing
  where
  w = W64# w#

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

