{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Data.ByteArray.Builder.Small
  ( -- * Unsafe Primitives
    Builder(..)
  , construct
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
    -- * Numbers
  , word64Dec
  , word64PaddedUpperHex
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types
import Data.Primitive
import GHC.Exts
import GHC.ST
import GHC.Word
import GHC.TypeLits (KnownNat,natVal')
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))

import qualified Data.Primitive as PM
import qualified Data.Vector as V
import qualified Data.ByteArray.Builder.Small.Unsafe as Unsafe

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

instance Monoid Builder where
  mempty = Builder $ \_ off0 _ s0 -> (# s0, off0 #)

-- | Run a builder. An accurate size hint is important for good performance.
run ::
     Int -- ^ Hint for upper bound on size
  -> Builder -- ^ Builder
  -> ByteArray
run hint b = runByteArrayST $ do
  let go !n = do
        arr <- newByteArray n
        pasteST b (MutableBytes arr 0 n) >>= \case
          Nothing -> go (n + 64)
          Just len -> do
            shrinkMutableByteArray arr len
            unsafeFreezeByteArray arr
  go hint

pasteArrayIO ::
     MutableBytes RealWorld -- ^ Buffer
  -> (a -> Builder) -- ^ Builder
  -> V.Vector a -- ^ Elements to serialize
  -> IO (V.Vector a, MutableBytes RealWorld) -- ^ Shifted vector, shifted buffer
pasteArrayIO !arr f !xs = stToIO (pasteArrayST arr f xs)

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
  -> MutableByteArray s
     -- ^ Initial buffer, used linearly. Do not reuse this argument.
  -> ST s (MutableByteArrayOffset s)
     -- ^ Final buffer that accomodated the builder.
pasteGrowST !n b !arr0 = do
  let go !arr !sz = pasteST b (MutableBytes arr 0 sz) >>= \case
        Nothing -> do
          let szNext = sz + n
          arrNext <- PM.newByteArray szNext
          go arrNext szNext
        Just ix -> pure (MutableByteArrayOffset{array=arr,offset=ix})
  go arr0 =<< PM.getSizeofMutableByteArray arr0

-- | Variant of 'pasteGrowST' that runs in 'IO'.
pasteGrowIO ::
     Int -- ^ How many bytes to grow by at a time
  -> Builder
  -> MutableByteArray RealWorld
     -- ^ Initial buffer, used linearly. Do not reuse this argument.
  -> IO (MutableByteArrayOffset RealWorld)
     -- ^ Final buffer that accomodated the builder.
pasteGrowIO !n b !arr = stToIO (pasteGrowST n b arr)

pasteST :: Builder -> MutableBytes s -> ST s (Maybe Int)
{-# inline pasteST #-}
pasteST (Builder f) (MutableBytes (MutableByteArray arr) (I# off) (I# len)) =
  ST $ \s0 -> case f arr off len s0 of
    (# s1, r #) -> if isTrue# (r /=# (-1#))
      then (# s1, Just (I# r) #)
      else (# s1, Nothing #)

pasteIO :: Builder -> MutableBytes RealWorld -> IO (Maybe Int)
{-# inline pasteIO #-}
pasteIO b m = stToIO (pasteST b m)

construct :: (forall s. MutableBytes s -> ST s (Maybe Int)) -> Builder
construct f = Builder
  $ \arr off len s0 ->
    case unST (f (MutableBytes (MutableByteArray arr) (I# off) (I# len))) s0 of
      (# s1, m #) -> case m of
        Nothing -> (# s1, (-1#) #)
        Just (I# n) -> (# s1, n #)

fromUnsafe :: forall n. KnownNat n => Unsafe.Builder n -> Builder
{-# inline fromUnsafe #-}
fromUnsafe (Unsafe.Builder f) = Builder $ \arr off len s0 ->
  case fromIntegral (natVal' (proxy# :: Proxy# n)) of
    I# req -> case len >=# req of
      1# -> f arr off s0
      _ -> (# s0, (-1#) #)

bytearray :: ByteArray -> Builder
bytearray a = bytes (Bytes a 0 (sizeofByteArray a))

bytes :: Bytes -> Builder
bytes (Bytes src soff slen) = construct $ \(MutableBytes arr off len) -> if len >= slen
  then do
    copyByteArray arr off src soff slen
    pure (Just (len - slen))
  else pure Nothing

word64Dec :: Word64 -> Builder
word64Dec w = fromUnsafe (Unsafe.word64Dec w)

word64PaddedUpperHex :: Word64 -> Builder
word64PaddedUpperHex w =
  fromUnsafe (Unsafe.word64PaddedUpperHex w)

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

shrinkMutableByteArray :: MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  primitive_ (shrinkMutableByteArray# arr sz)

