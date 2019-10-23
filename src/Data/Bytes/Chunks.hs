{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language TypeFamilies #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language NamedFieldPuns #-}

module Data.Bytes.Chunks
  ( Chunks(..)
  , concat
  , reverse
  , reverseOnto
  ) where

import Prelude hiding (length,concat,reverse)

import GHC.ST (ST(..))
import Data.Bytes.Types (Bytes(..))
import Data.Primitive (ByteArray(..),MutableByteArray(..))
import GHC.Exts (ByteArray#,MutableByteArray#)
import GHC.Exts (Int#,State#,Int(I#),(+#))
import Control.Monad.ST.Run (runByteArrayST)

import qualified GHC.Exts as Exts
import qualified Data.Primitive as PM

data Chunks
  = ChunksCons {-# UNPACK #-} !Bytes !Chunks
  | ChunksNil
  deriving stock (Show)

instance Semigroup Chunks where
  ChunksNil <> a = a
  cs@(ChunksCons _ _) <> ChunksNil = cs
  as@(ChunksCons _ _) <> bs@(ChunksCons _ _) =
    reverseOnto bs (reverse as)

instance Monoid Chunks where
  mempty = ChunksNil

instance Eq Chunks where
  -- TODO: There is a more efficient way to do this, but
  -- it is tedious.
  a == b = concat a == concat b

concat :: Chunks -> ByteArray
concat x = ByteArray (concat# x)

concat# :: Chunks -> ByteArray#
{-# noinline concat# #-}
concat# ChunksNil = case mempty of {ByteArray x -> x}
concat# (ChunksCons (Bytes{array=c,offset=coff,length=szc}) cs) = case cs of
  ChunksNil -> case c of {ByteArray x -> x}
  ChunksCons (Bytes{array=d,offset=doff,length=szd}) ds ->
    unBa $ runByteArrayST $ do
      let szboth = szc + szd
          len = chunksLengthGo szboth ds
      dst <- PM.newByteArray len
      PM.copyByteArray dst 0 c coff szc
      PM.copyByteArray dst szc d doff szd
      _ <- copy dst szboth ds
      PM.unsafeFreezeByteArray dst

chunksLengthGo :: Int -> Chunks -> Int
chunksLengthGo !n ChunksNil = n
chunksLengthGo !n (ChunksCons (Bytes{length}) cs) =
  chunksLengthGo (n + length) cs

-- | Copy the contents of the chunks into a mutable array.
-- Precondition: The destination must have enough space to
-- house the contents. This is not checked.
copy ::
     MutableByteArray s -- ^ Destination
  -> Int -- ^ Destination offset
  -> Chunks -- ^ Source
  -> ST s Int -- ^ Returns the next index into the destination after the payload
{-# inline copy #-}
copy (MutableByteArray dst) (I# off) cs = ST
  (\s0 -> case copy# dst off cs s0 of
    (# s1, nextOff #) -> (# s1, I# nextOff #)
  )

copy# :: MutableByteArray# s -> Int# -> Chunks -> State# s -> (# State# s, Int# #)
copy# _ off ChunksNil s0 = (# s0, off #)
copy# marr off (ChunksCons (Bytes{array,offset,length}) cs) s0 =
  case Exts.copyByteArray# (unBa array) (unI offset) marr off (unI length) s0 of
    s1 -> copy# marr (off +# unI length) cs s1


-- | Reverse chunks but not the bytes within each chunk.
reverse :: Chunks -> Chunks
reverse = reverseOnto ChunksNil

-- | Variant of 'reverse' that allows the caller to provide
-- an initial list of chunks that the reversed chunks will
-- be pushed onto.
reverseOnto :: Chunks -> Chunks -> Chunks
reverseOnto !x ChunksNil = x
reverseOnto !x (ChunksCons y ys) =
  reverseOnto (ChunksCons y x) ys

unI :: Int -> Int#
unI (I# i) = i

unBa :: ByteArray -> ByteArray#
unBa (ByteArray x) = x
