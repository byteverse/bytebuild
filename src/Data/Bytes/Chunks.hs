{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language TypeFamilies #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language NamedFieldPuns #-}

module Data.Bytes.Chunks
  ( Chunks(..)
  , concat
  ) where

import Prelude hiding (length,concat)

import GHC.ST (ST(..))
import Data.Bytes.Types (Bytes(..))
import Data.Primitive (ByteArray(..),MutableByteArray(..))
import GHC.Exts (ByteArray#,MutableByteArray#)
import GHC.Exts (IsList,Int#,State#,Int(I#),(+#),(-#))
import Control.Monad.ST.Run (runByteArrayST)

import qualified GHC.Exts as Exts
import qualified Data.Primitive as PM

data Chunks
  = ChunksCons {-# UNPACK #-} !Bytes !Chunks
  | ChunksNil

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

unI :: Int -> Int#
unI (I# i) = i

unBa :: ByteArray -> ByteArray#
unBa (ByteArray x) = x
