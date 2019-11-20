{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Data.ByteArray.Builder.Unsafe
  ( -- * Types
    Builder(..)
  , Commits(..)
    -- * Construction
  , fromEffect
    -- * Finalization
  , reverseCommitsOntoChunks
    -- * Safe Functions
    -- | These functions are actually completely safe, but they are defined
    -- here because they are used by typeclass instances. Import them from
    -- @Data.ByteArray.Builder@ instead.
  , stringUtf8
  , cstring
  ) where

import Control.Monad.Primitive (primitive_)
import Data.Bytes.Chunks (Chunks(ChunksCons))
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (MutableByteArray(..),ByteArray(..))
import Foreign.C.String (CString)
import GHC.Base (unpackCString#,unpackCStringUtf8#)
import GHC.Exts ((-#),(+#),(>#),(>=#))
import GHC.Exts (Addr#,ByteArray#,MutableByteArray#,Int(I#),Ptr(Ptr))
import GHC.Exts (IsString,Int#,State#)
import GHC.ST (ST(ST))

import qualified Data.ByteArray.Builder.Bounded as Bounded
import qualified Data.ByteArray.Builder.Bounded.Unsafe as UnsafeBounded
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- | An unmaterialized sequence of bytes that may be pasted
-- into a mutable byte array.
newtype Builder
  = Builder (forall s.
      MutableByteArray# s ->   -- buffer we are currently writing to
      Int# ->   -- offset into the current buffer
      Int# ->   -- number of bytes remaining in the current buffer
      Commits s ->   -- buffers and immutable byte slices that we have already committed
      State# s ->
      (# State# s, MutableByteArray# s, Int#, Int#, Commits s #) -- all the same things
    )

instance IsString Builder where
  {-# inline fromString #-}
  fromString = stringUtf8

instance Semigroup Builder where
  {-# inline (<>) #-}
  Builder f <> Builder g = Builder $ \buf0 off0 len0 cs0 s0 -> case f buf0 off0 len0 cs0 s0 of
    (# s1, buf1, off1, len1, cs1 #) -> g buf1 off1 len1 cs1 s1

instance Monoid Builder where
  {-# inline mempty #-}
  mempty = Builder $ \buf0 off0 len0 cs0 s0 -> (# s0, buf0, off0, len0, cs0 #)

data Commits s
  = Mutable
      (MutableByteArray# s)
      -- ^ Mutable buffer, start index implicitly zero
      Int# -- ^ Length (may be smaller than actual length)
      !(Commits s)
  | Immutable
      ByteArray# -- ^ Immutable chunk
      Int# -- ^ Offset into chunk, not necessarily zero
      Int# -- ^ Length (may be smaller than actual length)
      !(Commits s)
  | Initial

-- | Cons the chunks from a list of @Commits@ onto an initial
-- @Chunks@ list (this argument is often @ChunksNil@). This reverses
-- the order of the chunks, which is desirable since builders assemble
-- @Commits@ with the chunks backwards. This performs an in-place shrink
-- and freezes on any mutable byte arrays it encounters. Consequently,
-- these must not be reused.
reverseCommitsOntoChunks :: Chunks -> Commits s -> ST s Chunks
reverseCommitsOntoChunks !xs Initial = pure xs
reverseCommitsOntoChunks !xs (Immutable arr off len cs) =
  reverseCommitsOntoChunks (ChunksCons (Bytes (ByteArray arr) (I# off) (I# len)) xs) cs
reverseCommitsOntoChunks !xs (Mutable buf len cs) = case len of
  -- Skip over empty byte arrays.
  0# -> reverseCommitsOntoChunks xs cs
  _ -> do
    shrinkMutableByteArray (MutableByteArray buf) (I# len)
    arr <- PM.unsafeFreezeByteArray (MutableByteArray buf)
    reverseCommitsOntoChunks (ChunksCons (Bytes arr 0 (I# len)) xs) cs

-- | Create a builder from a cons-list of 'Char'. These
-- are be UTF-8 encoded.
stringUtf8 :: String -> Builder
{-# inline stringUtf8 #-}
stringUtf8 cs = Builder (goString cs)

-- | Create a builder from a @NUL@-terminated 'CString'. This ignores any
-- textual encoding, copying bytes until @NUL@ is reached.
cstring :: CString -> Builder
{-# inline cstring #-}
cstring (Ptr cs) = Builder (goCString cs)

goString :: String
  -> MutableByteArray# s -> Int# -> Int# -> Commits s
  -> State# s -> (# State# s, MutableByteArray# s, Int#, Int#, Commits s #)
{-# noinline goString #-}
goString [] buf0 off0 len0 cs0 s0 = (# s0, buf0, off0, len0, cs0 #)
goString (c : cs) buf0 off0 len0 cs0 s0 = case len0 ># 3# of
  1# -> case unST (UnsafeBounded.pasteST (Bounded.char c) (MutableByteArray buf0) (I# off0)) s0 of
    (# s1, I# off1 #) -> goString cs buf0 off1 (len0 -# (off1 -# off0)) cs0 s1
  _ -> case Exts.newByteArray# 4080# s0 of
    (# s1, buf1 #) -> case unST (UnsafeBounded.pasteST (Bounded.char c) (MutableByteArray buf1) 0) s1 of
      (# s2, I# off1 #) -> goString cs buf1 off1 (4080# -# off1) (Mutable buf0 off0 cs0) s2

-- We have to have a rule for both unpackCString# and unpackCStringUtf8#
-- since GHC uses a different function based on whether or not non-ASCII
-- codepoints are used in the string.
-- TODO: The UTF-8 variant of this rule is unsound because GHC actually
-- used Modified UTF-8.
{-# RULES
"Builder stringUtf8/cstring" forall s a b c d e.
  goString (unpackCString# s) a b c d e = goCString s a b c d e
"Builder stringUtf8/cstring-utf8" forall s a b c d e.
  goString (unpackCStringUtf8# s) a b c d e = goCString s a b c d e
#-}

goCString :: Addr# -> MutableByteArray# s -> Int# -> Int# -> Commits s
  -> State# s -> (# State# s, MutableByteArray# s, Int#, Int#, Commits s #)
goCString addr buf0 off0 len0 cs0 s0 = case Exts.indexWord8OffAddr# addr 0# of
  0## -> (# s0, buf0, off0, len0, cs0 #)
  w -> case len0 of
    0# -> case Exts.newByteArray# 4080# s0 of
      (# s1, buf1 #) -> case Exts.writeWord8Array# buf1 0# w s1 of
        s2 -> goCString
          (Exts.plusAddr# addr 1# ) buf1 1# (4080# -# 1# )
          (Mutable buf0 off0 cs0)
          s2
    _ -> case Exts.writeWord8Array# buf0 off0 w s0 of
      s1 -> goCString (Exts.plusAddr# addr 1# ) buf0 (off0 +# 1# ) (len0 -# 1# ) cs0 s1

fromEffect ::
     Int -- ^ Maximum number of bytes the paste function needs
  -> (forall s. MutableByteArray s -> Int -> ST s Int)
     -- ^ Paste function. Takes a byte array and an offset and returns
     -- the new offset and having pasted into the buffer.
  -> Builder
{-# inline fromEffect #-}
fromEffect (I# req) f = Builder $ \buf0 off0 len0 cs0 s0 ->
  let !(# s1, buf1, off1, len1, cs1 #) = case len0 >=# req of
        1# -> (# s0, buf0, off0, len0, cs0 #)
        _ -> let !(I# lenX) = max 4080 (I# req) in
          case Exts.newByteArray# lenX s0 of
            (# sX, bufX #) ->
              (# sX, bufX, 0#, lenX, Mutable buf0 off0 cs0 #)
   in case unST (f (MutableByteArray buf1) (I# off1)) s1 of
        (# s2, I# off2 #) -> (# s2, buf1, off2, len1 -# (off2 -# off1), cs1 #)

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

shrinkMutableByteArray :: MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  primitive_ (Exts.shrinkMutableByteArray# arr sz)
