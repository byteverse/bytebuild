{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Data.Bytes.Builder.Unsafe
  ( -- * Types
    Builder(..)
  , BuilderState(..)
  , Commits(..)
    -- * Execution
  , pasteST
  , pasteIO
    -- * Construction
  , fromEffect
    -- * Builder State
  , newBuilderState
  , closeBuilderState
    -- * Finalization
  , reverseCommitsOntoChunks
  , commitsOntoChunks
  , copyReverseCommits
  , addCommitsLength
    -- * Commit Distance
  , commitDistance
  , commitDistance1
    -- * Safe Functions
    -- | These functions are actually completely safe, but they are defined
    -- here because they are used by typeclass instances. Import them from
    -- @Data.Bytes.Builder@ instead.
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
import GHC.Exts (RealWorld,IsString,Int#,State#)
import GHC.ST (ST(ST))
import GHC.IO (stToIO)

import qualified Data.Bytes.Builder.Bounded as Bounded
import qualified Data.Bytes.Builder.Bounded.Unsafe as UnsafeBounded
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

-- | A list of committed chunks along with the chunk currently being
-- written to. This is kind of like a non-empty variant of 'Commmits'
-- but with the additional invariant that the head chunk is a mutable
-- byte array.
data BuilderState s = BuilderState
  (MutableByteArray# s) -- buffer we are currently writing to
  Int# -- offset into the current buffer
  Int# -- number of bytes remaining in the current buffer
  !(Commits s) -- buffers and immutable byte slices that are already committed

-- | Create an empty 'BuilderState' with a buffer of the given size.
newBuilderState :: Int -> ST s (BuilderState s)
{-# inline newBuilderState #-}
newBuilderState n@(I# n# ) = do
  MutableByteArray buf <- PM.newByteArray n
  pure (BuilderState buf 0# n# Initial)

-- | Push the active chunk onto the top of the commits.
-- The @BuilderState@ argument must not be reused after being passed
-- to this function. That is, its use must be affine.
closeBuilderState :: BuilderState s -> Commits s
closeBuilderState (BuilderState dst off _ cmts) = Mutable dst off cmts

-- | Run a builder, performing an in-place update on the state.
-- The @BuilderState@ argument must not be reused after being passed
-- to this function. That is, its use must be affine.
pasteST :: Builder -> BuilderState s -> ST s (BuilderState s)
{-# inline pasteST #-}
pasteST (Builder f) (BuilderState buf off len cmts) = ST $ \s0 ->
  case f buf off len cmts s0 of
    (# s1, buf1, off1, len1, cmts1 #) ->
      (# s1, BuilderState buf1 off1 len1 cmts1 #)

-- | Variant of 'pasteST' that runs in 'IO'.
pasteIO :: Builder -> BuilderState RealWorld -> IO (BuilderState RealWorld)
{-# inline pasteIO #-}
pasteIO b st = stToIO (pasteST b st)

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

-- | Add the total number of bytes in the commits to first
-- argument.
addCommitsLength :: Int -> Commits s -> Int
addCommitsLength !acc Initial = acc
addCommitsLength !acc (Immutable _ _ x cs) = addCommitsLength (acc + I# x) cs
addCommitsLength !acc (Mutable _ x cs) = addCommitsLength (acc + I# x) cs

-- | Cons the chunks from a list of @Commits@ onto an initial
-- @Chunks@ list (this argument is often @ChunksNil@). This reverses
-- the order of the chunks, which is desirable since builders assemble
-- @Commits@ with the chunks backwards. This performs an in-place shrink
-- and freezes any mutable byte arrays it encounters. Consequently,
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

-- | Variant of 'reverseCommitsOntoChunks' that does not reverse
-- the order of the commits. Since commits are built backwards by
-- consing, this means that the chunks appended to the front will
-- be backwards. Within each chunk, however, the bytes will be in
-- the correct order.
--
-- Unlike 'reverseCommitsOntoChunks', this function is not tail
-- recursive.
commitsOntoChunks :: Chunks -> Commits s -> ST s Chunks
commitsOntoChunks !xs0 cs0 = go cs0
  where
  go Initial = pure xs0
  go (Immutable arr off len cs) = do
    xs <- go cs
    pure $! ChunksCons (Bytes (ByteArray arr) (I# off) (I# len)) xs
  go (Mutable buf len cs) = case len of
    -- Skip over empty byte arrays.
    0# -> go cs
    _ -> do
      shrinkMutableByteArray (MutableByteArray buf) (I# len)
      arr <- PM.unsafeFreezeByteArray (MutableByteArray buf)
      xs <- go cs
      pure $! ChunksCons (Bytes arr 0 (I# len)) xs

-- | Copy the contents of the chunks into a mutable array, reversing
-- the order of the chunks.
-- Precondition: The destination must have enough space to house the
-- contents. This is not checked.
copyReverseCommits ::
     MutableByteArray s -- ^ Destination
  -> Int -- ^ Destination range successor
  -> Commits s -- ^ Source
  -> ST s Int
{-# inline copyReverseCommits #-}
copyReverseCommits (MutableByteArray dst) (I# off) cs = ST
  (\s0 -> case copyReverseCommits# dst off cs s0 of
    (# s1, nextOff #) -> (# s1, I# nextOff #)
  )

copyReverseCommits# ::
     MutableByteArray# s
  -> Int#
  -> Commits s
  -> State# s
  -> (# State# s, Int# #)
copyReverseCommits# _ off Initial s0 = (# s0, off #)
copyReverseCommits# marr prevOff (Mutable arr sz cs) s0 =
  let !off = prevOff -# sz in
  case Exts.copyMutableByteArray# arr 0# marr off sz s0 of
    s1 -> copyReverseCommits# marr off cs s1
copyReverseCommits# marr prevOff (Immutable arr soff sz cs) s0 =
  let !off = prevOff -# sz in
  case Exts.copyByteArray# arr soff marr off sz s0 of
    s1 -> copyReverseCommits# marr off cs s1

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

-- | Variant of commitDistance where you get to supply a
-- head of the commit list that has not yet been committed.
commitDistance1 ::
     MutableByteArray# s -- target
  -> Int# -- offset into target
  -> MutableByteArray# s -- head of array
  -> Int# -- offset into head of array
  -> Commits s
  -> Int#
commitDistance1 target offTarget buf0 offBuf cs =
  case Exts.sameMutableByteArray# target buf0 of
    1# -> offBuf -# offTarget
    _ -> commitDistance target offBuf cs -# offTarget

-- | Compute the number of bytes between the last byte and the offset
-- specified in a chunk. Precondition: the chunk must exist in the
-- list of committed chunks. This relies on mutable byte arrays having
-- identity (e.g. it uses @sameMutableByteArray#@).
commitDistance :: MutableByteArray# s -> Int# -> Commits s -> Int#
commitDistance !_ !_ Initial = errorWithoutStackTrace "chunkDistance: chunk not found"
commitDistance target !n (Immutable _ _ len cs) =
  commitDistance target (n +# len) cs
commitDistance target !n (Mutable buf len cs) =
  case Exts.sameMutableByteArray# target buf of
    1# -> n +# len
    _ -> commitDistance target (n +# len) cs
