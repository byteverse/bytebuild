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
    -- * Safe Functions
    -- | These functions are actually completely safe, but they are defined
    -- here because they are used by typeclass instances. Import them from
    -- @Data.ByteArray.Builder@ instead.
  , stringUtf8
  , cstring
  ) where

import Data.Primitive (MutableByteArray(MutableByteArray))
import Foreign.C.String (CString)
import GHC.Exts ((-#),(+#),(/=#),(>#))
import GHC.Exts (Addr#,Int(I#),Ptr(Ptr))
import GHC.Exts (IsString,Int#,State#,MutableByteArray#)
import GHC.ST (ST(ST))
import GHC.Base (unpackCString#,unpackCStringUtf8#)

import qualified GHC.Exts as Exts
import qualified Data.ByteArray.Builder.Bounded as Bounded
import qualified Data.ByteArray.Builder.Bounded.Unsafe as UnsafeBounded

-- | An unmaterialized sequence of bytes that may be pasted
-- into a mutable byte array.
newtype Builder
  = Builder (forall s. MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #))
    -- ^ This function takes a buffer, an offset, and a number of remaining bytes.
    --   It returns the new offset (should be greater than the old offset), or if
    --   there was not enough space left in buffer, it returns -1.

instance IsString Builder where
  {-# inline fromString #-}
  fromString = stringUtf8

instance Semigroup Builder where
  {-# inline (<>) #-}
  Builder f <> Builder g = Builder $ \arr off0 len0 s0 -> case f arr off0 len0 s0 of
    (# s1, r #) -> case r /=# (-1#) of
      1# -> g arr r (len0 +# (off0 -# r)) s1
      _ -> (# s1, (-1#) #)

instance Monoid Builder where
  {-# inline mempty #-}
  mempty = Builder $ \_ off0 _ s0 -> (# s0, off0 #)

-- | Create a builder from a cons-list of 'Char'. These
-- are be UTF-8 encoded.
stringUtf8 :: String -> Builder
{-# inline stringUtf8 #-}
stringUtf8 cs = Builder (\arr off0 len0 s0 -> goString cs arr off0 len0 s0)

-- | Create a builder from a @NUL@-terminated 'CString'. This ignores any
-- textual encoding, copying bytes until @NUL@ is reached.
cstring :: CString -> Builder
{-# inline cstring #-}
cstring (Ptr cs) = Builder (\arr off0 len0 s0 -> goCString cs arr off0 len0 s0)

goString :: String -> MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
{-# noinline goString #-}
goString [] _ off0 _ s0 = (# s0, off0 #)
goString (c : cs) buf off0 len0 s0 = case len0 ># 3# of
  1# -> case unST (UnsafeBounded.pasteST (Bounded.char c) (MutableByteArray buf) (I# off0)) s0 of
    (# s1, I# off1 #) -> goString cs buf off1 (len0 -# (off1 -# off0)) s1
  _ -> (# s0, (-1#) #)

-- We have to have a rule for both unpackCString# and unpackCStringUtf8#
-- since GHC uses a different function based on whether or not non-ASCII
-- codepoints are used in the string.
{-# RULES
"Builder stringUtf8/cstring" forall s a b c d.
  goString (unpackCString# s) a b c d = goCString s a b c d
"Builder stringUtf8/cstring-utf8" forall s a b c d.
  goString (unpackCStringUtf8# s) a b c d = goCString s a b c d
#-}

goCString :: Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
goCString addr buf off0 len0 s0 = case Exts.indexWord8OffAddr# addr 0# of
  0## -> (# s0, off0 #)
  w -> case len0 of
    0# -> (# s0, (-1#) #)
    _ -> case Exts.writeWord8Array# buf off0 w s0 of
      s1 -> goCString (Exts.plusAddr# addr 1# ) buf (off0 +# 1# ) (len0 -# 1# ) s1

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

