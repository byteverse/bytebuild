{-# language MagicHash #-}

module Op
  ( writeCharArray#
  , copyByteArray#
  ) where

import GHC.Exts (writeCharArray#,copyByteArray#,copyMutableByteArray#)
