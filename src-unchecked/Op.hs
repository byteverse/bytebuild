{-# language MagicHash #-}

module Op
  ( writeCharArray#
  , copyByteArray#
  , copyMutableByteArray#
  ) where

import GHC.Exts (copyMutableByteArray#,writeCharArray#,copyByteArray#,copyMutableByteArray#)
