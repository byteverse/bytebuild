module Op
  ( writeCharArray#
  ) where

import GHC.Exts ((<#),(>=#))
import qualified GHC.Exts as Exts

writeCharArray# :: Exts.MutableByteArray# s -> Exts.Int# -> Exts.Char# -> Exts.State# s -> Exts.State# s 
writeCharArray# arr i v st = case i <# 0# of
  1# -> error ("writeCharArray#: negative index " ++ show (I# i))
  _ -> case Exts.getSizeofMutableByteArray# arr st of
    (# st', sz #) -> case i >=# sz of
      1# -> error ("writeCharArray#: index " ++ show (I# i) ++ " >= length " ++ show (I# sz))
      _ -> Exts.writeInt8Array# arr i (Exts.intToInt8# v) st'
