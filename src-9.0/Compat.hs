{-# language MagicHash #-}

module Compat 
  ( int8ToInt#
  , int16ToInt#
  , int32ToInt#
  , wordToWord8# 
  , wordToWord16#
  , wordToWord32#
  , word8ToWord#
  , word16ToWord#
  , word32ToWord#
  ) where


int8ToInt# = id
int16ToInt# = id
int32ToInt# = id
wordToWord8#  = id
wordToWord16# = id
wordToWord32# = id
word8ToWord# = id
word16ToWord# = id
word32ToWord# = id
