{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

import Control.Monad.ST (runST)
import Data.Bytes.Types (MutableBytes(..))
import Data.ByteArray.Builder
import Data.Word
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.QuickCheck ((===))
import Text.Printf (printf)
import Test.Tasty.HUnit ((@=?))

import qualified Arithmetic.Nat as Nat
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.List as L
import qualified Data.Primitive as PM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as TQC

import qualified HexWord64

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "live"
    [ TQC.testProperty "word64Dec" $ \w ->
        run 1 (word64Dec w) === pack (show w)
    , TQC.testProperty "word64Dec-x3" $ \x y z ->
        run 1 (word64Dec x <> word64Dec y <> word64Dec z)
        ===
        pack (show x ++ show y ++ show z)
    , TQC.testProperty "int64Dec-x3" $ \x y z ->
        run 1 (int64Dec x <> int64Dec y <> int64Dec z)
        ===
        pack (show x ++ show y ++ show z)
    , TQC.testProperty "word64BE-x3" $ \x y z ->
        run 1 (word64BE x <> word64BE y <> word64BE z)
        ===
        pack (LB.unpack (BB.toLazyByteString (BB.word64BE x <> BB.word64BE y <> BB.word64BE z)))
    , TQC.testProperty "word64PaddedUpperHex" $ \w ->
        run 1 (word64PaddedUpperHex w)
        ===
        pack (showWord64PaddedUpperHex w)
    , TQC.testProperty "pasteArrayST" $ \(xs :: [Word64]) ->
        (runArray word64Dec (V.fromList xs))
        ===
        pack (foldMap show xs)
    , THU.testCase "stringUtf8" $
        packUtf8 "¿Cómo estás? I am doing well." @=?
          run 1 (stringUtf8 "¿Cómo estás? I am doing well.")
    , THU.testCase "doubleDec-A" $
        pack (show (2 :: Int)) @=? run 1 (doubleDec 2.0)
    , THU.testCase "doubleDec-B" $
        pack (show (2.5 :: Double)) @=? run 1 (doubleDec 2.5)
    , THU.testCase "doubleDec-C" $
        pack ("1e+15") @=? run 1 (doubleDec 1e15)
    , THU.testCase "doubleDec-D" $
        pack ("-42") @=? run 1 (doubleDec (-42))
    , THU.testCase "doubleDec-E" $
        pack ("-8.88888888888888e+14") @=? run 1 (doubleDec (-888888888888888.8888888))
    , THU.testCase "doubleDec-F" $
        pack ("42") @=? run 1 (doubleDec 42)
    , THU.testCase "doubleDec-G" $
        pack ("0") @=? run 1 (doubleDec 0)
    , THU.testCase "doubleDec-H" $
        pack ("0.5") @=? run 1 (doubleDec 0.5)
    , THU.testCase "doubleDec-I" $
        pack ("-0.5") @=? run 1 (doubleDec (-0.5))
    , THU.testCase "doubleDec-J" $
        pack ("999999999") @=? run 1 (doubleDec 999999999)
    , THU.testCase "doubleDec-K" $
        pack ("-99999999") @=? run 1 (doubleDec (-99999999))
    , THU.testCase "shortTextJsonString-A" $
        pack ("\"hello\"") @=? run 1 (shortTextJsonString "hello")
    , THU.testCase "shortTextJsonString-B" $
        pack ("\"\\\\_\\\"_/\"") @=? run 1 (shortTextJsonString "\\_\"_/")
    , THU.testCase "shortTextJsonString-C" $
        pack ("\"Hi\\r\\nLo\"") @=? run 1 (shortTextJsonString "Hi\r\nLo")
    , THU.testCase "shortTextJsonString-D" $
        pack ("\"Hi\\u001BLo\"") @=? run 1 (shortTextJsonString "Hi\ESCLo")
    ]
  , testGroup "alternate"
    [ TQC.testProperty "HexWord64" $ \x y ->
        run 1
          (  fromBounded Nat.constant (HexWord64.word64PaddedUpperHex x)
          <> fromBounded Nat.constant (HexWord64.word64PaddedUpperHex y)
          )
        ===
        pack (showWord64PaddedUpperHex x <> showWord64PaddedUpperHex y)
    ]
  ]

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

packUtf8 :: String -> ByteArray
packUtf8 = Exts.fromList . ByteString.unpack . TE.encodeUtf8 . T.pack

-- This is used to test pasteArrayST
runArray ::
     (a -> Builder) -- ^ Builder
  -> V.Vector a -- ^ Elements to serialize
  -> ByteArray -- ^ Number of elements serialized, serialization
runArray f !xs = runST $ do
  let go !v0 !sz !chunks = if V.null v0
        then pure (mconcat (L.reverse chunks))
        else do
          arr <- PM.newByteArray sz
          (v1,MutableBytes _ off _) <- pasteArrayST (MutableBytes arr 0 sz) f v0
          -- If nothing was serialized, we need a bigger buffer
          let szNext = if V.length v0 == V.length v1 then sz + 1 else sz
          c <- PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray arr off
          go v1 szNext (c : chunks)
  go xs 1 []

showWord64PaddedUpperHex :: Word64 -> String
showWord64PaddedUpperHex = printf "%016X" 
