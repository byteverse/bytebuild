{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Applicative (liftA2)
import Control.Monad.ST (runST)
import Data.ByteArray.Builder
import Data.Bytes.Types (MutableBytes(MutableBytes))
import Data.Primitive (PrimArray)
import Data.Word
import Data.Char (ord,chr)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Data.Primitive (ByteArray)
import Data.WideWord (Word128(Word128))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.QuickCheck ((===),Arbitrary)
import Text.Printf (printf)
import Test.Tasty.HUnit ((@=?))

import qualified Arithmetic.Nat as Nat
import qualified Data.ByteArray.Builder.Bounded as Bounded
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.List as L
import qualified Data.Primitive as PM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as TQC

import qualified HexWord64
import qualified Word16Tree

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "live"
    [ TQC.testProperty "word64Dec" $ \w ->
        runConcat 1 (word64Dec w) === pack (show w)
    , TQC.testProperty "word64Dec-x3" $ \x y z ->
        runConcat 1 (word64Dec x <> word64Dec y <> word64Dec z)
        ===
        pack (show x ++ show y ++ show z)
    , TQC.testProperty "int64Dec-x3" $ \x y z ->
        runConcat 1 (int64Dec x <> int64Dec y <> int64Dec z)
        ===
        pack (show x ++ show y ++ show z)
    , TQC.testProperty "word64BE-x3" $ \x y z ->
        runConcat 1 (word64BE x <> word64BE y <> word64BE z)
        ===
        pack (LB.unpack (BB.toLazyByteString (BB.word64BE x <> BB.word64BE y <> BB.word64BE z)))
    , TQC.testProperty "word64PaddedUpperHex" $ \w ->
        runConcat 1 (word64PaddedUpperHex w)
        ===
        pack (showWord64PaddedUpperHex w)
    , TQC.testProperty "word16PaddedLowerHex" $ \w ->
        runConcat 1 (word16PaddedLowerHex w)
        ===
        pack (showWord16PaddedLowerHex w)
    , TQC.testProperty "wordPaddedDec2" $ TQC.forAll (TQC.choose (0,99)) $ \w ->
        Bounded.run Nat.two (Bounded.wordPaddedDec2 w)
        ===
        pack (zeroPadL 2 (show w))
    , TQC.testProperty "wordPaddedDec9" $ TQC.forAll (TQC.choose (0,999999999)) $ \w ->
        Bounded.run Nat.constant (Bounded.wordPaddedDec9 w)
        ===
        pack (zeroPadL 9 (show w))
    , TQC.testProperty "word8Dec" $ \w ->
        runConcat 1 (word8Dec w)
        ===
        pack (show w)
    , TQC.testProperty "consLength32BE" $ \w ->
        runConcat 1 (consLength32BE (word8Dec w))
        ===
        pack ('\x00' : '\x00' : '\x00' : chr (L.length (show w)) : show w)
    , TQC.testProperty "consLength64BE-uni" $ \w ->
        pack
          ( '\x00' : '\x00' : '\x00' : '\x00'
          : '\x00' : '\x00' : '\x00' : chr (L.length (show w))
          : show w
          )
        ===
        runConcat 1 (consLength64BE (word16Dec w))
    , TQC.testProperty "consLength64BE-multi" $ \w ->
        pack
          ( '\x00' : '\x00' : '\x00' : '\x00'
          : '\x00' : '\x00' : '\x00' : chr (1 + L.length (show w))
          : '\x42' : show w
          )
        ===
        runConcat 1 (consLength64BE (word8 0x42 <> flush 2 <> word16Dec w))
    , THU.testCase "stringUtf8" $
        packUtf8 "¿Cómo estás? I am doing well." @=?
          runConcat 1 (stringUtf8 "¿Cómo estás? I am doing well.")
    , THU.testCase "doubleDec-A" $
        pack (show (2 :: Int)) @=? runConcat 1 (doubleDec 2.0)
    , THU.testCase "doubleDec-B" $
        pack (show (2.5 :: Double)) @=? runConcat 1 (doubleDec 2.5)
    , THU.testCase "doubleDec-C" $
        pack ("1e+15") @=? runConcat 1 (doubleDec 1e15)
    , THU.testCase "doubleDec-D" $
        pack ("-42") @=? runConcat 1 (doubleDec (-42))
    , THU.testCase "doubleDec-E" $
        pack ("-8.88888888888888e+14") @=? runConcat 1 (doubleDec (-888888888888888.8888888))
    , THU.testCase "doubleDec-F" $
        pack ("42") @=? runConcat 1 (doubleDec 42)
    , THU.testCase "doubleDec-G" $
        pack ("0") @=? runConcat 1 (doubleDec 0)
    , THU.testCase "doubleDec-H" $
        pack ("0.5") @=? runConcat 1 (doubleDec 0.5)
    , THU.testCase "doubleDec-I" $
        pack ("-0.5") @=? runConcat 1 (doubleDec (-0.5))
    , THU.testCase "doubleDec-J" $
        pack ("999999999") @=? runConcat 1 (doubleDec 999999999)
    , THU.testCase "doubleDec-K" $
        pack ("-99999999") @=? runConcat 1 (doubleDec (-99999999))
    , THU.testCase "shortTextJsonString-A" $
        pack ("\"hello\"") @=? runConcat 1 (shortTextJsonString "hello")
    , THU.testCase "shortTextJsonString-B" $
        pack ("\"\\\\_\\\"_/\"") @=? runConcat 1 (shortTextJsonString "\\_\"_/")
    , THU.testCase "shortTextJsonString-C" $
        pack ("\"Hi\\r\\nLo\"") @=? runConcat 1 (shortTextJsonString "Hi\r\nLo")
    , THU.testCase "shortTextJsonString-D" $
        pack ("\"Hi\\u001BLo\"") @=? runConcat 1 (shortTextJsonString "Hi\ESCLo")
    , THU.testCase "word-16-tree" $
        Word16Tree.expectedSmall @=? runConcat 1
          (Word16Tree.encode Word16Tree.exampleSmall)
    , THU.testCase "byteArray-small" $
        let a = replicateByte 3 0x50
            b = replicateByte 5 0x51
         in mconcat [a,b] @=? runConcat 1
              ( byteArray a <> byteArray b )
    , THU.testCase "byteArray-big" $
        let a = replicateByte 2105 0x50
            b = replicateByte 725 0x51
            c = replicateByte 900 0x52
            d = replicateByte 800 0x53
            e = replicateByte 700 0x54
            f = replicateByte 950 0x55
            g = replicateByte 975 0x56
            h = replicateByte 3000 0x57
            i = replicateByte 125 0x58
         in mconcat [a,b,c,d,e,f,g,h,i] @=? runConcat 1
              ( byteArray a <> byteArray b <> byteArray c <>
                byteArray d <> byteArray e <> byteArray f <>
                byteArray g <> byteArray h <> byteArray i
              )
    , TQC.testProperty "word16ArrayLE" $ \(xs :: [Word16]) ->
        let ys = Exts.fromList xs :: PrimArray Word16
         in runConcat 1 (foldMap word16LE xs)
            ===
            runConcat 1 (word16ArrayLE ys 0 (Prelude.length xs))
    , TQC.testProperty "word16ArrayBE" $ \(xs :: [Word16]) ->
        let ys = Exts.fromList xs :: PrimArray Word16
         in runConcat 1 (foldMap word16BE xs)
            ===
            runConcat 1 (word16ArrayBE ys 0 (Prelude.length xs))
    , TQC.testProperty "word32ArrayLE" $ \(xs :: [Word32]) ->
        let ys = Exts.fromList xs :: PrimArray Word32
         in runConcat 1 (foldMap word32LE xs)
            ===
            runConcat 1 (word32ArrayLE ys 0 (Prelude.length xs))
    , TQC.testProperty "word32ArrayBE" $ \(xs :: [Word32]) ->
        let ys = Exts.fromList xs :: PrimArray Word32
         in runConcat 1 (foldMap word32BE xs)
            ===
            runConcat 1 (word32ArrayBE ys 0 (Prelude.length xs))
    , TQC.testProperty "word64ArrayLE" $ \(xs :: [Word64]) ->
        let ys = Exts.fromList xs :: PrimArray Word64
         in runConcat 1 (foldMap word64LE xs)
            ===
            runConcat 1 (word64ArrayLE ys 0 (Prelude.length xs))
    , TQC.testProperty "word64ArrayBE" $ \(xs :: [Word64]) ->
        let ys = Exts.fromList xs :: PrimArray Word64
         in runConcat 1 (foldMap word64BE xs)
            ===
            runConcat 1 (word64ArrayBE ys 0 (Prelude.length xs))
    , TQC.testProperty "word128ArrayLE" $ \(xs :: [Word128]) ->
        let ys = Exts.fromList xs :: PrimArray Word128
         in runConcat 1 (foldMap word128LE xs)
            ===
            runConcat 1 (word128ArrayLE ys 0 (Prelude.length xs))
    , TQC.testProperty "word128ArrayBE" $ \(xs :: [Word128]) ->
        let ys = Exts.fromList xs :: PrimArray Word128
         in runConcat 1 (foldMap word128BE xs)
            ===
            runConcat 1 (word128ArrayBE ys 0 (Prelude.length xs))
    ]
  , testGroup "alternate"
    [ TQC.testProperty "HexWord64" $ \x y ->
        runConcat 1
          (  fromBounded Nat.constant (HexWord64.word64PaddedUpperHex x)
          <> fromBounded Nat.constant (HexWord64.word64PaddedUpperHex y)
          )
        ===
        pack (showWord64PaddedUpperHex x <> showWord64PaddedUpperHex y)
    ]
  , testGroup "putMany"
    [ THU.testCase "A" $ do
        ref <- newIORef []
        let txt = "hello_world_are_you_listening" :: [Char]
        putMany 7 ascii txt (bytesOntoRef ref)
        res <- readIORef ref
        id $
          [ map c2w "hello_"
          , map c2w "world_"
          , map c2w "are_yo"
          , map c2w "u_list"
          , map c2w "ening"
          ] @=? map Exts.toList (Exts.toList res)
    ]
  , testGroup "putManyConsLength"
    [ THU.testCase "A" $ do
        ref <- newIORef []
        let txt = "hello_world_are_you_listening" :: [Char]
        putManyConsLength Nat.constant
          (\n -> Bounded.word16BE (fromIntegral n))
          16 ascii txt (bytesOntoRef ref)
        res <- readIORef ref
        id $
          [ 0x00 : 0x0A : map c2w "hello_worl"
          , 0x00 : 0x0A : map c2w "d_are_you_"
          , 0x00 : 0x09 : map c2w "listening"
          ] @=? map Exts.toList (Exts.toList res)
    ]
  ]

bytesOntoRef ::
     IORef [PM.ByteArray]
  -> MutableBytes Exts.RealWorld
  -> IO ()
bytesOntoRef !ref (MutableBytes buf off len) = do
  rs <- readIORef ref
  dst <- PM.newByteArray len
  PM.copyMutableByteArray dst 0 buf off len
  dst' <- PM.unsafeFreezeByteArray dst
  writeIORef ref (rs ++ [dst'])

replicateByte :: Int -> Word8 -> ByteArray
replicateByte n w = runST $ do
  m <- PM.newByteArray n
  PM.setByteArray m 0 n w
  PM.unsafeFreezeByteArray m

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

packUtf8 :: String -> ByteArray
packUtf8 = Exts.fromList . ByteString.unpack . TE.encodeUtf8 . T.pack

showWord64PaddedUpperHex :: Word64 -> String
showWord64PaddedUpperHex = printf "%016X" 

showWord16PaddedLowerHex :: Word16 -> String
showWord16PaddedLowerHex = printf "%04x" 

runConcat :: Int -> Builder -> ByteArray
runConcat n = Chunks.concatU . run n

c2w :: Char -> Word8
c2w = fromIntegral . ord

instance Arbitrary Word128 where
  arbitrary = liftA2 Word128 TQC.arbitrary TQC.arbitrary

zeroPadL :: Int -> String -> String
zeroPadL n s
  | length s < n = replicate (n - length s) '0' ++ s
  | otherwise = s
