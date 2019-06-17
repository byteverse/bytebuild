{-# language TypeApplications #-}

import Data.ByteArray.Builder.Small
import Data.Word
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC
import qualified GHC.Exts as Exts

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ TQC.testProperty "word64Dec" $ \w ->
      run 1 (word64Dec w) === pack (show w)
  , TQC.testProperty "word64Dec-x3" $ \x y z ->
      run 1 (word64Dec x <> word64Dec y <> word64Dec z)
      ===
      pack (show x ++ show y ++ show z)
  ]

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

