module Main (main) where

import qualified Data.Aeson as A
import qualified Data.Binary as B
import qualified Data.Serialize as C

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import           Data.ByteString.Base64.Type

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ aeson
  , binary
  , cereal
  ]

aeson :: TestTree
aeson = QC.testProperty "Aeson" prop
  where prop ws = let bs64 = ByteString64 (pack ws)
                 in A.decode (A.encode [bs64]) === Just [bs64]

binary :: TestTree
binary = QC.testProperty "Binary" prop
  where prop ws = let bs64 = ByteString64 (pack ws)
                  in B.decode (B.encode bs64) === bs64

cereal :: TestTree
cereal = QC.testProperty "Cereal" prop
  where prop ws = let bs64 = ByteString64 (pack ws)
                  in C.decode (C.encode bs64) === Right bs64
