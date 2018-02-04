module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty, (===))

import qualified Data.Aeson                           as A
import qualified Data.Binary                          as B
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64.Lazy.Type     as L
import qualified Data.ByteString.Base64.Type          as S
import qualified Data.ByteString.Base64.URL.Lazy.Type as UL
import qualified Data.ByteString.Base64.URL.Type      as US
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Serialize                       as C

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ aeson
  , binary
  , cereal
  ]

aeson :: TestTree
aeson = testGroup "Aeson"
    [ testProperty "strict"     propS
    , testProperty "lazy"       propL
    , testProperty "strict url" propUL
    , testProperty "lazy url"   propUS
    ]
  where
    propS  bs64 = A.decode (A.encode bs64) === Just (bs64 :: S.ByteString64)
    propL  bs64 = A.decode (A.encode bs64) === Just (bs64 :: L.ByteString64)
    propUL bs64 = A.decode (A.encode bs64) === Just (bs64 :: UL.ByteString64)
    propUS bs64 = A.decode (A.encode bs64) === Just (bs64 :: US.ByteString64)

binary :: TestTree
binary = testGroup "Binary"
    [ testProperty "strict"     propS
    , testProperty "lazy"       propL
    , testProperty "strict url" propUL
    , testProperty "lazy url"   propUS
    ]
  where
    propS ws = let bs64 = S.makeByteString64 (BS.pack ws)
               in B.decode (B.encode bs64) === bs64

    propL ws = let bs64 = L.makeByteString64 (LBS.pack ws)
               in B.decode (B.encode bs64) === bs64

    propUS ws = let bs64 = US.makeByteString64 (BS.pack ws)
                in B.decode (B.encode bs64) === bs64

    propUL ws = let bs64 = UL.makeByteString64 (LBS.pack ws)
                in B.decode (B.encode bs64) === bs64

cereal :: TestTree
cereal = testGroup "Cereal"
    [ testProperty "strict"     propS
    , testProperty "lazy"       propL
    , testProperty "strict url" propUL
    , testProperty "lazy url"   propUS
    ]
  where
    propS ws = let bs64 = S.makeByteString64 (BS.pack ws)
               in C.decode (C.encode bs64) === Right bs64

    propL ws = let bs64 = L.makeByteString64 (LBS.pack ws)
               in C.decode (C.encode bs64) === Right bs64

    propUS ws = let bs64 = US.makeByteString64 (BS.pack ws)
                in C.decode (C.encode bs64) === Right bs64

    propUL ws = let bs64 = UL.makeByteString64 (LBS.pack ws)
                in C.decode (C.encode bs64) === Right bs64
