{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Lazy 'ByteString' base64 encoding with URL and filename safe alphabet.
--
-- See <https://tools.ietf.org/html/rfc4648>.
module Data.ByteString.Base64.URL.Lazy.Type (
    ByteString64,
    makeByteString64,
    getByteString64,
    mkBS64,
    getBS64,
    getEncodedByteString64,
  ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq         (NFData (..))
import Data.Aeson
       (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), withText)
import Data.Aeson.Types        (FromJSONKeyFunction (..), toJSONKeyText)
import Data.Binary             (Binary (..))
import Data.ByteString.Lazy    (ByteString, pack, unpack)
import Data.Data               (Data)
import Data.Hashable           (Hashable)
import Data.Semigroup          (Semigroup (..))
import Data.String             (IsString (..))
import Data.Text.Lazy          (fromStrict, toStrict)
import Data.Text.Lazy.Encoding (decodeLatin1, encodeUtf8)
import GHC.Generics            (Generic)
import Test.QuickCheck
       (Arbitrary (..), CoArbitrary (..), Function (..), functionMap,
       shrinkMap)

import qualified Data.ByteString.Base64.URL.Lazy as Base64

#ifdef MIN_VERSION_cereal
import Data.Serialize (Serialize)
#endif

#ifdef MIN_VERSION_serialise
import Codec.Serialise (Serialise (..))
#endif

-- | Aeson serialisable bytestring. Uses base64 encoding.
--
-- The inner 'ByteString' is in raw format.
--
-- >>> let bs64 = makeByteString64 "foobar"
-- >>> bs64
-- mkBS64 "foobar"
--
-- 'Binary' instance doesn't use base64 encoding:
--
-- >>> Binary.encode bs64
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKfoobar"
--
-- 'Aeson' instance does:
--
-- >>> Aeson.encode bs64
-- "\"Zm9vYmFy\""
--
-- This module uses standard alphabet
--
-- >>> Aeson.encode (makeByteString64 "aa\191")
-- "\"YWG_\""
--
newtype ByteString64 = BS64 ByteString
    deriving (Eq, Ord, Data, Generic)

instance Show ByteString64 where
    showsPrec d (BS64 bs) = showParen (d > 10) $ showString "mkBS64 " . showsPrec 11 bs

-- | Wrap 'ByteString' into 'ByteString64'. Essentially 'coerce'.
makeByteString64 :: ByteString -> ByteString64
makeByteString64 = BS64

-- | Shorter variant of 'makeByteString64'
mkBS64 :: ByteString -> ByteString64
mkBS64 = makeByteString64

-- | Unwrap 'ByteString' from 'ByteString64'. Essentially 'coerce'.
getByteString64 :: ByteString64 -> ByteString
getByteString64 = \(BS64 bs) -> bs

--  | Shorter variant of 'getByteString64'
getBS64 :: ByteString64 -> ByteString
getBS64 = \(BS64 bs) -> bs

-- | Get base64 encode bytestring
--
-- >>> getEncodedByteString64 "foobar"
-- "Zm9vYmFy"
--
-- >>> getEncodedByteString64 "aa\191"
-- "YWG_"
--
getEncodedByteString64 :: ByteString64 -> ByteString
getEncodedByteString64 = Base64.encode . getBS64

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance IsString ByteString64 where
   fromString = BS64 . fromString

instance Semigroup ByteString64 where
    BS64 a <> BS64 b = BS64 (a <> b)

instance Monoid ByteString64 where
    mempty = BS64 mempty
    mappend = (<>)

instance NFData ByteString64 where
    rnf = rnf' . getBS64 where
#if MIN_VERSION_bytestring(0,10,0)
        rnf' = rnf
#else
        rnf' Empty       = ()
        rnf' (Chunk _ b) = rnf' b
#endif

instance Hashable ByteString64

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance ToJSON ByteString64 where
    toJSON = toJSON . decodeLatin1 . getEncodedByteString64
    toEncoding = toEncoding . decodeLatin1 . getEncodedByteString64

instance FromJSON ByteString64 where
    parseJSON = withText "ByteString" $
        either fail (pure . BS64) . Base64.decode . encodeUtf8 . fromStrict

instance ToJSONKey ByteString64 where
    toJSONKey = toJSONKeyText (toStrict . decodeLatin1 . getEncodedByteString64)

instance FromJSONKey ByteString64 where
    fromJSONKey = FromJSONKeyTextParser $
        either fail (pure . BS64) . Base64.decode . encodeUtf8 . fromStrict

-------------------------------------------------------------------------------
-- cereal
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_cereal
-- | 'ByteString64' is serialised as 'ByteString'
--
-- >>> Cereal.encode (mkBS64 "foobar")
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKfoobar"
instance Serialize ByteString64
#endif

-------------------------------------------------------------------------------
-- binary
-------------------------------------------------------------------------------

-- | 'ByteString64' is serialised as 'ByteString'
--
-- >>> Binary.encode (mkBS64 "foobar")
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKfoobar"
instance Binary ByteString64 where
    put = put . getBS64
    get = fmap makeByteString64 get

-------------------------------------------------------------------------------
-- serialise
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_serialise
-- | >>> Serialise.serialise (mkBS64 "xyzzy")
-- "_Exyzzy\255"
instance Serialise ByteString64 where
    encode = encode . getBS64
    decode = fmap makeByteString64 decode
#endif

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

instance Arbitrary ByteString64 where
    arbitrary = BS64 . pack <$> arbitrary
    shrink = shrinkMap (BS64 . pack) (unpack . getBS64)

instance CoArbitrary ByteString64 where
    coarbitrary = coarbitrary . unpack . getBS64

instance Function ByteString64 where
    function = functionMap (unpack . getBS64) (BS64 . pack)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Codec.Serialise as Serialise
-- >>> import qualified Data.Serialize as Cereal
-- >>> import qualified Data.Binary as Binary
-- >>> import qualified Data.Aeson as Aeson
