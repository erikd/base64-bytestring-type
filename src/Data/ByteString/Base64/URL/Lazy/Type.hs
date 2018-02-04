{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Lazy 'ByteString' base64 encoding with URL and filename safe alphabet.
--
-- See <https://tools.ietf.org/html/rfc4648>.
module Data.ByteString.Base64.URL.Lazy.Type (
    ByteString64,
    makeByteString64,
    getByteString64,
    getEncodedByteString64,
  ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq         (NFData (..))
import Data.Aeson              (FromJSON (..), ToJSON (..), withText)
import Data.Binary             (Binary (..))
import Data.ByteString.Lazy    (ByteString, fromStrict)
import Data.Data               (Data, Typeable)
import Data.Hashable           (Hashable)
import Data.Semigroup          (Semigroup (..))
import Data.Serialize          (Serialize)
import Data.String             (IsString (..))
import Data.Text.Encoding      (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeLatin1)
import GHC.Generics            (Generic)

import qualified Data.ByteString.Base64.URL.Lazy as Base64

-- | Aeson serialisable bytestring. Uses base64 encoding.
--
-- The inner 'ByteString' is in raw format.
--
-- >>> let bs64 = makeByteString64 "foobar"
-- >>> bs64
-- BS64 "foobar"
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
-- This module uses URL and filename safe alphabet
--
-- >>> Aeson.encode (makeByteString64 "aa\191")
-- "\"YWG_\""
--
newtype ByteString64 = BS64 ByteString
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Wrap 'ByteString' into 'ByteString64'. Essentially 'coerce'.
makeByteString64 :: ByteString -> ByteString64
makeByteString64 = BS64

-- | Unwrap 'ByteString' from 'ByteString64'. Essentially 'coerce'.
getByteString64 :: ByteString64 -> ByteString
getByteString64 = \(BS64 bs) -> bs

-- | Get base64 encode bytestring
--
-- >>> getEncodedByteString64 "foobar"
-- "Zm9vYmFy"
--
-- >>> getEncodedByteString64 "aa\191"
-- "YWG_"
--
getEncodedByteString64 :: ByteString64 -> ByteString
getEncodedByteString64 = Base64.encode . getByteString64

instance ToJSON ByteString64 where
    toJSON = toJSON . decodeLatin1 . getEncodedByteString64
    toEncoding = toEncoding . decodeLatin1 . getEncodedByteString64

instance FromJSON ByteString64 where
    parseJSON = withText "ByteString" $
        either fail (pure . BS64) . Base64.decode . fromStrict . encodeUtf8

-- | 'ByteString64' is serialised as 'ByteString'
instance Serialize ByteString64

-- | 'ByteString64' is serialised as 'ByteString'
instance Binary ByteString64 where
    put = put . getByteString64
    get = fmap makeByteString64 get

instance IsString ByteString64 where
   fromString = BS64 . fromString

instance NFData ByteString64 where rnf = rnf . getByteString64
instance Hashable ByteString64

instance Semigroup ByteString64 where
    BS64 a <> BS64 b = BS64 (a <> b)

instance Monoid ByteString64 where
    mempty = BS64 mempty
    mappend = (<>)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Binary as Binary
-- >>> import qualified Data.Aeson as Aeson
