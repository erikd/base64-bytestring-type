{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Trustworthy        #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Base64.Type
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- 'ByteString64' is a strict 'ByteString' serialised with base64 encoding.
 ----------------------------------------------------------------------------
module Data.ByteString.Base64.Type (
    ByteString64(..),
    getEncodedByteString64,
    module Data.ByteString
  ) where

#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative
#endif

import Control.DeepSeq          (NFData)
import Data.Aeson               (ToJSON(..), FromJSON(..), withText)
import Data.Binary              (Binary)
import Data.ByteString
import Data.ByteString.Base64   as Base64
import Data.Data                (Data, Typeable)
import Data.Hashable            (Hashable)
import Data.Semigroup
import Data.Serialize           (Serialize)
import Data.String              (IsString (..))
import Data.Text.Encoding       (decodeLatin1, encodeUtf8)
import GHC.Generics             (Generic)

-- | Aeson serialisable bytestring. Uses base64 encoding.
newtype ByteString64 = ByteString64 { getByteString64 :: ByteString }
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- | Get base64 encode bytestring
getEncodedByteString64 :: ByteString64 -> ByteString
getEncodedByteString64 = Base64.encode . getByteString64

instance ToJSON ByteString64 where
    toJSON = toJSON . decodeLatin1 . getEncodedByteString64

instance FromJSON ByteString64 where
    parseJSON = withText "ByteString" $
        pure . ByteString64 . decodeLenient . encodeUtf8

-- | 'ByteString64' is serialised as 'ByteString'
instance Serialize ByteString64

-- | 'ByteString64' is serialised as 'ByteString'
instance Binary ByteString64

instance IsString ByteString64 where
   fromString = ByteString64 . fromString

instance NFData ByteString64
instance Hashable ByteString64

instance Semigroup ByteString64 where
    ByteString64 a <> ByteString64 b = ByteString64 (a <> b)

instance Monoid ByteString64 where
    mempty = ByteString64 mempty
    mappend = (<>)
