{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}
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
module Data.ByteString.Base64.Type (ByteString64(..), module Data.ByteString) where

#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative
import Data.Monoid
#endif

import Data.Aeson
import Data.Binary
import Data.ByteString
import Data.ByteString.Base64 as Base64
import Data.Data
import Data.Serialize
import Data.String
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error
import GHC.Generics

-- | Aeson serialisable bytestring. Uses base64 encoding.
newtype ByteString64 = ByteString64 { getByteString64 :: ByteString }
  deriving (Eq, Show, Ord, Data, Typeable, Generic)

instance ToJSON ByteString64 where
  toJSON = toJSON . decodeUtf8With ignore  . Base64.encode . getByteString64

instance FromJSON ByteString64 where
  parseJSON v = ByteString64 <$> withText "ByteString" (pure . decodeLenient .  encodeUtf8) v

-- | 'ByteString64' is serialised as 'ByteString'
instance Serialize ByteString64

-- | 'ByteString64' is serialised as 'ByteString'
instance Binary ByteString64

instance IsString ByteString64 where
  fromString = ByteString64 . fromString

instance Monoid ByteString64 where
  mempty = ByteString64 mempty
  mappend (ByteString64 a) (ByteString64 b) = ByteString64 (mappend a b)
  mconcat = ByteString64 . mconcat . fmap getByteString64