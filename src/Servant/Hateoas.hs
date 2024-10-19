{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}

module Servant.Hateoas where

import Servant.API.ContentTypes
import Servant
import qualified Network.HTTP.Media as M
import Data.Aeson
import GHC.Generics
import GHC.Exts

data HALJSON
-- data CollectionJSON

instance Accept HALJSON where
  contentType _ = "application" M.// "hal+json"

-- instance Accept HALJSON where
  -- contentType _ = "application" M.// "hal+json"

data HALResource a = HALResource
  { resource :: a
  , links    :: [(String, String)]
  } deriving (Generic)

instance ToJSON a => ToJSON (HALResource a) where
  toJSON (HALResource res lks) = case toJSON res of
    Object kvm -> Object $ ["_links" .= lks'] <> kvm
    v -> v
    where
      lks' = object [ fromString rel .= object ["href" .= href] | (rel, href) <- lks ]

instance ToJSON a => MimeRender HALJSON (HALResource a) where
  mimeRender _ = encode

class ToHAL a where
  toHAL :: a -> HALResource a
  default toHAL :: (Generic a, GToHAL (Rep a)) => a -> HALResource a
  toHAL x = HALResource x (gToHAL (from x))

class GToHAL f where
  gToHAL :: f p -> [(String, String)]
















-------------- Example --------------
data User = User { userId :: Int, addressId :: Int } deriving (Generic, Show, Eq, Ord)
instance ToJSON User
instance ToHAL User where
  toHAL u@(User _ addrId) = HALResource u userLinks
    where
      userLinks = pure ("address", "http://host:port/addresses/" <> show addrId)

type UserApi = "user" :> Capture "id" Int :> Get '[HALJSON] (HALResource User)
userServer :: Server UserApi
userServer = \i -> return $ toHAL $ User i 42
