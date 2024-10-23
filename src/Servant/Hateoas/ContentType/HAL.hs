{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ContentType.HAL where

import Servant.Hateoas.Resource
import Servant.Hateoas.Some
import Servant.API.ContentTypes
import qualified Network.HTTP.Media as M
import Servant.Links
import Data.Kind
import Data.Aeson
import Data.Aeson.KeyMap (singleton)
import GHC.Exts

data HAL (a :: Type)

data HALResource a = HALResource
  { resource :: a
  , links    :: [(String, Link)]
  , embedded :: [(String, SomeToJSON HALResource)]
  }

instance HasResource (HAL JSON) where
  type Resource (HAL JSON) = HALResource

instance Accept (HAL JSON) where
  contentType _ = "application" M.// "hal+json"

instance ToJSON a => ToJSON (HALResource a) where
  toJSON (HALResource res ls es) = case toJSON res of
    Object kvm -> Object $ (singleton "_links" ls') <> (singleton "_embedded" es') <> kvm
    v -> v
    where
      ls' = object [fromString rel .= object ["href" .= linkURI href] | (rel, href) <- ls]
      es' = object [fromString name .= toJSON e | (name, (SomeToJSON e)) <- es]

instance ToJSON (HALResource a) => MimeRender (HAL JSON) (HALResource a) where
  mimeRender _ = encode
