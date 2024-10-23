{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ContentType.HALJSON where

import Servant.Hateoas.Resource
import Servant.Hateoas.Some
import Servant.API.ContentTypes
import qualified Network.HTTP.Media as M
import Servant.Links
import Data.Aeson
import Data.Aeson.KeyMap (singleton)
import GHC.Exts

data HALJSON

data HALResource a = HALResource
  { resource :: a
  , links    :: [(String, Link)]
  , embedded :: [(String, SomeToJSON HALResource)]
  }

instance HasResource HALJSON a where
  type Resource HALJSON = HALResource

instance Accept HALJSON where
  contentType _ = "application" M.// "hal+json"

instance ToJSON a => ToJSON (HALResource a) where
  toJSON (HALResource res ls es) = case toJSON res of
    Object kvm -> Object $ (singleton "_links" ls') <> (singleton "_embedded" es') <> kvm
    v -> v
    where
      ls' = object [fromString rel .= object ["href" .= linkURI href] | (rel, href) <- ls]
      es' = object [fromString name .= toJSON e | (name, (SomeToJSON e)) <- es]

instance ToJSON (HALResource a) => MimeRender HALJSON (HALResource a) where
  mimeRender _ = encode
