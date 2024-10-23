{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ContentType
( HALJSON
, CollectionJSON
, HALFormsJSON
, UberJSON
, SirenJSON
)
where

import Servant.Hateoas.Resource
import Servant.API.ContentTypes
import qualified Network.HTTP.Media as M
import Servant.Links
import Data.Aeson
import Data.Aeson.KeyMap (singleton)
import GHC.Exts

data HALJSON
data CollectionJSON
data HALFormsJSON
data UberJSON
data SirenJSON

instance Accept HALJSON where
  contentType _ = "application" M.// "hal+json"

instance ToJSON a => MimeRender HALJSON (Resource a) where
  mimeRender _ (Resource res lks) = encode $ case toJSON res of
    Object kvm -> Object $ (singleton "_links" lks') <> kvm
    v -> v
    where
      lks' = object [fromString rel .= object ["href" .= linkURI href] | (rel, href) <- lks]

instance Accept CollectionJSON where
  contentType _ = "application" M.// "vnd.collection+json"

instance Accept HALFormsJSON where
  contentType _ = "application" M.// "prs.hal-forms+json"

instance Accept UberJSON where
  contentType _ = "application" M.// "vnd.amundsen-uber+json"

instance Accept SirenJSON where
  contentType _ = "application" M.// "vnd.siren+json"
