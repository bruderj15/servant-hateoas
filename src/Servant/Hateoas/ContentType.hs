{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ContentType
( CollectionJSON
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

data CollectionJSON
data HALFormsJSON
data UberJSON
data SirenJSON

instance Accept CollectionJSON where
  contentType _ = "application" M.// "vnd.collection+json"

instance Accept HALFormsJSON where
  contentType _ = "application" M.// "prs.hal-forms+json"

instance Accept UberJSON where
  contentType _ = "application" M.// "vnd.amundsen-uber+json"

instance Accept SirenJSON where
  contentType _ = "application" M.// "vnd.siren+json"
