{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ContentType where

import Servant.API.ContentTypes
import qualified Network.HTTP.Media as M

data HALJSON
data CollectionJSON
data HALFormsJSON
data UberJSON
data SirenJSON

instance Accept HALJSON where
  contentType _ = "application" M.// "hal+json"

instance Accept CollectionJSON where
  contentType _ = "application" M.// "vnd.collection+json"

instance Accept HALFormsJSON where
  contentType _ = "application" M.// "prs.hal-forms+json"

instance Accept UberJSON where
  contentType _ = "application" M.// "vnd.amundsen-uber+json"

instance Accept SirenJSON where
  contentType _ = "application" M.// "vnd.siren+json"
