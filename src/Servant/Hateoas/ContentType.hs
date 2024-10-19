{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ContentType where

import Servant.API.ContentTypes
import qualified Network.HTTP.Media as M

data HALJSON
data CollectionJSON

instance Accept HALJSON where
  contentType _ = "application" M.// "hal+json"

instance Accept CollectionJSON where
  contentType _ = "application" M.// "vnd.collection+json"
