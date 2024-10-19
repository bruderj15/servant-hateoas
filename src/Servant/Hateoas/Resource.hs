{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}

module Servant.Hateoas.Resource where

import Servant.Hateoas.ContentType
import Servant.API.ContentTypes
import Data.Aeson
import GHC.Generics
import GHC.Exts

data Resource t a = Resource
  { resource :: a
  , links    :: [(String, String)]
  } deriving (Generic)

instance ToJSON a => ToJSON (Resource HALJSON a) where
  toJSON (Resource res lks) = case toJSON res of
    Object kvm -> Object $ ["_links" .= lks'] <> kvm
    v -> v
    where
      lks' = object [fromString rel .= object ["href" .= href] | (rel, href) <- lks]

-- TODO:
-- instance ToJSON a => ToJSON (Resource CollectionJSON a) where
-- instance FromJSON a => FromJSON (Resource HALJSON a) where
-- instance FromJSON a => FromJSON (Resource CollectionJSON a) where

instance (ToJSON (Resource t a), Accept t) => MimeRender t (Resource t a) where
  mimeRender _ = encode

instance (FromJSON (Resource t a), Accept t) => MimeUnrender t (Resource t a) where
  mimeUnrender _ = eitherDecode

class ToResource a where
  toResource :: a -> Resource t a
  default toResource :: (Generic a, GToResource (Rep a)) => a -> Resource t a
  toResource x = Resource x (gToResource (from x))

class GToResource f where
  gToResource :: f p -> [(String, String)]
