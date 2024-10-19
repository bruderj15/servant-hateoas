{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}

module Servant.Hateoas.Resource where

import Servant.Hateoas.ContentType
import Servant.API.ContentTypes
import Data.Aeson
import GHC.Generics
import GHC.Exts

-- Later add actions and more props for other MimeTypes like CollectionJSON, Hydra, ...
data Resource a = Resource
  { resource :: a
  , links    :: [(String, String)]
  } deriving (Generic)

instance ToJSON a => MimeRender HALJSON (Resource a) where
  mimeRender _ (Resource res lks) = encode $ case toJSON res of
    Object kvm -> Object $ ["_links" .= lks'] <> kvm
    v -> v
    where
      lks' = object [fromString rel .= object ["href" .= href] | (rel, href) <- lks]

class ToResource a where
  toResource :: a -> Resource a
  default toResource :: (Generic a, GToResource (Rep a)) => a -> Resource a
  toResource x = Resource x (gToResource (from x))

class GToResource f where
  gToResource :: f p -> [(String, String)]
