{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}

module Servant.Hateoas.Resource where

import Servant.Hateoas.ContentType
import Servant.API.ContentTypes
import Data.Aeson
import Data.Proxy
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

class ToResource a api where
  toResource :: Proxy api -> a -> Resource a
  default toResource :: (Generic a, GToResource (Rep a) api) => Proxy api -> a -> Resource a
  toResource api x = Resource x (gToResource api (from x))

class GToResource f api where
  gToResource :: Proxy api -> f p -> [(String, String)]
