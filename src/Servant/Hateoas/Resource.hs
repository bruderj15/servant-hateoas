{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Servant.Hateoas.Resource
( Resource(..)
, HasResource(..)
, addRel, addRels
) where

import Servant.Hateoas.ContentType
import Servant.API.ContentTypes
import Servant.Links
import Data.Aeson
import Data.Proxy
import GHC.Generics
import GHC.Exts

-- | RESTy Resource representation of a datatype.
data Resource a = Resource
  { resource :: a
  , links    :: [(String, Link)]
  } deriving (Show, Generic)

-- Add a relation to another resource.
addRel :: (String, Link) -> Resource a -> Resource a
addRel l (Resource x ls) = Resource x (l:ls)
{-# INLINE addRel #-}

-- Add multiple relations to other resources.
addRels :: [(String, Link)] -> Resource a -> Resource a
addRels ls' (Resource x ls) = Resource x $ ls' ++ ls
{-# INLINE addRels #-}

instance ToJSON a => MimeRender HALJSON (Resource a) where
  mimeRender _ (Resource res lks) = encode $ case toJSON res of
    Object kvm -> Object $ ["_links" .= lks'] <> kvm
    v -> v
    where
      lks' = object [fromString rel .= object ["href" .= linkURI href] | (rel, href) <- lks]

-- | Datatypes that can be represented as 'Resource's.
class HasResource api a where
  -- | Creates the resource representation of a value.
  toResource :: Proxy api -> a -> Resource a
