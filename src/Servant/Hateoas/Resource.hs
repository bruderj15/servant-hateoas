{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.Resource
(
  -- * ResourceLink
  ResourceLink(..),

  -- * Resource
  -- ** MkResource
  MkResource,

  -- ** Base Class
  Resource(..),
  addSelfRel,

  -- ** Specialized classes
  EmbeddingResource(..),
  CollectingResource(..),

  -- * Creation
  ToResource(..)
) where

import Servant
import Servant.Hateoas.RelationLink
import Data.Kind
import Data.Aeson

-- | Type family computing the Resource-Type belonging to this Content-Type.
type family MkResource ct :: (Type -> Type)

-- | Type for Hypermedia-Links.
--
data ResourceLink =
    CompleteLink Link               -- ^ A complete 'Link' with all information.
  | TemplateLink RelationLink       -- ^ A 'RelationLink' that can be used as a template for URIs.
  deriving (Show)

instance ToJSON ResourceLink where
  toJSON (CompleteLink l) = let uri = linkURI l in toJSON $ uri { uriPath = "/" <> uriPath uri }
  toJSON (TemplateLink l) = toJSON l

-- | Class for resources that carry Hypermedia-Relations.
class Resource res where
  -- | Wrap a value into a 'Resource'.
  wrap :: a -> res a

  -- | Add a hypermedia relation @(rel, link)@ to a 'Resource'.
  addRel :: (String, ResourceLink) -> res a -> res a

-- | Add the self-relation to a 'Resource'.
addSelfRel :: Resource res => ResourceLink -> res a -> res a
addSelfRel l = addRel ("self", l)

-- | Class for 'Resource's that can embed other resources.
class Resource res => EmbeddingResource res where
  -- | Embed a resource @b@ with its relation @rel@ as tuple @(rel, b)@.
  embed :: ToJSON e => (String, res e) -> res a -> res a

-- | Class for 'Resource's that can collect multiple resources.
class Resource res => CollectingResource res where
  -- | Collect a resource into the collection.
  collect :: a -> res a -> res a

-- | Class for converting values of @a@ to their respective Resource-Representation.
--
-- The default implementation wraps the value into a 'Resource' without adding any further information.
-- Therefore you can derive an instance for this class.
class ToResource res a where
  -- | Describes how a value @a@ can be converted to a 'Resource'.
  toResource :: Proxy res -> a -> res a
  default toResource :: Resource res => Proxy res -> a -> res a
  toResource _ = wrap

instance Resource res => ToResource res [a]
