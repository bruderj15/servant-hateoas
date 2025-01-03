{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.Resource
(
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

-- | Class for resources that carry Hypermedia-Relations.
class Resource res where
  -- | Wrap a value into a 'Resource'.
  wrap :: a -> res a

  -- | Add a hypermedia relation @(rel, link)@ to a 'Resource'.
  addRel :: (String, RelationLink) -> res a -> res a

-- | Add the self-relation to a 'Resource'.
addSelfRel :: Resource res => RelationLink -> res a -> res a
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
  toResource :: (res ~ MkResource ct, Accept ct) => Proxy res -> Proxy ct -> a -> res a
  default toResource :: (res ~ MkResource ct, Resource res) => Proxy res -> Proxy ct -> a -> res a
  toResource _ _ = wrap

instance Resource res => ToResource res [a]
