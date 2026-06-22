{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Resource
(
  -- * Resource
  -- ** MkResource
  MkResource,
  MkCollectionResource,
  Responsify,

  -- ** Base Class
  Resource(..),
  addSelfRel,

  -- ** Specialized classes
  EmbeddingResource(..),
  CollectingResource(..),

  -- * Creation
  ToResource(..),

  -- * Response-building
  BuildResource(..),
  BuildCollection(..)
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

-- | Type family computing the collection-Resource-Type holding items of type @a@ for a Content-Type @ct@.
--
-- This is the representation a list-response @[a]@ is mapped to, see 'Responsify'.
type family MkCollectionResource ct (a :: Type) :: Type

-- | Type family computing the response-Resource-Type for a response-value of type @a@ for a Content-Type @ct@.
--
-- A list-response @[a]@ is turned into a collection-resource via 'MkCollectionResource' so that every item
-- carries its own hypermedia-relations as produced by 'ToResource'. Every other response is wrapped via 'MkResource'.
type Responsify :: Type -> Type -> Type
type family Responsify ct a where
  Responsify ct [a] = MkCollectionResource ct a
  Responsify ct a   = MkResource ct a

-- | Class describing how a collection of values @[a]@ is turned into its collection-resource-representation.
class BuildCollection ct a where
  -- | Build the collection-resource from the self-'RelationLink' and the collected items.
  --
  -- Every item should be converted via 'toResource' so it carries its own hypermedia-relations.
  buildCollection :: Proxy ct -> RelationLink -> [a] -> MkCollectionResource ct a

-- | Class describing how a response-value of type @a@ is turned into its response-resource-representation.
class BuildResource ct a where
  -- | Build the response-resource from the self-'RelationLink' and the response-value.
  buildResource :: Proxy ct -> RelationLink -> a -> Responsify ct a

instance {-# OVERLAPPABLE #-}
  ( Responsify ct a ~ MkResource ct a
  , Resource (MkResource ct)
  , ToResource (MkResource ct) a
  , Accept ct
  ) => BuildResource ct a where
  buildResource _ self x = addSelfRel self $ toResource (Proxy @(MkResource ct)) (Proxy @ct) x

instance {-# OVERLAPPING #-} BuildCollection ct a => BuildResource ct [a] where
  buildResource = buildCollection
