{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Servant.Hateoas.Resource where

import Servant
import Data.Kind
import Data.Aeson

-- | Type family computing the Resource-Type belonging to this Content-Type.
type family MkResource ct :: (Type -> Type)

-- | Class for resources that carry Hypermedia-Relations.
class Resource res where
  wrap :: a -> res a

  -- | Add a relation @(rel, link)@ to a resource.
  addLink :: (String, Link) -> res a -> res a

-- | Class for 'Resource's that can embed other resources.
class Resource res => EmbeddingResource res where
  -- | Embed a resource @b@ with its relation @rel@ as tuple @(rel, b)@.
  embed :: ToJSON e => (String, e) -> res a -> res a

-- | Class for 'Resource's that can collect multiple resources.
class Resource res => CollectingResource res where
  -- | Collect a resource into the collection.
  collect :: a -> res a -> res a

-- | Class for converting values of @a@ to their respective Resource-Representation.
class ToResource res a where
  toResource :: Proxy res -> a -> res a
  default toResource :: Resource res => Proxy res -> a -> res a
  toResource _ = wrap

instance Resource res => ToResource res [a]
