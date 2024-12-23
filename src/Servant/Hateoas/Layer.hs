{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Layer
(
  -- * Create
  MkLayers,

  -- * Type
  module Servant.Hateoas.Layer.Type,

  -- * Build
  module Servant.Hateoas.Layer.Build,

  -- * Merge
  module Servant.Hateoas.Layer.Merge,

  -- * Utilities
  GoLayers,
  Normalize,
)
where

import Servant
import Servant.Hateoas.Layer.Type
import Servant.Hateoas.Layer.Merge
import Servant.Hateoas.Layer.Build
import Servant.Hateoas.Internal.Sym
import Data.Kind

-- | Given an API, create a list of 'Layer's that represents the HATEOAS structure of the API.
--
-- This is done by:
--
-- 1. Rewriting every @(sym :: Symbol) :> b@ into @Sym sym :> b@ so all subsets of the API have kind 'Type', see 'Symify'.
--
-- 2. Normalizing the API to make every node-choice unambiguous, see 'Normalize'.
--
-- 3. Creating all intermediate layers of the API and their immediate next layers, see 'GoLayers'.
--
-- 4. Merging the immediate next layers of all layers that share the same API, see 'MergeLayers'.
--
type MkLayers :: p -> [Layer]
type family MkLayers api where
  MkLayers api = MergeLayers (GoLayers (Normalize (Symify api)) '[]) '[]

-- | Rewrite the API pulling out every shared prefix between all branches of the API.
--
-- This results in a tree-structure where every path from the root of the API to every node or leaf is unique.
type family Normalize api where
  Normalize ((prefix :> a) :<|> (prefix :> b)) = Normalize (prefix :> (Normalize a :<|> Normalize b))
  Normalize (a :<|> b)                         = Normalize a :<|> Normalize b    -- | TODO: Here we also need to apply 'Normalize' again! How?
  Normalize ((prefix :> a) :>   (prefix :> b)) = Normalize (prefix :> (Normalize a :>   Normalize b))
  Normalize (a :> b)                           = a :> Normalize b
  Normalize a                                  = a

-- | Creates all intermediate layers of the API and their immediate next layers.
--
-- This is done by traversing the API-Tree where in @GoLayers api stand@, @api@ is the API seen below from @stand@.
--
-- Argument @stand@ therefore also represents an API, but due to the right-associative nature of ':>' we model it as a list here,
-- which can be turned into an API by folding it with ':>', see 'MkPrefix'.
type GoLayers :: p -> [Type] -> [Layer]
type family GoLayers api stand where
  GoLayers (a :<|> b)                 prefix = GoLayers a prefix ++ GoLayers b prefix
  GoLayers (Sym a               :> b) prefix = '[ 'Layer prefix '[Sym a]               GetIntermediate ] ++ GoLayers b (prefix ++ '[Sym a])
  GoLayers (Capture' mods sym a :> b) prefix = '[ 'Layer prefix '[Capture' mods sym a] GetIntermediate ] ++ GoLayers b (prefix ++ '[Capture' mods sym a])
  GoLayers (CaptureAll sym a    :> b) prefix = '[ 'Layer prefix '[CaptureAll    sym a] GetIntermediate ] ++ GoLayers b (prefix ++ '[CaptureAll    sym a])
  GoLayers (a :> b)                   prefix = GoLayers b (prefix ++ '[a])
  GoLayers _ _ = '[]
