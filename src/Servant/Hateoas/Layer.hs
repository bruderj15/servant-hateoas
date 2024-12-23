{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Layer
(MkLayers, GoLayers, Normalize, module Servant.Hateoas.Layer.Type, module Servant.Hateoas.Layer.Merge, module Servant.Hateoas.Layer.Build)
where

import Servant
import Servant.Hateoas.Layer.Type
import Servant.Hateoas.Layer.Merge
import Servant.Hateoas.Layer.Build
import Servant.Hateoas.Internal.Sym
import Data.Kind

type MkLayers :: p -> [Layer]
type family MkLayers api where
  MkLayers api = MergeLayers (GoLayers (Normalize (Symify api)) '[]) '[]

-- Make api a tree with shared prefixes - making every choice unambiguous
type family Normalize api where
  Normalize ((prefix :> a) :<|> (prefix :> b)) = Normalize (prefix :> (Normalize a :<|> Normalize b))
  Normalize (a :<|> b)                         = Normalize a :<|> Normalize b
  Normalize ((prefix :> a) :>   (prefix :> b)) = Normalize (prefix :> (Normalize a :>   Normalize b))
  Normalize (a :> b)                           = a :> Normalize b
  Normalize a                                  = a

-- Creates all intermediate layers of the api and their immediate children as HATEOAS-endpoints
-- Normalize api before for correctness
-- TODO: Due to branching on (:<|>) it currently returns multiple layers for the same nodeApi - we need to combine them by combining their children
-- Or even better: Solve this by construction
type GoLayers :: p -> [Type] -> [Layer]
type family GoLayers api stand where
  GoLayers (a :<|> b)                 prefix = GoLayers a prefix ++ GoLayers b prefix
  GoLayers (Sym a               :> b) prefix = '[ 'Layer prefix '[Sym a]               GetIntermediate ] ++ GoLayers b (prefix ++ '[Sym a])
  GoLayers (Capture' mods sym a :> b) prefix = '[ 'Layer prefix '[Capture' mods sym a] GetIntermediate ] ++ GoLayers b (prefix ++ '[Capture' mods sym a])
  GoLayers (CaptureAll sym a    :> b) prefix = '[ 'Layer prefix '[CaptureAll    sym a] GetIntermediate ] ++ GoLayers b (prefix ++ '[CaptureAll    sym a])
  GoLayers (a :> b)                   prefix = GoLayers b (prefix ++ '[a])
  GoLayers _ _ = '[]
