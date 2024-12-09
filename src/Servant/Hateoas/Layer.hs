{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Layer where

import Servant
import Data.Kind
import Control.Monad.IO.Class

data Layer = Layer
  { nodeApi      :: Type      -- relative path from host: e.g. /api/users
  , childrenApis :: [Type]    -- immediate relative children paths from host: e.g. [/api/users/1]
  }

type family NodeApi (a :: Layer) where
  NodeApi ('Layer api _) = api

type family ChildrenApis (a :: Layer) where
  ChildrenApis ('Layer _ children) = children

type (++) xs ys = AppendList xs ys

-- Wrapping api in: Bottom :> api :> Top, so api has kind k and not Type.
-- This is crucial so we can match paths (:: Symbol) and potential other-kinded combinators
data Bottom
data Top

-- Creates all intermediate layers of the api and their immediate children
type Layers :: p -> q -> [Layer]
type family Layers api stand where
  Layers (a :<|> b)  Bottom                   = Layers a Bottom ++ Layers b Bottom
  Layers (a :<|> b) (Bottom :> prefix :> Top) = Layers a (Bottom :> prefix :> Top) ++ Layers b (Bottom :> prefix :> Top)
  Layers (a :> b)    Bottom                   = '[ 'Layer  Bottom                   (FirstPath a Bottom) ] ++ Layers b (Bottom           :> a :> Top)
  Layers (a :> b)   (Bottom :> prefix :> Top) = '[ 'Layer (Bottom :> prefix :> Top) (FirstPath a prefix) ] ++ Layers b (Bottom :> prefix :> a :> Top)
  Layers _ _                                  = '[]

-- Interpreting api as a tree returning the first layers of the tree
type FirstPath :: p -> q -> [Type]
type family FirstPath api prefix where
  FirstPath (a :<|> b) prefix = FirstPath a prefix ++ FirstPath b prefix
  FirstPath (a :> _)   Bottom = '[Bottom           :> a :> Top]
  FirstPath (a :> _)   prefix = '[Bottom :> prefix :> a :> Top]
  FirstPath a          Bottom = '[Bottom           :> a :> Top]
  FirstPath a          prefix = '[Bottom :> prefix :> a :> Top]

class HasLayerServer (l :: Layer) server m ct where
  getLayerServer :: (MonadIO m, ServerT (NodeApi l) m ~ server)
    => Proxy m -> Proxy ct -> Proxy l -> Proxy server -> ServerT (NodeApi l) m
