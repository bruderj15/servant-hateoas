{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Layer where

import Servant
import Servant.Hateoas.Resource
import Data.Kind
import Control.Monad.IO.Class
import GHC.TypeLits

data Layer = Layer
  { nodeApi      :: Type      -- relative path from host: e.g. /api/users
  , childrenApis :: [Type]    -- immediate relative children paths from host: e.g. [/api/users/1]
  }

type family NodeApi (a :: Layer) where
  NodeApi ('Layer api _) = api

type family ChildrenApis (a :: Layer) where
  ChildrenApis ('Layer _ children) = children

instance HasServer api context => HasServer ('Layer api cs) context where
  type ServerT ('Layer api cs) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

type (++) xs ys = AppendList xs ys

-- Wrapping api in: Bottom :> api :> Top, so api has kind k and not Type.
-- This is crucial so we can match paths (:: Symbol) and potential other-kinded combinators
data Bottom
data Top

-- Creates all intermediate layers of the api and their immediate children as HATEOAS-endpoints
-- Normalize api before for correctness
type Layers :: p -> q -> [Layer]
type family Layers api stand where
  Layers (a :<|> b)  Bottom                   = Layers a Bottom ++ Layers b Bottom
  Layers (a :<|> b) (Bottom :> prefix :> Top) = Layers a (Bottom :> prefix :> Top) ++ Layers b (Bottom :> prefix :> Top)
  Layers (a :> b)    Bottom                   = '[ 'Layer            ()  (FirstPath a Bottom) ] ++ Layers b (Bottom           :> a :> Top)
  Layers (a :> b)   (Bottom :> prefix :> Top) = '[ 'Layer (prefix :> ()) (FirstPath a prefix) ] ++ Layers b (Bottom :> prefix :> a :> Top)
  Layers _ _                                  = '[]

-- Interpreting api as a tree returning the first layers of the tree as HATEOAS-endpoint
type FirstPath :: p -> q -> [Type]
type family FirstPath api prefix where
  FirstPath (a :<|> b) prefix = FirstPath a prefix ++ FirstPath b prefix
  FirstPath (a :> _)   Bottom = '[          a :> ()]
  FirstPath (a :> _)   prefix = '[prefix :> a :> ()]
  FirstPath a          Bottom = '[          a :> ()]
  FirstPath a          prefix = '[prefix :> a :> ()]

type family RelName children :: Symbol where
  RelName ((sym :: Symbol) :> m s ct a) = sym
  RelName (Capture sym t   :> m s ct a) = sym

type family ReplaceHandler server replacement where
  ReplaceHandler (a :<|> b)  replacement = ReplaceHandler a replacement :<|> ReplaceHandler b replacement
  ReplaceHandler (a -> b)    replacement = a -> ReplaceHandler b replacement
  ReplaceHandler _           replacement = replacement

type BuildLayerLinks :: Layer -> Type -> Constraint
class BuildLayerLinks l server where
  buildLayerLinks ::
    ( HasLink (NodeApi l)
    , IsElem (NodeApi l) (NodeApi l)
    ) => Proxy l -> Proxy server -> ReplaceHandler server [(String, Link)]

instance
  ( MkLink api Link ~ Link
  , ReplaceHandler (m (res ())) [(String, Link)] ~ [(String, Link)]
  ) => BuildLayerLinks ('Layer api '[]) (m (res ())) where
  buildLayerLinks _ _ = [("self", self)]
    where
      self = safeLink (Proxy @api) (Proxy @api)

instance
  ( HasLink c
  , IsElem c c
  , MkLink c Link ~ Link
  , ReplaceHandler (m (res ())) [(String, Link)] ~ [(String, Link)]
  , BuildLayerLinks ('Layer api cs) (m (res ()))
  , KnownSymbol (RelName c)
  ) => BuildLayerLinks ('Layer api (c ': cs)) (m (res ())) where
  buildLayerLinks _ server = (symbolVal (Proxy @(RelName c)), l) : buildLayerLinks (Proxy @('Layer api cs)) server
    where
      l = safeLink (Proxy @c) (Proxy @c)

instance
  ( MkLink api Link ~ (p -> Link)
  , ReplaceHandler (p -> m (res ())) [(String, Link)] ~ (p -> [(String, Link)])
  ) => BuildLayerLinks ('Layer api '[]) (p -> m (res ())) where
  buildLayerLinks _ _ p = [("self", self p)]
    where
      self = safeLink (Proxy @api) (Proxy @api)

instance
  ( HasLink c
  , IsElem c c
  , MkLink c Link ~ (p -> Link)
  , ReplaceHandler (p -> m (res ())) [(String, Link)] ~ (p -> [(String, Link)])
  , BuildLayerLinks ('Layer api cs) (p -> m (res ()))
  , KnownSymbol (RelName c)
  ) => BuildLayerLinks ('Layer api (c ': cs)) (p -> m (res ())) where
  buildLayerLinks _ server p = (symbolVal (Proxy @(RelName c)), mkLink p) : buildLayerLinks (Proxy @('Layer api cs)) server p
    where
      mkLink = safeLink (Proxy @c) (Proxy @c)

type LayerApi :: [Layer] -> Type
type family LayerApi ls where
  LayerApi '[]       = EmptyAPI
  LayerApi (l ': ls) = NodeApi l :<|> LayerApi ls

type MkLayerServer :: (Type -> Type) -> Type -> [Layer] -> Constraint
class MkLayerServer m ct ls where
  mkLayerServer :: Proxy m -> Proxy ct -> Proxy ls -> ServerT (LayerApi ls) m

instance MkLayerServer m ct '[] where
  mkLayerServer _ _ _ = emptyServer

instance
  ( MkLayerServer m ct ls
  , HasLayerServer l (ServerT (NodeApi l) m) m ct
  , IsElem (NodeApi l) (NodeApi l)
  , MonadIO m, HasLink (NodeApi l)
  , BuildLayerLinks l (ServerT (NodeApi l) m)
  ) => MkLayerServer m ct (l ': ls) where
  mkLayerServer m ct _ = getLayerServer m ct (Proxy @l) (Proxy @(ServerT (NodeApi l) m)) :<|> mkLayerServer m ct (Proxy @ls)
