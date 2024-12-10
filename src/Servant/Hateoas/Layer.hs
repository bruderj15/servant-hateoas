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

type (++) xs ys = AppendList xs ys

-- Wrapping api in: Bottom :> api :> Top, so api has kind k and not Type.
-- This is crucial so we can match paths (:: Symbol) and potential other-kinded combinators
data Bottom
data Top

type GetHateoas ct = Get '[ct] (MkResource ct ())

-- Creates all intermediate layers of the api and their immediate children as HATEOAS-endpoints
type Layers :: p -> q -> Type -> [Layer]
type family Layers api stand ct where
  Layers (a :<|> b)  Bottom                   ct = Layers a Bottom ct ++ Layers b Bottom ct
  Layers (a :<|> b) (Bottom :> prefix :> Top) ct = Layers a (Bottom :> prefix :> Top) ct ++ Layers b (Bottom :> prefix :> Top) ct
  Layers (a :> b)    Bottom                   ct = '[ 'Layer           (GetHateoas ct) (FirstPath a Bottom ct) ] ++ Layers b (Bottom           :> a :> Top) ct
  Layers (a :> b)   (Bottom :> prefix :> Top) ct = '[ 'Layer (prefix :> GetHateoas ct) (FirstPath a prefix ct) ] ++ Layers b (Bottom :> prefix :> a :> Top) ct
  Layers _ _                                  ct = '[]

-- Interpreting api as a tree returning the first layers of the tree as HATEOAS-endpoint
type FirstPath :: p -> q -> Type -> [Type]
type family FirstPath api prefix ct where
  FirstPath (a :<|> b) prefix ct = FirstPath a prefix ct ++ FirstPath b prefix ct
  FirstPath (a :> _)   Bottom ct = '[          a :> GetHateoas ct]
  FirstPath (a :> _)   prefix ct = '[prefix :> a :> GetHateoas ct]
  FirstPath a          Bottom ct = '[          a :> GetHateoas ct]
  FirstPath a          prefix ct = '[prefix :> a :> GetHateoas ct]

-- This seems highly similar to HasResourceServer if it was poly-kinded in the api
type HasLayerServer :: Layer -> Type -> (Type -> Type) -> Type -> Constraint
class HasLayerServer l server m ct where
  getLayerServer ::
    ( MonadIO m
    , HasLink (NodeApi l)
    , IsElem (NodeApi l) (NodeApi l)
    , BuildLayerLinks l server
    , ServerT (NodeApi l) m ~ server
    ) => Proxy m
      -> Proxy ct
      -> Proxy l
      -> Proxy server
      -> ServerT (NodeApi l) m

instance
  ( res ~ MkResource ct
  , Resource res
  , ReplaceHandler (m (res ())) [(String, Link)] ~ [(String, Link)]
  ) => HasLayerServer ('Layer api cs) (m (res ())) m ct where
  getLayerServer _ _ l server = return $ foldr addLink (wrap ()) $ buildLayerLinks l server

instance
  ( res ~ MkResource ct
  , Resource res
  , ReplaceHandler (m (res ())) [(String, Link)] ~ [(String, Link)]
  ) => HasLayerServer ('Layer api cs) (p -> m (res ())) m ct where
  getLayerServer _ _ l server p = return $ foldr addLink (wrap ()) $ buildLayerLinks l server p

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
