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

type HasLayerServer :: Layer -> Type -> (Type -> Type) -> Type -> Constraint
class HasLayerServer l server m ct where
  getLayerServer :: (MonadIO m, ServerT (NodeApi l) m ~ server)
    => Proxy m
    -> Proxy ct
    -> Proxy l
    -> Proxy server
    -> ServerT (NodeApi l) m

instance
  ( res ~ MkResource ct
  , Resource res
  , BuildLayerLinks ('Layer api cs)
  ) => HasLayerServer ('Layer api cs) (m (res ())) m ct where
  getLayerServer _ _ l _ = return $ foldr addLink (wrap ()) $ buildLayerLinks l

type BuildLayerLinks :: Layer -> Constraint
class BuildLayerLinks l where
  buildLayerLinks :: Proxy l -> [(String, Link)]

instance
  ( HasLink api
  , IsElem api api
  , MkLink api Link ~ Link
  ) => BuildLayerLinks ('Layer api '[]) where
  buildLayerLinks _ = [("self", self)]
    where
      self = safeLink (Proxy @api) (Proxy @api)

instance
  ( HasLink c
  , IsElem c c
  , MkLink c Link ~ Link
  , BuildLayerLinks ('Layer api cs)
  , KnownSymbol (RelName c)
  ) => BuildLayerLinks ('Layer api (c ': cs)) where
  buildLayerLinks _ = (symbolVal (Proxy @(RelName c)), l) : buildLayerLinks (Proxy @('Layer api cs))
    where
      l = safeLink (Proxy @c) (Proxy @c)

type family RelName children :: Symbol where
  RelName ((sym :: Symbol) :> m s ct a) = sym
  RelName (Capture sym t   :> m s ct a) = sym
