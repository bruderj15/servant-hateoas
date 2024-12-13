{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Layer where

import Servant
import Servant.Hateoas.Resource
import Data.Kind
import Data.Aeson
import Data.Coerce
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

instance HasServer ('[] :: [Layer]) context where
  type ServerT '[] m = ServerT EmptyAPI m
  route _ = route (Proxy @EmptyAPI)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @EmptyAPI)

instance HasServer (l ': ls :: [Layer]) context where
  type ServerT (l ': ls) m = ServerT l m :<|> ServerT ls m
  route _ = route (Proxy @(l ': ls))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(l ': ls))

newtype Intermediate = Intermediate ()
  deriving newtype (Show, Eq, Ord, ToJSON)
type GetIntermediate = Get '[] Intermediate

instance Resource res => ToResource res Intermediate where
  toResource _ _ = wrap $ coerce ()

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
  Layers (a :> b)    Bottom                   = '[ 'Layer            GetIntermediate  (FirstPath a Bottom) ] ++ Layers b (Bottom           :> a :> Top)
  Layers (a :> b)   (Bottom :> prefix :> Top) = '[ 'Layer (prefix :> GetIntermediate) (FirstPath a prefix) ] ++ Layers b (Bottom :> prefix :> a :> Top)
  Layers _ _                                  = '[]

-- Interpreting api as a tree returning the first layers of the tree as HATEOAS-endpoint
type FirstPath :: p -> q -> [Type]
type family FirstPath api prefix where
  FirstPath (a :<|> b) prefix = FirstPath a prefix ++ FirstPath b prefix
  FirstPath (a :> _)   Bottom = '[          a :> GetIntermediate]
  FirstPath (a :> _)   prefix = '[prefix :> a :> GetIntermediate]
  FirstPath a          Bottom = '[          a :> GetIntermediate]
  FirstPath a          prefix = '[prefix :> a :> GetIntermediate]

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
  , ReplaceHandler (m (res Intermediate)) [(String, Link)] ~ [(String, Link)]
  ) => BuildLayerLinks ('Layer api '[]) (m (res Intermediate)) where
  buildLayerLinks _ _ = [("self", self)]
    where
      self = safeLink (Proxy @api) (Proxy @api)

instance
  ( HasLink c
  , IsElem c c
  , MkLink c Link ~ Link
  , ReplaceHandler (m (res Intermediate)) [(String, Link)] ~ [(String, Link)]
  , BuildLayerLinks ('Layer api cs) (m (res Intermediate))
  , KnownSymbol (RelName c)
  ) => BuildLayerLinks ('Layer api (c ': cs)) (m (res Intermediate)) where
  buildLayerLinks _ server = (symbolVal (Proxy @(RelName c)), l) : buildLayerLinks (Proxy @('Layer api cs)) server
    where
      l = safeLink (Proxy @c) (Proxy @c)

instance
  ( MkLink api Link ~ (p -> Link)
  , ReplaceHandler (p -> m (res Intermediate)) [(String, Link)] ~ (p -> [(String, Link)])
  ) => BuildLayerLinks ('Layer api '[]) (p -> m (res Intermediate)) where
  buildLayerLinks _ _ p = [("self", self p)]
    where
      self = safeLink (Proxy @api) (Proxy @api)

instance
  ( HasLink c
  , IsElem c c
  , MkLink c Link ~ (p -> Link)
  , ReplaceHandler (p -> m (res Intermediate)) [(String, Link)] ~ (p -> [(String, Link)])
  , BuildLayerLinks ('Layer api cs) (p -> m (res Intermediate))
  , KnownSymbol (RelName c)
  ) => BuildLayerLinks ('Layer api (c ': cs)) (p -> m (res Intermediate)) where
  buildLayerLinks _ server p = (symbolVal (Proxy @(RelName c)), mkLink p) : buildLayerLinks (Proxy @('Layer api cs)) server p
    where
      mkLink = safeLink (Proxy @c) (Proxy @c)
