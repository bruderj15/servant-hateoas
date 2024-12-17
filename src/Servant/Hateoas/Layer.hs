{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Layer where

import Servant
import Servant.Server.Internal
import Servant.Hateoas.HasTemplateLink
import Servant.Hateoas.Resource
import Data.Kind
import Data.Aeson
import Data.Coerce
import Control.DotDotDot
import Control.Monad.IO.Class
import GHC.TypeLits

data Layer = Layer
  { nodeApi      :: Type      -- relative path from host to this layer:                     e.g.  /api/users
  , childrenApis :: [Type]    -- immediate relative children paths of this layer from host: e.g. [/api/users/1, ...]
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

instance (HasServer l context, HasServer ls context) => HasServer (l ': ls :: [Layer]) context where
  type ServerT (l ': ls) m = ServerT l m :<|> ServerT ls m
  route _ ctx delayed = route (Proxy @l) ctx ((\(sl :<|> _) -> sl) <$> delayed) `choice` route (Proxy @ls) ctx ((\(_ :<|> sls) -> sls) <$> delayed)
  hoistServerWithContext _ ctx f (sl :<|> sls) = hoistServerWithContext (Proxy @l) ctx f sl :<|> hoistServerWithContext (Proxy @ls) ctx f sls

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
-- TODO: Here is also the place where we can decide which layers are actually valid
--       E.g. a layer for BasicAuth makes no sense - so skip it here
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

type family ReplaceHandler server replacement where
  ReplaceHandler (a :<|> b)  replacement = ReplaceHandler a replacement :<|> ReplaceHandler b replacement
  ReplaceHandler (a -> b)    replacement = a -> ReplaceHandler b replacement
  ReplaceHandler _           replacement = replacement

type BuildLayerLinks :: Layer -> (Type -> Type) -> Constraint
class BuildLayerLinks l m where
  buildLayerLinks ::
    ( HasLink (NodeApi l)
    , IsElem (NodeApi l) (NodeApi l)
    , MonadIO m
    ) => Proxy l -> Proxy m -> ReplaceHandler (ServerT l m) [(String, URI)]

instance
  ( mkSelf ~ MkLink api Link
  , DotDotDot mkSelf (IsFun mkSelf)
  , Return mkSelf (IsFun mkSelf) ~ Link
  , Replace mkSelf [(String, URI)] (IsFun mkSelf) ~ ReplaceHandler (ServerT api m) [(String, URI)]
  ) => BuildLayerLinks ('Layer api '[]) m where
  buildLayerLinks _ _ = (pure @[] . ("self", ) . linkURI) ... mkSelf
    where
      mkSelf = safeLink (Proxy @api) (Proxy @api)

type LayerLinkable api c cs m mkLink =
  ( BuildLayerLinks ('Layer api cs) m
  , DotDotDot mkLink (IsFun mkLink)
  , ReplaceHandler (ServerT api m) [(String, URI)] ~ [(String, URI)]
  , Replace mkLink [(String, URI)] (IsFun mkLink) ~ [(String, URI)]
  , Return mkLink (IsFun mkLink) ~ Link
  )

instance
  ( LayerLinkable api c cs m mkLink
  , api ~ Verb http s cts a
  , c ~ (next :> Verb http s cts a)
  , mkLink ~ MkLink c Link
  , KnownSymbol next
  ) => BuildLayerLinks ('Layer (Verb http s cts a) (((next :: Symbol) :> Verb http s cts a) ': cs)) m where
  buildLayerLinks _ m = ((: ls) . (symbolVal (Proxy @next),) . linkURI) ... mkLink
    where
      mkLink = safeLink (Proxy @c) (Proxy @c)
      ls = buildLayerLinks (Proxy @('Layer api cs)) m

instance
  ( LayerLinkable api c cs m mkLink
  , api ~ (prefix :> Verb http s cts a)
  , c ~ (prefix :> next :> Verb http s cts a), IsElem c c, HasLink c
  , mkLink ~ MkLink c Link
  , KnownSymbol next
  ) => BuildLayerLinks ('Layer (prefix :> Verb http s cts a) ((prefix :> (next :: Symbol) :> Verb http s cts a) ': cs)) m where
  buildLayerLinks _ m = ((: ls) . (symbolVal (Proxy @next),) . linkURI) ... mkLink
    where
      mkLink = safeLink (Proxy @c) (Proxy @c)
      ls = buildLayerLinks (Proxy @('Layer api cs)) m

instance
  ( LayerLinkable api c cs m mkLink
  , api ~ Verb http s cts a
  , c ~ (Capture sym x :> Verb http s cts a)
  , HasTemplateLink c
  , (x -> mkLink) ~ MkLink c Link
  , KnownSymbol sym
  ) => BuildLayerLinks ('Layer (Verb http s cts a) ((Capture sym x :> Verb http s cts a) ': cs)) m where
  buildLayerLinks _ m = ((: ls) . (symbolVal (Proxy @sym),) . templateLink) ... toTemplateLink (Proxy @c)
    where
      ls = buildLayerLinks (Proxy @('Layer api cs)) m

instance
  ( LayerLinkable api c cs m mkLink
  , api ~ (prefix :> Verb http s cts a)
  , c ~ (prefix :> Capture sym x :> Verb http s cts a)
  , HasTemplateLink c
  , (x -> mkLink) ~ MkLink c Link
  , KnownSymbol sym
  ) => BuildLayerLinks ('Layer (prefix :> Verb http s cts a) ((prefix :> Capture sym x :> Verb http s cts a) ': cs)) m where
  buildLayerLinks _ m = ((: ls) . (symbolVal (Proxy @sym),) . templateLink) ... toTemplateLink (Proxy @c)
    where
      ls = buildLayerLinks (Proxy @('Layer api cs)) m

-- TODO: We need instances for all the other combinators as well
