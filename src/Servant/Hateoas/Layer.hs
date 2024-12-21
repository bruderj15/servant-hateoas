{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Layer where

import Servant
import Servant.Server.Internal
import Servant.Hateoas.Resource
import Servant.Hateoas.HasRelationLink
import Servant.Hateoas.Internal.Polyvariadic
import Data.Kind
import Data.Aeson
import Data.Coerce
import Control.Monad.IO.Class
import GHC.TypeLits

data Layer = Layer
  { api              :: [Type]
  , relativeChildren :: [Type]
  , verb             :: Type
  }

type family LayerApiCs (a :: Layer) where
  LayerApiCs ('Layer api _ _) = api

type family RelativeChildren (a :: Layer) where
  RelativeChildren ('Layer _ children _) = children

type family LayerVerb (a :: Layer) where
  LayerVerb ('Layer _ _ verb) = verb

type family LayerApi (a :: Layer) where
  LayerApi ('Layer api _ verb) = MkPrefix api verb

instance HasServer (MkPrefix apiCs verb) context => HasServer ('Layer apiCs cs verb) context where
  type ServerT ('Layer apiCs cs verb) m = ServerT (MkPrefix apiCs verb) m
  route _ = route (Proxy @(MkPrefix apiCs verb))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(MkPrefix apiCs verb))

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

-- Make api a tree with shared prefixes - making every choice unambiguous
type family Normalize api where
  Normalize ((prefix :> a) :<|> (prefix :> b)) = Normalize (prefix :> (Normalize a :<|> Normalize b))
  Normalize (a :<|> b)                         = Normalize a :<|> Normalize b
  Normalize ((prefix :> a) :>   (prefix :> b)) = Normalize (prefix :> (Normalize a :>   Normalize b))
  Normalize (a :> b)                           = a :> Normalize b
  Normalize a                                  = a

type MkLayers :: p -> [Layer]
type family MkLayers api where
  MkLayers api = MergeLayers (GoLayers (Normalize (Symify api)) '[]) '[]

type MergeLayers :: [Layer] -> [Layer] -> [Layer]
type family MergeLayers ls acc where
  MergeLayers '[] acc = acc
  MergeLayers (l ': ls) acc = MergeLayers ls (
      If (ContainsLayerApi acc (LayerApiCs l))
      (WithAllChildrenOfLayerApi acc l)
        (l ': acc)
      )

type family ContainsLayerApi ls api where
  ContainsLayerApi '[] _ = 'False
  ContainsLayerApi ('Layer api _ _ ': ls) api = 'True
  ContainsLayerApi (_ ': ls) api = ContainsLayerApi ls api

type family WithAllChildrenOfLayerApi ls l where
  WithAllChildrenOfLayerApi '[] _ = '[]
  WithAllChildrenOfLayerApi (('Layer api cs verb) ': ls) ('Layer api cs' verb) = 'Layer api (cs ++ cs') verb ': ls
  WithAllChildrenOfLayerApi (l ': ls) l' = l ': WithAllChildrenOfLayerApi ls l'

data Sym (sym :: Symbol)

instance (HasServer api context, KnownSymbol sym) => HasServer (Sym sym :> api) context where
  type ServerT (Sym sym :> api) m = ServerT (sym :> api) m
  route _ = route (Proxy @(sym :> api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(sym :> api))

instance (HasLink api, KnownSymbol sym) => HasLink (Sym sym :> api) where
  type MkLink (Sym sym :> api) link = MkLink (sym :> api) link
  toLink f _ = toLink f (Proxy @(sym :> api))

type family Symify api where
  Symify (a :<|> b) = Symify a :<|> Symify b
  Symify ((sym :: Symbol) :> b) = Sym sym :> Symify b
  Symify (a :> b) = a :> Symify b
  Symify a = a

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

type MkPrefix :: [Type] -> Type -> Type
type family MkPrefix prefix api where
  MkPrefix (Sym x      ': xs) api = x :> MkPrefix xs api
  MkPrefix (x          ': xs) api = x :> MkPrefix xs api
  MkPrefix '[]                api = api

type family ReplaceHandler server replacement where
  ReplaceHandler (a :<|> b)  replacement = ReplaceHandler a replacement :<|> ReplaceHandler b replacement
  ReplaceHandler (a -> b)    replacement = a -> ReplaceHandler b replacement
  ReplaceHandler _           replacement = replacement

type BuildLayerLinks :: Layer -> (Type -> Type) -> Constraint
class BuildLayerLinks l m where
  buildLayerLinks :: MonadIO m => Proxy l -> Proxy m -> ReplaceHandler (ServerT l m) [(String, ResourceLink)]

instance
  ( api ~ MkPrefix apiCs verb
  , HasLink api, IsElem api api
  , mkSelf ~ MkLink api Link
  , PolyvariadicComp mkSelf (IsFun mkSelf)
  , Return mkSelf (IsFun mkSelf) ~ Link
  , Replace mkSelf [(String, ResourceLink)] (IsFun mkSelf) ~ ReplaceHandler (ServerT api m) [(String, ResourceLink)]
  ) => BuildLayerLinks ('Layer apiCs '[] verb) m where
  buildLayerLinks _ _ = (pure @[] . ("self", ) . CompleteLink) ... mkSelf
    where
      mkSelf = safeLink (Proxy @api) (Proxy @api)

type LayerLinkable api cs verb m mkLink =
  ( BuildLayerLinks ('Layer api cs verb) m
  , PolyvariadicComp mkLink (IsFun mkLink)
  , ReplaceHandler (ServerT (MkPrefix api verb) m) [(String, ResourceLink)] ~ [(String, ResourceLink)]
  , Replace mkLink [(String, ResourceLink)] (IsFun mkLink) ~ [(String, ResourceLink)]
  , Return mkLink (IsFun mkLink) ~ Link
  )

instance
  ( LayerLinkable apiCs cs verb m mkLink
  , c ~ MkPrefix (apiCs ++ '[Sym sym]) verb
  , HasLink c, IsElem c c
  , mkLink ~ MkLink c Link
  , KnownSymbol sym
  ) => BuildLayerLinks ('Layer apiCs (Sym sym ': cs) verb) m where
  buildLayerLinks _ m = ((: ls) . (symbolVal (Proxy @sym),) . CompleteLink) ... mkLink
    where
      mkLink = safeLink (Proxy @c) (Proxy @c)
      ls = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m

instance
  ( LayerLinkable apiCs cs verb m mkLink
  , c ~ MkPrefix (apiCs ++ '[Capture' mods sym x]) verb
  , HasRelationLink c
  , (x -> mkLink) ~ MkLink c Link
  , KnownSymbol sym
  ) => BuildLayerLinks ('Layer apiCs (Capture' mods sym x ': cs) verb) m where
  buildLayerLinks _ m = ((: ls) . (symbolVal (Proxy @sym),) . TemplateLink) ... toRelationLink (Proxy @c)
    where
      ls = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m

instance
  ( LayerLinkable apiCs cs verb m mkLink
  , c ~ MkPrefix (apiCs ++ '[CaptureAll sym x]) verb
  , HasRelationLink c
  , (x -> mkLink) ~ MkLink c Link
  , KnownSymbol sym
  ) => BuildLayerLinks ('Layer apiCs (CaptureAll sym x ': cs) verb) m where
  buildLayerLinks _ m = ((: ls) . (symbolVal (Proxy @sym),) . TemplateLink) ... toRelationLink (Proxy @c)
    where
      ls = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
