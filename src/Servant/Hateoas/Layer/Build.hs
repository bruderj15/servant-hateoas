{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Layer.Build
(
  -- * Type family
  ReplaceHandler,

  -- * Class
  BuildLayerLinks(..),
)
where

import Servant
import Servant.API.ContentTypes
import Servant.Hateoas.RelationLink
import Servant.Hateoas.Layer.Type
import Servant.Hateoas.Internal.Sym
import Servant.Hateoas.Internal.Polyvariadic
import Data.Kind
import Control.Monad.IO.Class
import GHC.TypeLits

-- | Replace the result 'Type' of a function with a new 'Type'.
type family ReplaceHandler server replacement where
  ReplaceHandler (a :<|> b)  replacement = ReplaceHandler a replacement :<|> ReplaceHandler b replacement
  ReplaceHandler (a -> b)    replacement = a -> ReplaceHandler b replacement
  ReplaceHandler _           replacement = replacement

-- | Create all 'RelationLink's to a 'Layer's 'RelativeChildren'.
type BuildLayerLinks :: Layer -> (Type -> Type) -> Constraint
class BuildLayerLinks l m where
  buildLayerLinks :: MonadIO m => Proxy l -> Proxy m -> ReplaceHandler (ServerT l m) [(String, RelationLink)]

instance
  ( api ~ MkPrefix apiCs verb
  , HasRelationLink api
  , mkSelf ~ MkLink api RelationLink
  , PolyvariadicComp mkSelf (IsFun mkSelf)
  , Return mkSelf (IsFun mkSelf) ~ RelationLink
  , Replace mkSelf [(String, RelationLink)] (IsFun mkSelf) ~ ReplaceHandler (ServerT api m) [(String, RelationLink)]
  ) => BuildLayerLinks ('Layer apiCs '[] verb) m where
  buildLayerLinks _ _ = (pure @[] . ("self", )) ... mkSelf
    where
      mkSelf = toRelationLink (Proxy @api)

instance
  ( c ~ MkPrefix (apiCs ++ '[Sym sym]) verb
  , HasRelationLink c
  , mkLink ~ MkLink c RelationLink
  , KnownSymbol sym
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT (MkPrefix apiCs verb) m) [(String, RelationLink)])
  , PolyvariadicComp2 mkLink buildLinksFun (IsFun mkLink)
  , Return2 mkLink buildLinksFun (IsFun mkLink) ~ (RelationLink, [(String, RelationLink)])
  , Replace2 mkLink buildLinksFun [(String, RelationLink)] (IsFun mkLink) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (Sym sym ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(l, ls) -> (symbolVal (Proxy @sym), l) : ls) mkLink mkLinks
    where
      mkLink = toRelationLink (Proxy @c)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , HasRelationLink api
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[Capture' mods sym x] verb)
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, RelationLink)])
  , PolyvariadicComp2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun) ~ (RelationLink, [(String, RelationLink)])
  , Replace2 (MkLink api RelationLink) buildLinksFun [(String, RelationLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (Capture' mods sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, self <<< child) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = toRelationLink (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[Capture' mods sym x] verb))

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , HasRelationLink api
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[CaptureAll sym x] verb)
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, RelationLink)])
  , PolyvariadicComp2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun) ~ (RelationLink, [(String, RelationLink)])
  , Replace2 (MkLink api RelationLink) buildLinksFun [(String, RelationLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (CaptureAll sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, self <<< child) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = toRelationLink (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[CaptureAll sym x] verb))

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , HasRelationLink api
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[QueryParam' mods sym x] verb)
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, RelationLink)])
  , PolyvariadicComp2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun) ~ (RelationLink, [(String, RelationLink)])
  , Replace2 (MkLink api RelationLink) buildLinksFun [(String, RelationLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (QueryParam' mods sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, self <<< child) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = toRelationLink (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[QueryParam' mods sym x] verb))

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , HasRelationLink api
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[QueryParams sym x] verb)
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, RelationLink)])
  , PolyvariadicComp2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun) ~ (RelationLink, [(String, RelationLink)])
  , Replace2 (MkLink api RelationLink) buildLinksFun [(String, RelationLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (QueryParams sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, self <<< child) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = toRelationLink (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[QueryParams sym x] verb))

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , HasRelationLink api
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[DeepQuery sym x] verb)
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, RelationLink)])
  , PolyvariadicComp2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun) ~ (RelationLink, [(String, RelationLink)])
  , Replace2 (MkLink api RelationLink) buildLinksFun [(String, RelationLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (DeepQuery sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, self <<< child) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = toRelationLink (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[DeepQuery sym x] verb))

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , HasRelationLink api
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[QueryFlag sym] verb)
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, RelationLink)])
  , PolyvariadicComp2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api RelationLink) buildLinksFun (IsFun buildLinksFun) ~ (RelationLink, [(String, RelationLink)])
  , Replace2 (MkLink api RelationLink) buildLinksFun [(String, RelationLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (QueryFlag sym ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, self <<< child) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = toRelationLink (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[QueryFlag sym] verb))
