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
import Servant.Hateoas.Resource
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

-- | Create all 'ResourceLink's to a 'Layer's 'RelativeChildren'.
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

instance
  ( c ~ MkPrefix (apiCs ++ '[Sym sym]) verb
  , HasLink c, IsElem c c
  , mkLink ~ MkLink c Link
  , KnownSymbol sym
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT (MkPrefix apiCs verb) m) [(String, ResourceLink)])
  , PolyvariadicComp2 mkLink buildLinksFun (IsFun mkLink)
  , Return2 mkLink buildLinksFun (IsFun mkLink) ~ (Link, [(String, ResourceLink)])
  , Replace2 mkLink buildLinksFun [(String, ResourceLink)] (IsFun mkLink) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (Sym sym ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(l, ls) -> (symbolVal (Proxy @sym), CompleteLink l) : ls) mkLink mkLinks
    where
      mkLink = safeLink (Proxy @c) (Proxy @c)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[Capture' mods sym x] verb)
  , IsElem api api, HasLink api
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, ResourceLink)])
  , PolyvariadicComp2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun) ~ (Link, [(String, ResourceLink)])
  , Replace2 (MkLink api Link) buildLinksFun [(String, ResourceLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (Capture' mods sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, mkTemplatedNext self) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = safeLink (Proxy @api) (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[Capture' mods sym x] verb))
      mkTemplatedNext = TemplateLink
        . (\rl -> rl { _path = _path rl `appendPath` _path child, _templated = True })
        . fromURI (allMime $ Proxy @cts) (reflectStdMethod (Proxy @method))
        . linkURI

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[CaptureAll sym x] verb)
  , IsElem api api, HasLink api
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, ResourceLink)])
  , PolyvariadicComp2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun) ~ (Link, [(String, ResourceLink)])
  , Replace2 (MkLink api Link) buildLinksFun [(String, ResourceLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (CaptureAll sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, mkTemplatedNext self) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = safeLink (Proxy @api) (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[CaptureAll sym x] verb))
      mkTemplatedNext = TemplateLink
        . (\rl -> rl { _path = _path rl `appendPath` _path child, _templated = True })
        . fromURI (allMime $ Proxy @cts) (reflectStdMethod (Proxy @method))
        . linkURI

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[QueryParam' mods sym x] verb)
  , IsElem api api, HasLink api
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, ResourceLink)])
  , PolyvariadicComp2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun) ~ (Link, [(String, ResourceLink)])
  , Replace2 (MkLink api Link) buildLinksFun [(String, ResourceLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (QueryParam' mods sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, mkTemplatedNext self) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = safeLink (Proxy @api) (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[QueryParam' mods sym x] verb))
      mkTemplatedNext = TemplateLink
        . (\rl -> rl { _params = _params rl ++ _params child, _templated = True })
        . fromURI (allMime $ Proxy @cts) (reflectStdMethod (Proxy @method))
        . linkURI

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[QueryParams sym x] verb)
  , IsElem api api, HasLink api
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, ResourceLink)])
  , PolyvariadicComp2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun) ~ (Link, [(String, ResourceLink)])
  , Replace2 (MkLink api Link) buildLinksFun [(String, ResourceLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (QueryParams sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, mkTemplatedNext self) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = safeLink (Proxy @api) (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[QueryParams sym x] verb))
      mkTemplatedNext = TemplateLink
        . (\rl -> rl { _params = _params rl ++ _params child, _templated = True })
        . fromURI (allMime $ Proxy @cts) (reflectStdMethod (Proxy @method))
        . linkURI

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[DeepQuery sym x] verb)
  , IsElem api api, HasLink api
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, ResourceLink)])
  , PolyvariadicComp2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun) ~ (Link, [(String, ResourceLink)])
  , Replace2 (MkLink api Link) buildLinksFun [(String, ResourceLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (DeepQuery sym x ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, mkTemplatedNext self) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = safeLink (Proxy @api) (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[DeepQuery sym x] verb))
      mkTemplatedNext = TemplateLink
        . (\rl -> rl { _params = _params rl ++ _params child, _templated = True })
        . fromURI (allMime $ Proxy @cts) (reflectStdMethod (Proxy @method))
        . linkURI

instance
  ( verb ~ Verb method status cts a
  , AllMime cts, ReflectMethod method
  , api ~ MkPrefix apiCs verb
  , KnownSymbol sym
  , HasTemplatedLink (MkPrefix '[QueryFlag sym] verb)
  , IsElem api api, HasLink api
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT api m) [(String, ResourceLink)])
  , PolyvariadicComp2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun)
  , Return2 (MkLink api Link) buildLinksFun (IsFun buildLinksFun) ~ (Link, [(String, ResourceLink)])
  , Replace2 (MkLink api Link) buildLinksFun [(String, ResourceLink)] (IsFun buildLinksFun) ~ buildLinksFun
  ) => BuildLayerLinks ('Layer apiCs (QueryFlag sym ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(self, ls) -> (relName, mkTemplatedNext self) : ls) mkSelf mkLinks
    where
      relName = symbolVal (Proxy @sym)
      mkSelf = safeLink (Proxy @api) (Proxy @api)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      child = toTemplatedLink (Proxy @(MkPrefix '[QueryFlag sym] verb))
      mkTemplatedNext = TemplateLink
        . (\rl -> rl { _params = _params rl ++ _params child, _templated = True })
        . fromURI (allMime $ Proxy @cts) (reflectStdMethod (Proxy @method))
        . linkURI
