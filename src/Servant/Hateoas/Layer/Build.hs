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
  , Replace2 mkLink buildLinksFun [(String, ResourceLink)] (IsFun mkLink) ~ ReplaceHandler (ServerT (MkPrefix apiCs verb) m) [(String, ResourceLink)]
  ) => BuildLayerLinks ('Layer apiCs (Sym sym ': cs) verb) m where
  buildLayerLinks _ m = pcomp2 (\(l, ls) -> (symbolVal (Proxy @sym), CompleteLink l) : ls) mkLink mkLinks
    where
      mkLink = safeLink (Proxy @c) (Proxy @c)
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m

instance
  ( c ~ MkPrefix (apiCs ++ '[Capture' mods sym x]) verb
  , HasRelationLink c
  , KnownSymbol sym
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT (MkPrefix apiCs verb) m) [(String, ResourceLink)])
  , PolyvariadicComp buildLinksFun (IsFun buildLinksFun)
  , Replace buildLinksFun [(String, ResourceLink)] (IsFun buildLinksFun) ~ ReplaceHandler (ServerT (MkPrefix apiCs verb) m) [(String, ResourceLink)]
  , Return buildLinksFun (IsFun buildLinksFun) ~ [(String, ResourceLink)]
  ) => BuildLayerLinks ('Layer apiCs (Capture' mods sym x ': cs) verb) m where
  buildLayerLinks _ m = (\ls -> (symbolVal (Proxy @sym), TemplateLink l) : ls) ... mkLinks
    where
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      l = toRelationLink (Proxy @c)

instance
  ( c ~ MkPrefix (apiCs ++ '[CaptureAll sym x]) verb
  , HasRelationLink c
  , KnownSymbol sym
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT (MkPrefix apiCs verb) m) [(String, ResourceLink)])
  , PolyvariadicComp buildLinksFun (IsFun buildLinksFun)
  , Replace buildLinksFun [(String, ResourceLink)] (IsFun buildLinksFun) ~ ReplaceHandler (ServerT (MkPrefix apiCs verb) m) [(String, ResourceLink)]
  , Return buildLinksFun (IsFun buildLinksFun) ~ [(String, ResourceLink)]
  ) => BuildLayerLinks ('Layer apiCs (CaptureAll sym x ': cs) verb) m where
  buildLayerLinks _ m = (\ls -> (symbolVal (Proxy @sym), TemplateLink l) : ls) ... mkLinks
    where
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      l = toRelationLink (Proxy @c)

instance
  ( c ~ MkPrefix (apiCs ++ '[QueryParam' mods sym x]) verb
  , HasRelationLink c
  , KnownSymbol sym
  , BuildLayerLinks ('Layer apiCs cs verb) m
  , buildLinksFun ~ (ReplaceHandler (ServerT (MkPrefix apiCs verb) m) [(String, ResourceLink)])
  , PolyvariadicComp buildLinksFun (IsFun buildLinksFun)
  , Replace buildLinksFun [(String, ResourceLink)] (IsFun buildLinksFun) ~ ReplaceHandler (ServerT (MkPrefix apiCs verb) m) [(String, ResourceLink)]
  , Return buildLinksFun (IsFun buildLinksFun) ~ [(String, ResourceLink)]
  ) => BuildLayerLinks ('Layer apiCs (QueryParam' mods sym x ': cs) verb) m where
  buildLayerLinks _ m = (\ls -> (symbolVal (Proxy @sym), TemplateLink l) : ls) ... mkLinks
    where
      mkLinks = buildLayerLinks (Proxy @('Layer apiCs cs verb)) m
      l = toRelationLink (Proxy @c)
