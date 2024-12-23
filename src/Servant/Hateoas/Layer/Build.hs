{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Layer.Build
(
  -- * Type family
  ReplaceHandler,

  -- * Class
  BuildLayerLinks(..),

  -- * Utility Constraint
  LayerLinkable
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

-- | Convenience alias 'Constraint' for 'Layer's that can be linked.
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
