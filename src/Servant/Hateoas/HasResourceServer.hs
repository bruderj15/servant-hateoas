{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.HasResourceServer where

import Servant
import Servant.Hateoas.Layer
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler
import Servant.Hateoas.Internal.Polyvariadic
import Data.Kind
import Control.Monad.IO.Class
import GHC.TypeLits

-- Wrap response type with the content-types resource.
type Resourcify :: k -> Type -> k
type family Resourcify api ct where
  Resourcify EmptyAPI        ct = EmptyAPI
  Resourcify (a :<|> b)      ct = Resourcify a ct :<|> Resourcify b ct
  Resourcify (a :> b)        ct = a :> Resourcify b ct
  Resourcify (Verb m s _ a)  ct = Verb m s '[ct] (MkResource ct a)
  Resourcify ('Layer api cs verb) ct = 'Layer (Resourcify api ct) (Resourcify cs ct) (Resourcify verb ct)
  Resourcify (x:xs)          ct = Resourcify x ct : Resourcify xs ct
  Resourcify a               _  = a

-- Given a @ServerT api m@ and some @ct@ we want a @ServerT (Resourcify api ct) m@.
type ResourcifyServer :: k -> Type -> (Type -> Type) -> Type
type family ResourcifyServer server ct m where
  ResourcifyServer EmptyServer ct m = EmptyServer
  ResourcifyServer (a :<|> b)  ct m = ResourcifyServer a ct m :<|> ResourcifyServer b ct m
  ResourcifyServer (a -> b)    ct m = a -> ResourcifyServer b ct m
  ResourcifyServer (m a)       ct m = m (MkResource ct a)
  ResourcifyServer (f a)       ct m = f (ResourcifyServer a ct m) -- needed for containers like [Foo]

class HasResourceServer api m ct where
  getResourceServer :: MonadIO m => Proxy m -> Proxy ct -> Proxy api -> ServerT (Resourcify api ct) m

instance {-# OVERLAPPING #-} (HasResourceServer a m ct, HasResourceServer b m ct) => HasResourceServer (a :<|> b) m ct where
  getResourceServer m ct _ = getResourceServer m ct (Proxy @a) :<|> getResourceServer m ct (Proxy @b)

instance {-# OVERLAPPING #-} HasResourceServer b m ct => HasResourceServer ((sym :: Symbol) :> b) m ct where
  getResourceServer m ct _ = getResourceServer m ct (Proxy @b)

instance {-# OVERLAPPING #-} HasResourceServer b m ct => HasResourceServer (Description sym :> b) m ct where
  getResourceServer m ct _ = getResourceServer m ct (Proxy @b)

instance {-# OVERLAPPING #-} HasResourceServer b m ct => HasResourceServer (Summary sym :> b) m ct where
  getResourceServer m ct _ = getResourceServer m ct (Proxy @b)

instance
  ( server ~ ServerT api m
  , ServerT (Resourcify api ct) m ~ ResourcifyServer server ct m
  , mkLink ~ MkLink api Link
  , res ~ MkResource ct
  , Resource res
  , ToResource res a
  , HasHandler api
  , HasLink api, IsElem api api
  , PolyvariadicComp2 server mkLink (IsFun server)
  , Return2 server mkLink (IsFun server) ~ (m a, Link)
  , Replace2 server mkLink (m (res a)) (IsFun mkLink) ~ ResourcifyServer server ct m
  ) => HasResourceServer (api :: Type) m ct where
  getResourceServer m _ api = pcomp2 ((\(ma, self) -> (addSelfRel (CompleteLink self) . toResource (Proxy @res)) <$> ma)) (getHandler m api) mkSelf
    where
      mkSelf = safeLink api api

instance {-# OVERLAPPING #-}
  ( api ~ LayerApi l
  , rApi ~ Resourcify api ct
  , ServerT (Resourcify l ct) m ~ ResourcifyServer (ServerT l m) ct m
  , rServer ~ ResourcifyServer (ServerT l m) ct m
  , res ~ MkResource ct
  , buildFun ~ ReplaceHandler rServer [(String, ResourceLink)]
  , Resource res
  , BuildLayerLinks (Resourcify l ct) m
  , PolyvariadicComp buildFun (IsFun buildFun)
  , Return buildFun (IsFun buildFun) ~ [(String, ResourceLink)]
  , Replace buildFun (m (res Intermediate)) (IsFun buildFun) ~ rServer
  ) => HasResourceServer l m ct where
  getResourceServer m _ _ = (return @m . foldr addRel (wrap @res $ Intermediate ())) ... buildLayerLinks (Proxy @(Resourcify l ct)) m

instance {-# OVERLAPPING #-} HasResourceServer ('[] :: [Layer]) m ct where
  getResourceServer _ _ _ = emptyServer

instance {-# OVERLAPPING #-}
  ( MonadIO m
  , HasResourceServer ls m ct
  , HasResourceServer l m ct
  , BuildLayerLinks (Resourcify l ct) m
  ) => HasResourceServer (l ': ls) m ct where
  getResourceServer m ct _ = getResourceServer m ct (Proxy @l) :<|> getResourceServer m ct (Proxy @ls)
