{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.HasResourceServer where

import Servant
import Servant.Hateoas.Layer
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler
import Data.Kind
import Control.DotDotDot
import Control.Monad.IO.Class

-- Wrap response type with the content-types resource.
type Resourcify :: k -> Type -> k
type family Resourcify api ct where
  Resourcify EmptyAPI        ct = EmptyAPI
  Resourcify (a :<|> b)      ct = Resourcify a ct :<|> Resourcify b ct
  Resourcify (a :> b)        ct = a :> Resourcify b ct
  Resourcify (Verb m s _ a)  ct = Verb m s '[ct] (MkResource ct a)
  Resourcify ('Layer api cs) ct = 'Layer (Resourcify api ct) (Resourcify cs ct)
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

instance
  ( server ~ ServerT api m
  , ServerT (Resourcify api ct) m ~ ResourcifyServer server ct m
  , res ~ MkResource ct
  , ToResource res a
  , HasHandler api
  , DotDotDot server (IsFun server)
  , Return server (IsFun server) ~ m a
  , Replace server (m (res a)) (IsFun server) ~ ResourcifyServer server ct m
  ) => HasResourceServer api m ct where
  getResourceServer m _ api = fmap (toResource (Proxy @res)) ... getHandler m api

instance {-# OVERLAPPING #-}
  ( l ~ 'Layer api cs
  , rApi ~ Resourcify api ct
  , HasLink rApi
  , IsElem rApi rApi
  , ServerT (Resourcify api ct) m ~ ResourcifyServer (ServerT api m) ct m
  , rServer ~ ResourcifyServer (ServerT api m) ct m
  , res ~ MkResource ct
  , rServerRels ~ ReplaceHandler rServer [(String, Link)]
  , Resource res
  , BuildLayerLinks (Resourcify l ct) rServer
  , DotDotDot rServerRels (IsFun rServerRels)
  , Return rServerRels (IsFun rServerRels) ~ [(String, Link)]
  , Replace rServerRels (m (res Intermediate)) (IsFun rServerRels) ~ rServer
  ) => HasResourceServer ('Layer api cs) m ct where
  getResourceServer _ _ _ = (return @m . foldr addLink (wrap @res $ Intermediate ())) ... buildLayerLinks (Proxy @(Resourcify l ct)) (Proxy @rServer)

instance {-# OVERLAPPING #-} HasResourceServer ('[] :: [Layer]) m ct where
  getResourceServer _ _ _ = emptyServer

instance {-# OVERLAPPING #-}
  ( MonadIO m
  , HasResourceServer ls m ct
  , HasResourceServer l m ct
  , HasLink (NodeApi l)
  , IsElem (NodeApi l) (NodeApi l)
  , BuildLayerLinks (Resourcify l ct) (ResourcifyServer (ServerT l m) ct m)
  ) => HasResourceServer (l ': ls) m ct where
  getResourceServer m ct _ = getResourceServer m ct (Proxy @l) :<|> getResourceServer m ct (Proxy @ls)
