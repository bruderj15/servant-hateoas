{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

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

class HasResourceServer api server m ct where
  getResourceServer ::
    ( MonadIO m
    , HasHandler api
    , ServerT api m ~ server
    , ServerT (Resourcify api ct) m ~ ResourcifyServer server ct m
    ) => Proxy m -> Proxy ct -> Proxy api -> Proxy server -> ServerT (Resourcify api ct) m

instance {-# OVERLAPPING #-}
  ( HasResourceServer a aServer m ct, HasHandler a
  , HasResourceServer b bServer m ct, HasHandler b
  ) => HasResourceServer (a :<|> b) (aServer :<|> bServer) m ct where
  getResourceServer m ct _ _ = getResourceServer m ct (Proxy @a) (Proxy @aServer) :<|> getResourceServer m ct (Proxy @b) (Proxy @bServer)

instance
  ( DotDotDot server (IsFun server)
  , Replace server (m (res a)) (IsFun server) ~ ResourcifyServer server ct m
  , Return server (IsFun server) ~ m a
  , res ~ MkResource ct
  , ToResource res a
  ) => HasResourceServer api server m ct where
  getResourceServer m _ api _ = fmap (toResource (Proxy @(MkResource ct))) ... getHandler m api

instance {-# OVERLAPPING #-}
  ( l ~ 'Layer api cs
  , rApi ~ Resourcify api ct
  , rServer ~ ResourcifyServer server ct m
  , res ~ MkResource ct
  , Resource res
  , HasLink rApi
  , IsElem rApi rApi
  , BuildLayerLinks (Resourcify l ct) rServer
  , DotDotDot (ReplaceHandler rServer [(String, Link)]) (IsFun (ReplaceHandler rServer [(String, Link)]))
  , Return (ReplaceHandler rServer [(String, Link)]) (IsFun (ReplaceHandler rServer [(String, Link)])) ~ [(String, Link)]
  , Replace (ReplaceHandler rServer [(String, Link)]) (m (res Intermediate)) (IsFun (ReplaceHandler rServer [(String, Link)])) ~ rServer
  ) => HasResourceServer ('Layer api cs) server m ct where
  getResourceServer _ _ _ _ = (return @m . foldr addLink (wrap @res $ Intermediate ())) ... buildLayerLinks (Proxy @(Resourcify l ct)) (Proxy @rServer)

instance {-# OVERLAPPING #-} HasResourceServer ('[] :: [Layer]) (Tagged m EmptyServer) m ct where
  getResourceServer _ _ _ _ = emptyServer

instance {-# OVERLAPPING #-}
  ( HasResourceServer ls (ServerT ls m) m ct
  , HasResourceServer l (ServerT l m) m ct
  , HasHandler l
  , HasHandler ls
  , IsElem (NodeApi l) (NodeApi l)
  , HasLink (NodeApi l)
  , BuildLayerLinks (Resourcify l ct) (ResourcifyServer (ServerT l m) ct m)
  , MonadIO m
  ) => HasResourceServer (l ': ls) server m ct where
  getResourceServer m ct _ _ = getResourceServer m ct (Proxy @l) (Proxy @(ServerT l m)) :<|> getResourceServer m ct (Proxy @ls) (Proxy @(ServerT ls m))
