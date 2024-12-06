{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.HasResourceServer where

import Servant
import Servant.Hateoas.Rewrite
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler
import Control.Monad.IO.Class

class HasResourceServer api server m ct where
  getResourceServer ::
    ( MonadIO m
    , ServerT api m ~ server
    , ServerT (Resourcify api ct) m ~ ResourcifyServer server ct m
    ) => Proxy m -> Proxy ct -> Proxy api -> Proxy server -> ServerT (Resourcify api ct) m

instance {-# OVERLAPPABLE #-}
  ( HasResourceServer a aServer m ct
  , HasResourceServer b bServer m ct
  ) => HasResourceServer (a :<|> b) (aServer :<|> bServer) m ct where
  getResourceServer m ct _ _ =
         getResourceServer m ct (Proxy @a) (Proxy @aServer)
    :<|> getResourceServer m ct (Proxy @b) (Proxy @bServer)

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer api (m p) m ct where
  getResourceServer m _ api _ = toResource (Proxy @(MkResource ct)) <$> getHandler m api

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer api (q -> m p) m ct where
  getResourceServer m _ api _ q = toResource (Proxy @(MkResource ct)) <$> getHandler m api q

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer api (q -> r -> m p) m ct where
  getResourceServer m _ api _ q r = toResource (Proxy @(MkResource ct)) <$> getHandler m api q r

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer api (q -> r -> s -> m p) m ct where
  getResourceServer m _ api _ q r s = toResource (Proxy @(MkResource ct)) <$> getHandler m api q r s

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer api (q -> r -> s -> t -> m p) m ct where
  getResourceServer m _ api _ q r s t = toResource (Proxy @(MkResource ct)) <$> getHandler m api q r s t

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer api (q -> r -> s -> t -> u -> m p) m ct where
  getResourceServer m _ api _ q r s t u = toResource (Proxy @(MkResource ct)) <$> getHandler m api q r s t u

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer api (q -> r -> s -> t -> u -> v -> m p) m ct where
  getResourceServer m _ api _ q r s t u v = toResource (Proxy @(MkResource ct)) <$> getHandler m api q r s t u v

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer api (q -> r -> s -> t -> u -> v -> w -> m p) m ct where
  getResourceServer m _ api _ q r s t u v w = toResource (Proxy @(MkResource ct)) <$> getHandler m api q r s t u v w
