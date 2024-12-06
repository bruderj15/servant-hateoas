{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Servant.Hateoas.HasResourceServer where

import Servant
import Servant.Hateoas.Rewrite
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler
import Control.Monad.IO.Class

class HasResourceServer world api server m ct where
  getResourceServer ::
    ( MonadIO m
    , ServerT api m ~ server
    , ServerT (Resourcify api ct) m ~ ResourcifyServer server ct m
    ) => Proxy m -> Proxy ct -> Proxy world -> Proxy api -> Proxy server -> ServerT (Resourcify api ct) m

instance {-# OVERLAPPABLE #-}
  ( HasResourceServer world a aServer m ct
  , HasResourceServer world b bServer m ct
  ) => HasResourceServer world (a :<|> b) (aServer :<|> bServer) m ct where
  getResourceServer m ct world _ _ =
         getResourceServer m ct world (Proxy @a) (Proxy @aServer)
    :<|> getResourceServer m ct world (Proxy @b) (Proxy @bServer)

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (m p) m ct where
  getResourceServer m _ _ api _ = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> m p) m ct where
  getResourceServer m _ _ api _ q = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> r -> m p) m ct where
  getResourceServer m _ _ api _ q r = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q r

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> r -> s -> m p) m ct where
  getResourceServer m _ _ api _ q r s = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q r s

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> r -> s -> t -> m p) m ct where
  getResourceServer m _ _ api _ q r s t = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q r s t

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> r -> s -> t -> u -> m p) m ct where
  getResourceServer m _ _ api _ q r s t u = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q r s t u

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> r -> s -> t -> u -> v -> m p) m ct where
  getResourceServer m _ _ api _ q r s t u v = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q r s t u v

instance
  ( MonadIO m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> r -> s -> t -> u -> v -> w -> m p) m ct where
  getResourceServer m _ _ api _ q r s t u v w = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q r s t u v w
