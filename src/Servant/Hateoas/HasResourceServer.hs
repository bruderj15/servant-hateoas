{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Servant.Hateoas.HasResourceServer where

import Servant
import Servant.Hateoas.Rewrite
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler

class HasResourceServer world api server m ct where
  getResourceServer ::
    ( Monad m
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
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (m p) m ct where
  getResourceServer m _ _ api _ = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> m p) m ct where
  getResourceServer m _ _ api _ q = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> r -> m p) m ct where
  getResourceServer m _ _ api _ q r = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q r

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (q -> r -> s -> m p) m ct where
  getResourceServer m _ _ api _ q r s = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api q r s
