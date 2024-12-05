{-# LANGUAGE UndecidableInstances #-}

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
  ( HasResourceServer world a aServerApi m ct
  , HasResourceServer world b bServerApi m ct
  ) => HasResourceServer world (a :<|> b) (aServerApi :<|> bServerApi) m ct where
  getResourceServer m ct world _ _ =
         getResourceServer m ct world (Proxy @a) (Proxy @aServerApi)
    :<|> getResourceServer m ct world (Proxy @b) (Proxy @bServerApi)

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) p
  , ResourcifyServer (m p) ct m ~ m ((MkResource ct) p)
  ) => HasResourceServer world api (m p) m ct where
  getResourceServer m _ _ api _ = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api

instance
  ( Monad m
  , HasResourceServer world api tail m ct
  , ResourcifyServer tail ct m ~ ResourcifyServer tail ct m
  ) => HasResourceServer world api (p -> tail) m ct where
  getResourceServer m ct world api arity x = plainHandler x
    where
      plainHandler = getResourceServer m ct world api arity
