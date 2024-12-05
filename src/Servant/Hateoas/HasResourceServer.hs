{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.HasResourceServer where

import Servant
import Servant.Hateoas.Rewrite
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler
import Servant.Hateoas.ContentType.HAL

class HasResourceServer world api server m where
  getResourceServer ::
    ( Monad m
    , ServerT api m ~ server
    , ServerT (Resourcify api (HAL JSON)) m ~ ResourcifyServer server (HAL JSON) m
    ) => Proxy m -> Proxy world -> Proxy api -> Proxy server -> ServerT (Resourcify api (HAL JSON)) m

instance {-# OVERLAPPABLE #-}
  ( HasResourceServer world a aServerApi m
  , HasResourceServer world b bServerApi m
  ) => HasResourceServer world (a :<|> b) (aServerApi :<|> bServerApi) m where
  getResourceServer m _ _ _ =
         getResourceServer m (Proxy @world) (Proxy @a) (Proxy @aServerApi)
    :<|> getResourceServer m (Proxy @world) (Proxy @b) (Proxy @bServerApi)

-- TODO: There surely is a hack for the instances with differing arity here.
-- Probably an instance for (a -> b) where b also has an instance...?
instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world (HAL JSON)) HALResource x
  , ResourcifyServer (m x) (HAL JSON) m ~ m (HALResource x)
  ) => HasResourceServer world api (m x) m where
  getResourceServer m _ api _ = toResource @(Resourcify world (HAL JSON)) @HALResource <$> getHandler m api

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world (HAL JSON)) HALResource x
  , ResourcifyServer (p -> m x) (HAL JSON) m ~ (p -> m (HALResource x))
  ) => HasResourceServer world api (p -> m x) m where
  getResourceServer m _ api _ p = toResource @(Resourcify world (HAL JSON)) @HALResource <$> plainHandler p
    where
      plainHandler = getHandler m api

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world (HAL JSON)) HALResource x
  , ResourcifyServer (p -> q -> m x) (HAL JSON) m ~ (p -> q -> m (HALResource x))
  ) => HasResourceServer world api (p -> q -> m x) m where
  getResourceServer m _ api _ p q = toResource @(Resourcify world (HAL JSON)) @HALResource <$> plainHandler p q
    where
      plainHandler = getHandler m api

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world (HAL JSON)) HALResource x
  , ResourcifyServer (p -> q -> r -> m x) (HAL JSON) m ~ (p -> q -> r -> m (HALResource x))
  ) => HasResourceServer world api (p -> q -> r -> m x) m where
  getResourceServer m _ api _ p q r = toResource @(Resourcify world (HAL JSON)) @HALResource <$> plainHandler p q r
    where
      plainHandler = getHandler m api
