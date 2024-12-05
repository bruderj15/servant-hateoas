{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.HasResourceServer where

import Servant
import Servant.Hateoas.Rewrite
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler
import Servant.Hateoas.ContentType.HAL

class HasResourceServer world api serverApi serverResourceApi where
  getResourceServer ::
    ( Monad m
    , ServerT api m ~ serverApi
    , serverResourceApi ~ (ServerT (Resourcify api (HAL JSON)) m)
    ) => Proxy m -> Proxy world -> Proxy api -> Proxy serverApi -> Proxy serverResourceApi -> ServerT (Resourcify api (HAL JSON)) m

instance {-# OVERLAPPABLE #-}
  ( HasResourceServer world a aServerApi aServerResourceApi
  , HasResourceServer world b bServerApi bServerResourceApi
  ) => HasResourceServer world (a :<|> b) (aServerApi :<|> bServerApi) (aServerResourceApi :<|> bServerResourceApi) where
  getResourceServer m _ _ _ _ = getResourceServer m (Proxy @world) (Proxy @a) (Proxy @aServerApi) (Proxy @aServerResourceApi)
                           :<|> getResourceServer m (Proxy @world) (Proxy @b) (Proxy @bServerApi) (Proxy @bServerResourceApi)

instance (Monad m, HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (m x) (m (HALResource x)) where
  getResourceServer m _ api _ _ = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ getHandler m api

instance (Monad m, HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (p -> m x) (p -> m (HALResource x)) where
  getResourceServer m _ api _ _ = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) . plainHandler
    where
      plainHandler = getHandler m api

instance (Monad m, HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (p -> q -> m x) (p -> q -> m (HALResource x)) where
  getResourceServer m _ api _ _ p q = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ plainHandler p q
    where
      plainHandler = getHandler m api

instance (Monad m, HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (p -> q -> r -> m x) (p -> q -> r -> m (HALResource x)) where
  getResourceServer m _ api _ _ p q r = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ plainHandler p q r
    where
      plainHandler = getHandler m api
