{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.HasResourceServer where

import Servant
import Servant.Hateoas.Rewrite
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler
import Servant.Hateoas.ContentType.HAL

class HasResourceServer world api serverApi serverResourceApi where
  getResourceServer ::
    (Server api ~ serverApi
    , serverResourceApi ~ (Server (Resourcify api (HAL JSON)))
    ) => Proxy world -> Proxy api -> Proxy serverApi -> Proxy serverResourceApi -> Server (Resourcify api (HAL JSON))

instance {-# OVERLAPPABLE #-}
  ( HasResourceServer world a aServerApi aServerResourceApi
    , HasResourceServer world b bServerApi bServerResourceApi
  ) => HasResourceServer world (a :<|> b) (aServerApi :<|> bServerApi) (aServerResourceApi :<|> bServerResourceApi) where
  getResourceServer _ _ _ _ = getResourceServer (Proxy @world) (Proxy @a) (Proxy @aServerApi) (Proxy @aServerResourceApi)
                  :<|> getResourceServer (Proxy @world) (Proxy @b) (Proxy @bServerApi) (Proxy @bServerResourceApi)

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (Handler x) (Handler (HALResource x)) where
  getResourceServer _ api _ _ = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ getHandler api

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (p -> Handler x) (p -> Handler (HALResource x)) where
  getResourceServer _ api _ _ = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) . plainHandler
    where
      plainHandler = getHandler api

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (p -> q -> Handler x) (p -> q -> Handler (HALResource x)) where
  getResourceServer _ api _ _ p q = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ plainHandler p q
    where
      plainHandler = getHandler api

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (p -> q -> r -> Handler x) (p -> q -> r -> Handler (HALResource x)) where
  getResourceServer _ api _ _ p q r = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ plainHandler p q r
    where
      plainHandler = getHandler api
