{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.HasResourceServer where

import Servant
import Servant.Hateoas.Rewrite
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler
import Servant.Hateoas.ContentType.HAL

-- TODO: We could get rid of @serverResourceApi@ by rewriting @serverApi@ and asserting:
-- @ServerT (Resourcify api (HAL JSON)) m ~ ~ rewrittenServerApi@
class HasResourceServer world api serverApi serverResourceApi where
  getResourceServer ::
    ( Monad m
    , ServerT api m ~ serverApi
    , ServerT (Resourcify api (HAL JSON)) m ~ serverResourceApi
    ) => Proxy m -> Proxy world -> Proxy api -> Proxy serverApi -> Proxy serverResourceApi -> ServerT (Resourcify api (HAL JSON)) m

instance {-# OVERLAPPABLE #-}
  ( HasResourceServer world a aServerApi aServerResourceApi
  , HasResourceServer world b bServerApi bServerResourceApi
  ) => HasResourceServer world (a :<|> b) (aServerApi :<|> bServerApi) (aServerResourceApi :<|> bServerResourceApi) where
  getResourceServer m _ _ _ _ =
         getResourceServer m (Proxy @world) (Proxy @a) (Proxy @aServerApi) (Proxy @aServerResourceApi)
    :<|> getResourceServer m (Proxy @world) (Proxy @b) (Proxy @bServerApi) (Proxy @bServerResourceApi)

-- TODO: There surely is a hack for the instances with differing arity here.
-- Probably an instance for (a -> b) where b also has an instance...?
instance (Monad m, HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (m x) (m (HALResource x)) where
  getResourceServer m _ api _ _ = toResource @(Resourcify world (HAL JSON)) @HALResource <$> getHandler m api

instance (Monad m, HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (p -> m x) (p -> m (HALResource x)) where
  getResourceServer m _ api _ _ p = toResource @(Resourcify world (HAL JSON)) @HALResource <$> plainHandler p
    where
      plainHandler = getHandler m api

instance (Monad m, HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (p -> q -> m x) (p -> q -> m (HALResource x)) where
  getResourceServer m _ api _ _ p q = toResource @(Resourcify world (HAL JSON)) @HALResource <$> plainHandler p q
    where
      plainHandler = getHandler m api

instance (Monad m, HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => HasResourceServer world api (p -> q -> r -> m x) (p -> q -> r -> m (HALResource x)) where
  getResourceServer m _ api _ _ p q r = toResource @(Resourcify world (HAL JSON)) @HALResource <$> plainHandler p q r
    where
      plainHandler = getHandler m api
