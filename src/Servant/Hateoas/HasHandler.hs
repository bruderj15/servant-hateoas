{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Servant.Hateoas.HasHandler where

import Servant
import Servant.Hateoas.Rewrite
import Servant.Hateoas.Resource
import Servant.Hateoas.ContentType.HAL
import Data.Kind
import GHC.TypeLits

class HasHandler api where
  getHandler :: Proxy api -> Server api

instance {-# OVERLAPPABLE #-} (HasHandler a, HasHandler b) => HasHandler (a :<|> b) where
  getHandler _ = getHandler (Proxy @a) :<|> getHandler (Proxy @b)

-- TODO: Instances like this for all other combinators that do not change the arity of the resulting handler-function
--       Instances for default handlers: EmptyAPI, ...
instance {-# OVERLAPPABLE #-} HasHandler b => HasHandler ((a :: Symbol) :> b) where
  getHandler _ = getHandler (Proxy @b)

class MkResourcy world api serverApi serverResourceApi where
  mkResource ::
    (Server api ~ serverApi
    , serverResourceApi ~ (Server (Resourcify api (HAL JSON)))
    ) => Proxy world -> Proxy api -> Proxy serverApi -> Proxy serverResourceApi -> Server (Resourcify api (HAL JSON))

instance {-# OVERLAPPABLE #-}
  ( MkResourcy world a aServerApi aServerResourceApi
  , MkResourcy world b bServerApi bServerResourceApi
  ) => MkResourcy world (a :<|> b) (aServerApi :<|> bServerApi) (aServerResourceApi :<|> bServerResourceApi) where
  mkResource _ _ _ _ = mkResource (Proxy @world) (Proxy @a) (Proxy @aServerApi) (Proxy @aServerResourceApi)
                  :<|> mkResource (Proxy @world) (Proxy @b) (Proxy @bServerApi) (Proxy @bServerResourceApi)

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => MkResourcy world api (Handler x) (Handler (HALResource x)) where
  mkResource _ api _ _ = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ getHandler api

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => MkResourcy world api (p -> Handler x) (p -> Handler (HALResource x)) where
  mkResource _ api _ _ = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) . plainHandler
    where
      plainHandler = getHandler api

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => MkResourcy world api (p -> q -> Handler x) (p -> q -> Handler (HALResource x)) where
  mkResource _ api _ _ p q = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ plainHandler p q
    where
      plainHandler = getHandler api

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => MkResourcy world api (p -> q -> r -> Handler x) (p -> q -> r -> Handler (HALResource x)) where
  mkResource _ api _ _ p q r = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ plainHandler p q r
    where
      plainHandler = getHandler api
