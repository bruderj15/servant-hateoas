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

class MkResourcy world api (c :: Type -> Constraint) (c' :: Type -> Constraint) where
  mkResource :: (c (Server api), c' (Server (Resourcify api (HAL JSON))))
    => Proxy c -> Proxy c' -> Proxy world -> Proxy api -> Server (Resourcify api (HAL JSON))

-- instance {-# OVERLAPPABLE #-} (MkResourcy world a c c', MkResourcy world b d d') => MkResourcy world (a :<|> b) h h' where
  -- mkResource world c c' _ = mkResource world c c' (Proxy @a) :<|> mkResource world c c' (Proxy @b)

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => MkResourcy world api ((~) (Handler x)) ((~) (Handler (HALResource x))) where
  mkResource _ _ _ = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) . getHandler

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => MkResourcy world api ((~) (t -> Handler x)) ((~) (t -> Handler (HALResource x))) where
  mkResource _ _ _ api = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) . plainHandler
    where
      plainHandler = getHandler api

instance (HasHandler api, ToResource (Resourcify world (HAL JSON)) HALResource x)
  => MkResourcy world api ((~) (p -> q -> r -> Handler x)) ((~) (p -> q -> r -> Handler (HALResource x))) where
  mkResource _ _ _ api p q r = fmap (toResource @(Resourcify world (HAL JSON)) @HALResource) $ plainHandler p q r
    where
      plainHandler = getHandler api
