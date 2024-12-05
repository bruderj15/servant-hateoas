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

-- TODO: There surely is a hack for the instances with differing arity here.
-- Probably an instance for (a -> b) where b also has an instance...?
instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) x
  , ResourcifyServer (m x) ct m ~ m ((MkResource ct) x)
  ) => HasResourceServer world api (m x) m ct where
  getResourceServer m _ _ api _ = toResource @(Resourcify world ct) @(MkResource ct) <$> getHandler m api

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) x
  , ResourcifyServer (p -> m x) ct m ~ (p -> m ((MkResource ct) x))
  ) => HasResourceServer world api (p -> m x) m ct where
  getResourceServer m _ _ api _ p = toResource @(Resourcify world ct) @(MkResource ct) <$> plainHandler p
    where
      plainHandler = getHandler m api

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) x
  , ResourcifyServer (p -> q -> m x) ct m ~ (p -> q -> m ((MkResource ct) x))
  ) => HasResourceServer world api (p -> q -> m x) m ct where
  getResourceServer m _ _ api _ p q = toResource @(Resourcify world ct) @(MkResource ct) <$> plainHandler p q
    where
      plainHandler = getHandler m api

instance
  ( Monad m
  , HasHandler api
  , ToResource (Resourcify world ct) (MkResource ct) x
  , ResourcifyServer (p -> q -> r -> m x) ct m ~ (p -> q -> r -> m ((MkResource ct) x))
  ) => HasResourceServer world api (p -> q -> r -> m x) m ct where
  getResourceServer m _ _ api _ p q r = toResource @(Resourcify world ct) @(MkResource ct) <$> plainHandler p q r
    where
      plainHandler = getHandler m api
