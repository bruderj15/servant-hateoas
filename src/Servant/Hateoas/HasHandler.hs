module Servant.Hateoas.HasHandler where

import Servant
import GHC.TypeLits

class HasHandler api where
  getHandler :: Proxy api -> Server api

instance {-# OVERLAPPABLE #-} (HasHandler a, HasHandler b) => HasHandler (a :<|> b) where
  getHandler _ = getHandler (Proxy @a) :<|> getHandler (Proxy @b)

-- TODO: Instances like this for all other combinators that do not change the arity of the resulting handler-function
--       Instances for default handlers: EmptyAPI, ...
instance {-# OVERLAPPABLE #-} HasHandler b => HasHandler ((a :: Symbol) :> b) where
  getHandler _ = getHandler (Proxy @b)

instance HasHandler EmptyAPI where
  getHandler _ = emptyServer
