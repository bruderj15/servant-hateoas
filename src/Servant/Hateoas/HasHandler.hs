module Servant.Hateoas.HasHandler where

import Servant
import Control.Monad.IO.Class
import GHC.TypeLits

class HasHandler api where
  getHandler :: MonadIO m => Proxy m -> Proxy api -> ServerT api m

instance {-# OVERLAPPABLE #-} (HasHandler a, HasHandler b) => HasHandler (a :<|> b) where
  getHandler m _ = getHandler m (Proxy @a) :<|> getHandler m (Proxy @b)

-- TODO: Instances like this for all other combinators that do not change the arity of the resulting handler-function
instance {-# OVERLAPPABLE #-} HasHandler b => HasHandler ((a :: Symbol) :> b) where
  getHandler m _ = getHandler m (Proxy @b)

instance HasHandler EmptyAPI where
  getHandler _ _ = emptyServer
