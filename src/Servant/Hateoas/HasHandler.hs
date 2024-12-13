module Servant.Hateoas.HasHandler where

import Servant
import Servant.Hateoas.Layer
import Control.Monad.IO.Class
import GHC.TypeLits

class HasHandler api where
  getHandler :: MonadIO m => Proxy m -> Proxy api -> ServerT api m

instance {-# OVERLAPPABLE #-} (HasHandler a, HasHandler b) => HasHandler (a :<|> b) where
  getHandler m _ = getHandler m (Proxy @a) :<|> getHandler m (Proxy @b)

instance {-# OVERLAPPABLE #-} HasHandler b => HasHandler ((a :: Symbol) :> b) where
  getHandler m _ = getHandler m (Proxy @b)

instance {-# OVERLAPPABLE #-} HasHandler b => HasHandler (Description sym :> b) where
  getHandler m _ = getHandler m (Proxy @b)

instance {-# OVERLAPPABLE #-} HasHandler b => HasHandler (Summary sym :> b) where
  getHandler m _ = getHandler m (Proxy @b)

instance {-# OVERLAPPABLE #-} HasHandler b => HasHandler (Fragment a :> b) where
  getHandler m _ = getHandler m (Proxy @b)

instance HasHandler EmptyAPI where
  getHandler _ _ = emptyServer

instance HasHandler ('[] :: [Layer]) where
  getHandler _ _ = emptyServer

instance (HasHandler l, HasHandler ls) => HasHandler ((l ': ls) :: [Layer]) where
  getHandler m _ = getHandler m (Proxy @l) :<|> getHandler m (Proxy @ls)

instance HasHandler api => HasHandler ('Layer api cs) where
  getHandler m _ = getHandler m (Proxy @api)

instance HasHandler (Verb m s ct ()) where
  getHandler _ _ = return ()
