module Servant.Hateoas.HasHandler
(
  -- * Class
  HasHandler(..)
)
where

import Servant
import Data.Kind (Type)
import GHC.TypeLits

-- | Typeclass for extracting the handler of an API.
class HasHandler (m :: Type -> Type) (api :: k) where
  getHandler :: Proxy m -> Proxy api -> ServerT api m

instance {-# OVERLAPPABLE #-} (HasHandler m a, HasHandler m b) => HasHandler m (a :<|> b) where
  getHandler m _ = getHandler m (Proxy @a) :<|> getHandler m (Proxy @b)

instance {-# OVERLAPPABLE #-} HasHandler m b => HasHandler m ((a :: Symbol) :> b) where
  getHandler m _ = getHandler m (Proxy @b)

instance {-# OVERLAPPABLE #-} HasHandler m b => HasHandler m (Description sym :> b) where
  getHandler m _ = getHandler m (Proxy @b)

instance {-# OVERLAPPABLE #-} HasHandler m b => HasHandler m (Summary sym :> b) where
  getHandler m _ = getHandler m (Proxy @b)

instance {-# OVERLAPPABLE #-} HasHandler m b => HasHandler m (Fragment a :> b) where
  getHandler m _ = getHandler m (Proxy @b)

instance HasHandler m EmptyAPI where
  getHandler _ _ = emptyServer
