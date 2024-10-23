module Servant.Hateoas.Resource where

import Data.Proxy
import Data.Kind

class HasResource ct a where
  type Resource ct :: Type -> Type

class HasResource ct a => ToResource ct api a where
  toResource :: Proxy ct -> Proxy api -> a -> Resource ct a
