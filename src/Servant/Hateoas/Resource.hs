module Servant.Hateoas.Resource where

import Servant
import Data.Kind

class HasResource ct where
  type Resource ct :: Type -> Type

class HasResource ct => ToResource ct api a where
  toResource :: Proxy ct -> Proxy api -> a -> Resource ct a

class Resty a where
  type Id a :: Type
  type GetOneApi a :: Type
  getId :: a -> Id a

type IsResty api a = (Resty a, HasLink (GetOneApi a), MkLink (GetOneApi a) Link ~ (Id a -> Link), IsElem (GetOneApi a) api)

selfLink :: forall api a. IsResty api a => Proxy api -> a -> (String, Link)
selfLink api x = ("self", mkSelf $ getId x)
  where
    mkSelf = safeLink api (Proxy @(GetOneApi a))
