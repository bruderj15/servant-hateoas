module Servant.Hateoas.Resource where

import Servant
import Data.Kind
import GHC.TypeLits
import GHC.Records

class HasResource ct where
  type Resource ct :: Type -> Type

class HasResource ct => ToResource ct api a where
  toResource :: Proxy ct -> Proxy api -> a -> Resource ct a

-- | (relationName, fieldName, Api for fields value)
type Relation = (Symbol, Symbol, Type) :: Type

class Resty a where
  type Id a :: Type
  getId :: a -> Id a

  type GetOneApi a :: Type

  type CollectionName a :: Symbol
  type CollectionName a = "items"

  type Relations a :: Relation

type IsResty api a = (Resty a, HasLink (GetOneApi a), MkLink (GetOneApi a) Link ~ (Id a -> Link), IsElem (GetOneApi a) api)

selfLink :: forall api a. IsResty api a => Proxy api -> a -> (String, Link)
selfLink api x = ("self", mkSelf $ getId x)
  where
    mkSelf = safeLink api (Proxy @(GetOneApi a))

defaultLink :: forall api a relName fieldName endpoint b. ( Resty a, '(relName, fieldName, endpoint) ~ Relations a, KnownSymbol relName
                , HasField fieldName a b, HasLink endpoint, IsElem endpoint api, MkLink endpoint Link ~ (b -> Link)
                ) => Proxy api -> a -> [(String, Link)]
defaultLink api x = [(symbolVal (Proxy @relName), mkLink $ getField @fieldName x)]
  where
    mkLink = safeLink api (Proxy @endpoint)
