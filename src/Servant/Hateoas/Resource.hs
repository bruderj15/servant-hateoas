{-# LANGUAGE UndecidableInstances #-}

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

  type Relations a :: [Relation]

type IsResty api a = (Resty a, HasLink (GetOneApi a), MkLink (GetOneApi a) Link ~ (Id a -> Link), IsElem (GetOneApi a) api)

selfLink :: forall api a. IsResty api a => Proxy api -> a -> (String, Link)
selfLink api x = ("self", mkSelf $ getId x)
  where
    mkSelf = safeLink api (Proxy @(GetOneApi a))

type BuildLinks :: Type -> [Relation] -> Type -> Constraint
class BuildLinks api rs a where
  buildLinks :: Proxy api -> Proxy rs -> a -> [(String, Link)]

instance BuildLinks api '[] a where
  buildLinks _ _ _ = []

instance
  ( KnownSymbol relName
  , HasField fieldName a id
  , HasLink endpoint
  , IsElem endpoint api
  , MkLink endpoint Link ~ (id -> Link)
  , BuildLinks api rs a
  ) => BuildLinks api ('(relName, fieldName, endpoint) ': rs) a where
  buildLinks api _ x = l : buildLinks api (Proxy @rs) x
    where
      mkLink = safeLink api (Proxy @endpoint)
      l = (symbolVal (Proxy @relName), mkLink $ getField @fieldName x)

defaultLinks :: forall api a. (Resty a, BuildLinks api (Relations a) a) => Proxy api -> a -> [(String, Link)]
defaultLinks api x = buildLinks api (Proxy @(Relations a)) x
