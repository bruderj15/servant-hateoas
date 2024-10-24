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
  type IdField a :: Symbol
  type GetOneApi a :: Type
  type CollectionName a :: Symbol
  type CollectionName a = "items"
  type Relations a :: [Relation]

type BuildLinks :: Type -> [Relation] -> Type -> Constraint
class BuildLinks api rs a where
  buildLinks :: Proxy rs -> Proxy api -> a -> [(String, Link)]

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
  buildLinks _ api x = l : buildLinks (Proxy @rs) api x
    where
      mkLink = safeLink api (Proxy @endpoint)
      l = (symbolVal (Proxy @relName), mkLink $ getField @fieldName x)

relatedLinks :: forall api a. (Resty a, BuildLinks api (Relations a) a) => Proxy api -> a -> [(String, Link)]
relatedLinks = buildLinks (Proxy @(Relations a))

selfLink :: forall api a id. (Resty a, HasField (IdField a) a id, IsElem (GetOneApi a) api, HasLink (GetOneApi a), MkLink (GetOneApi a) Link ~ (id -> Link))
  => Proxy api -> a -> (String, Link)
selfLink api x = ("self", mkSelf $ getField @(IdField a) x)
  where
    mkSelf = safeLink api (Proxy @(GetOneApi a))

defaultLinks :: forall api a id.
  ( Resty a, HasField (IdField a) a id, IsElem (GetOneApi a) api
  , HasLink (GetOneApi a), MkLink (GetOneApi a) Link ~ (id -> Link)
  , BuildLinks api (Relations a) a
  ) => Proxy api -> a -> [(String, Link)]
defaultLinks api x = selfLink api x : relatedLinks api x
