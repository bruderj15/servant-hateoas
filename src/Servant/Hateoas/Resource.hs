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

data HRel = HRel { relName :: Symbol, fieldName :: Symbol, endpoint :: Type }

class Related a where
  type IdField a :: Symbol
  type GetOneApi a :: Type
  type CollectionName a :: Symbol
  type CollectionName a = "items"
  type Relations a :: [HRel]

type BuildRels :: Type -> [HRel] -> Type -> Constraint
class BuildRels api rs a where
  buildRels :: Proxy rs -> Proxy api -> a -> [(String, Link)]

instance BuildRels api '[] a where
  buildRels _ _ _ = []

instance
  ( KnownSymbol relName
  , HasField fieldName a id
  , HasLink endpoint
  , IsElem endpoint api
  , MkLink endpoint Link ~ (id -> Link)
  , BuildRels api rs a
  ) => BuildRels api (('HRel relName fieldName endpoint) ': rs) a where
  buildRels _ api x = l : buildRels (Proxy @rs) api x
    where
      mkLink = safeLink api (Proxy @endpoint)
      l = (symbolVal (Proxy @relName), mkLink $ getField @fieldName x)

relatedLinks :: forall api a. (Related a, BuildRels api (Relations a) a) => Proxy api -> a -> [(String, Link)]
relatedLinks = buildRels (Proxy @(Relations a))

selfLink :: forall api a id.
  ( Related a, HasField (IdField a) a id
  , IsElem (GetOneApi a) api, HasLink (GetOneApi a)
  , MkLink (GetOneApi a) Link ~ (id -> Link)
  ) => Proxy api -> a -> (String, Link)
selfLink api x = ("self", mkSelf $ getField @(IdField a) x)
  where
    mkSelf = safeLink api (Proxy @(GetOneApi a))

defaultLinks :: forall api a id.
  ( Related a, HasField (IdField a) a id, IsElem (GetOneApi a) api
  , HasLink (GetOneApi a), MkLink (GetOneApi a) Link ~ (id -> Link)
  , BuildRels api (Relations a) a
  ) => Proxy api -> a -> [(String, Link)]
defaultLinks api x = selfLink api x : relatedLinks api x
