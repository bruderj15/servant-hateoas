{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Resource
(
  -- * Resource
  HasResource(..)
, ToResource(..)

-- * Hypermedia-Relations
-- ** Type
, HRel(..)

-- ** Class
, Related(..)

-- ** Construction
, BuildRels(..)
, selfLink, relatedLinks, defaultLinks
)
where

import Servant
import Data.Kind
import GHC.TypeLits
import GHC.Records

-- | Class that indicates that a Content-Type has a specific Resource-Representation.
class HasResource ct where
  -- | Associated type for this Content-Type
  type Resource ct :: Type -> Type

-- | Class for converting values of @a@ to their respective Resource-Representation.
class HasResource ct => ToResource ct api a where
  -- | Converts a value into it's Resource-Representation.
  toResource :: Proxy ct -> Proxy api -> a -> Resource ct a

-- | Data-Kind for Hypermedia-Relations.
data HRel = HRel
  { relName  :: Symbol       -- ^ Name of the relation
  , selName  :: Symbol       -- ^ Record selectors field name
  , endpoint :: Type         -- ^ Servant-Endpoint to use for retrieving one value for relation @relName@
  }

-- | Types that have Hypermedia-Relations.
class Related a where
  type IdSelName a      :: Symbol       -- ^ Name of the record selector that holds the resources identifier
  type GetOneApi a      :: Type         -- ^ Servant-Endpoint for retrieving one @a@ by its identifier
  type CollectionName a :: Symbol       -- ^ Name for collected values, defaults to @\"item\"@
  type CollectionName a = "items"
  type Relations a      :: [HRel]       -- ^ List of all relations @a@ has

-- | Class for deriving Hypermedia-Relations for types.
type BuildRels :: Type -> [HRel] -> Type -> Constraint
class BuildRels api rs a where
  buildRels :: Proxy rs -> Proxy api -> a -> [(String, Link)]

instance BuildRels api '[] a where
  buildRels _ _ _ = []

instance
  ( KnownSymbol relName
  , HasField selName a id
  , HasLink endpoint
  , IsElem endpoint api
  , MkLink endpoint Link ~ (id -> Link)
  , BuildRels api rs a
  ) => BuildRels api (('HRel relName selName endpoint) ': rs) a where
  buildRels _ api x = l : buildRels (Proxy @rs) api x
    where
      mkLink = safeLink api (Proxy @endpoint)
      l = (symbolVal (Proxy @relName), mkLink $ getField @selName x)

-- | Generates pairs @(rel, link)@ for all related resources as defined with 'Relations'.
relatedLinks :: forall api a. (Related a, BuildRels api (Relations a) a) => Proxy api -> a -> [(String, Link)]
relatedLinks = buildRels (Proxy @(Relations a))

-- | Generates the pair (\"self\", link) where @link@ is the 'Link' to @a@ itself.
selfLink :: forall api a id.
  ( Related a, HasField (IdSelName a) a id
  , IsElem (GetOneApi a) api, HasLink (GetOneApi a)
  , MkLink (GetOneApi a) Link ~ (id -> Link)
  ) => Proxy api -> a -> (String, Link)
selfLink api x = ("self", mkSelf $ getField @(IdSelName a) x)
  where
    mkSelf = safeLink api (Proxy @(GetOneApi a))

-- | Generate Hypermedia-Links by default.
--
-- @
-- defaultLinks api x = selfLink api x : relatedLinks api x
-- @
defaultLinks :: forall api a id.
  ( Related a, HasField (IdSelName a) a id, IsElem (GetOneApi a) api
  , HasLink (GetOneApi a), MkLink (GetOneApi a) Link ~ (id -> Link)
  , BuildRels api (Relations a) a
  ) => Proxy api -> a -> [(String, Link)]
defaultLinks api x = selfLink api x : relatedLinks api x
