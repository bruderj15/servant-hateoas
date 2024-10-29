{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Servant.Hateoas.Resource
(
  -- * Resource
  -- ** Modification
  Resource(..), EmbeddingResource(..), CollectingResource(..)

  -- ** Construction
, ToResource(..)
, ToCollection(..)

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
import Data.Aeson
import GHC.TypeLits
import GHC.Records

-- | Class for resources that carry Hypermedia-Relations.
class Resource res where
  -- | Add a relation @(rel, link)@ to a resource.
  addLink :: (String, Link) -> res a -> res a

-- | Class for 'Resource's that can embed other resources.
class Resource res => EmbeddingResource res where
  -- | Embed a resource @b@ with its relation @rel@ as tuple @(rel, b)@.
  embed :: ToJSON b => (String, b) -> res a -> res a

-- | Class for 'Resource's that can collect multiple resources.
class Resource res => CollectingResource res where
  -- | Collect a resource into the collection.
  collect :: a -> res a -> res a

-- | Class for converting values of @a@ to their respective Resource-Representation.
class ToResource api res a where
  -- | Converts a value into it's Resource-Representation.
  toResource :: a -> res a
  toResource = toResource' (Proxy @api) (Proxy @res)

  -- | Like 'toResource' but takes proxies for ambiguity.
  toResource' :: Proxy api -> Proxy res -> a -> res a
  toResource' _ _ = toResource @api @res
  {-# MINIMAL toResource | toResource' #-}

-- | Class for converting multiple values of @a@ to their respective collection-like representation.
class ToCollection api res a where
  -- | Converts a many values into their Collection-Representation.
  toCollection :: Foldable f => f a -> res a
  toCollection = toCollection' (Proxy @api) (Proxy @res)

  -- | Like 'toCollection' but takes proxies for ambiguity.
  toCollection' :: Foldable f => Proxy api -> Proxy res -> f a -> res a
  toCollection' _ _ = toCollection @api @res
  {-# MINIMAL toCollection | toCollection' #-}

-- | Data-Kind for Hypermedia-Relations.
data HRel = HRel
  { relName  :: Symbol       -- ^ Name of the relation
  , selName  :: Symbol       -- ^ Record selectors field name
  , endpoint :: Type         -- ^ Servant-Endpoint to use for retrieving one value for relation @relName@
  }

-- | Types that have Hypermedia-Relations.
class Related a where
  -- | Name of the record selector that holds the resources identifier
  type IdSelName a      :: Symbol
  -- | Servant-Endpoint for retrieving one @a@ by its identifier
  type GetOneApi a      :: Type
  -- | Name for collected values
  type CollectionName a :: Symbol
  type CollectionName a = "items"
  -- | List of all relations @a@ has
  type Relations a      :: [HRel]

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

-- | Generates the pair @(\"self\", link)@ where @link@ is the 'Link' to @a@ itself.
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
