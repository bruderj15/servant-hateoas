{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Servant.Hateoas.Resource
( Resource(..)
, HasRestyApi(..)
, ToResource(..)
, defaultToResource
, addRel, addRels
) where

import Servant.Hateoas.ContentType
import Servant.API.ContentTypes
import Servant.Links
import Data.Aeson
import Data.Proxy
import Data.Kind
import GHC.Generics
import GHC.Exts

-- | RESTy Resource representation of a datatype.
data Resource a = Resource
  { resource :: a
  , links    :: [(String, Link)]
  } deriving (Show, Generic)

-- Add a relation to another resource.
addRel :: (String, Link) -> Resource a -> Resource a
addRel l (Resource x ls) = Resource x (l:ls)
{-# INLINE addRel #-}

-- Add multiple relations to other resources.
addRels :: [(String, Link)] -> Resource a -> Resource a
addRels ls' (Resource x ls) = Resource x $ ls' ++ ls
{-# INLINE addRels #-}

instance ToJSON a => MimeRender HALJSON (Resource a) where
  mimeRender _ (Resource res lks) = encode $ case toJSON res of
    Object kvm -> Object $ ["_links" .= lks'] <> kvm
    v -> v
    where
      lks' = object [fromString rel .= object ["href" .= linkURI href] | (rel, href) <- lks]

-- | Datatypes that have a resty api.
class HasRestyApi a where
  -- | Servants Api-Type (endpoint) for fetching one value of the datatype by its identifier.
  type GetOneApi a :: Type

  -- | Type of the identifier for values of the datatype.
  type Id a :: Type

  -- | Function for getting the identifier from a value of the datatype.
  getId :: a -> Id a

-- | Checks if a constraint holds.
type family Holds (c :: Constraint) :: Bool where
  Holds () = True
  Holds _  = False

-- | Datatypes that can be represented as 'Resource's.
class ToResource api a where
  -- | Creates the resource representation of a value.
  toResource :: Proxy api -> a -> Resource a
  default toResource :: (Generic a, GToResource api (Rep a), HasRestyApi a, HasLink (GetOneApi a)
    , IsElem (GetOneApi a) api, MkLink (GetOneApi a) Link ~ (Id a -> Link))
    => Proxy api
    -> a
    -> Resource a
  toResource = defaultToResource

-- | The default implementation for 'toResource'.
--
-- Adds a link to the self and links to all resty fields.
defaultToResource :: forall api a. (Generic a, GToResource api (Rep a), HasRestyApi a, HasLink (GetOneApi a)
  , IsElem (GetOneApi a) api, MkLink (GetOneApi a) Link ~ (Id a -> Link))
  => Proxy api
  -> a
  -> Resource a
defaultToResource api x = Resource x $ ("self", self $ getId x):(gToResource api (from x))
  where
    self = safeLink api $ Proxy @(GetOneApi a)

class GToResource api f where
  gToResource :: Proxy api -> f p -> [(String, Link)]

instance GToResource api U1 where
  gToResource _ _ =  mempty

instance GToResource api V1 where
  gToResource _ _ =  mempty

instance (GToResource api f, GToResource api g) => GToResource api (f :*: g) where
  gToResource api (a :*: b) =  gToResource api a <> gToResource api b

instance GToResource api f => GToResource api (M1 i c f) where
  gToResource api (M1 x) = gToResource api x

instance {-# OVERLAPPING #-} (GToResource api f, Selector c) => GToResource api (M1 S c f) where
  gToResource api m1@(M1 x) = (\(_, link) -> (selName m1, link)) <$> (gToResource api x)

-- Dear GHC, why is the 'f ~ K1 i a' trick needed here?
instance {-# OVERLAPPABLE #-} (GToResource' (Holds (HasRestyApi a)) api f, f ~ K1 i a) => GToResource api f where
  gToResource = gToResourceHelper @(Holds (HasRestyApi a))

-- | Helper for choosing generic implementation on the type-level absed on param @hasResource@.
class GToResource' hasResource api f where
  gToResourceHelper :: Proxy api -> f p -> [(String, Link)]

instance (HasRestyApi a, HasLink (GetOneApi a), IsElem (GetOneApi a) api, MkLink (GetOneApi a) Link ~ (Id a -> Link))
  => GToResource' 'True api (K1 i a) where
  gToResourceHelper api (K1 x) = pure (mempty, link $ getId x)
    where link = safeLink api $ Proxy @(GetOneApi a)

instance GToResource' 'False api (K1 i a) where
  gToResourceHelper _ _ = mempty
