{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Servant.Hateoas.Resource where

import Servant.Hateoas.ContentType
import Servant.API.ContentTypes
import Servant.Links
import Data.Aeson
import Data.Proxy
import Data.Kind
import GHC.Generics
import GHC.Exts

data Resource a = Resource
  { resource :: a
  , links    :: [(String, Link)]
  } deriving (Show, Generic)

instance ToJSON a => MimeRender HALJSON (Resource a) where
  mimeRender _ (Resource res lks) = encode $ case toJSON res of
    Object kvm -> Object $ ["_links" .= lks'] <> kvm
    v -> v
    where
      lks' = object [fromString rel .= object ["href" .= linkURI href] | (rel, href) <- lks]

class HasResource a where
  type GetOneApi a :: Type
  type Id a :: Type
  getId :: a -> Id a

type family Holds (c :: Constraint) :: Bool where
  Holds () = True
  Holds _  = False

type family HasResourceInstance a where
  HasResourceInstance a = Holds (HasResource a)

class ToResource api a where
  toResource :: Proxy api -> a -> Resource a
  default toResource :: (Generic a, GToResource api (Rep a), HasResource a, HasLink (GetOneApi a)
    , IsElem (GetOneApi a) api, MkLink (GetOneApi a) Link ~ (Id a -> Link))
    => Proxy api
    -> a
    -> Resource a
  toResource = defaultToResource

defaultToResource :: forall api a. (Generic a, GToResource api (Rep a), HasResource a, HasLink (GetOneApi a)
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
instance {-# OVERLAPPABLE #-} (GToResource' (HasResourceInstance a) api f, f ~ K1 i a) => GToResource api f where
  gToResource = gToResourceHelper @(HasResourceInstance a)

class GToResource' hasResource api f where
  gToResourceHelper :: Proxy api -> f p -> [(String, Link)]

instance (HasResource a, HasLink (GetOneApi a), IsElem (GetOneApi a) api, MkLink (GetOneApi a) Link ~ (Id a -> Link))
  => GToResource' 'True api (K1 i a) where
  gToResourceHelper api (K1 x) = pure (mempty, link $ getId x)
    where link = safeLink api $ Proxy @(GetOneApi a)

instance GToResource' 'False api (K1 i a) where
  gToResourceHelper _ _ = mempty
