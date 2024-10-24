{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Servant.Hateoas.ContentType.HAL where

import Servant.Hateoas.Resource
import Servant.Hateoas.Some
import Servant.API.ContentTypes
import qualified Network.HTTP.Media as M
import Servant.Links
import Data.Kind
import Data.Proxy
import Data.Aeson
import Data.Aeson.KeyMap (singleton)
import GHC.Exts
import GHC.TypeLits
import GHC.Generics
import GHC.Records

data HAL (a :: Type)

data HALResource a = HALResource
  { resource :: a
  , links    :: [(String, Link)]
  , embedded :: [(String, SomeToJSON HALResource)]
  } deriving (Generic)

instance HasResource (HAL t) where
  type Resource (HAL t) = HALResource

instance Accept (HAL JSON) where
  contentType _ = "application" M.// "hal+json"

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (HALResource a) where
  toJSON (HALResource res ls es) = case toJSON res of
    Object kvm -> Object $ (singleton "_links" ls') <> (singleton "_embedded" es') <> kvm
    v -> v
    where
      ls' = object [fromString rel .= object ["href" .= linkURI href] | (rel, href) <- ls]
      es' = object [fromString name .= toJSON e | (name, e) <- es]

instance {-# OVERLAPPING #-} (ToJSON a, Related a, KnownSymbol (CollectionName a)) => ToJSON ([HALResource a]) where
  toJSON xs = object ["_links" .= (mempty :: Object), "_embedded" .= es]
    where
      es = object $
        [  fromString (symbolVal (Proxy @(CollectionName a)))
        .= (Array $ foldl' (\xs' x -> xs' <> pure (toJSON x)) mempty xs)
        ]

instance {-# OVERLAPPABLE #-}
  ( Related a, HasField (IdField a) a id, IsElem (GetOneApi a) api
  , HasLink (GetOneApi a), MkLink (GetOneApi a) Link ~ (id -> Link)
  , BuildRels api (Relations a) a
  , HasResource (HAL t)
  ) => ToResource (HAL t) api a where
  toResource _ api x = HALResource x (defaultLinks api x) mempty
