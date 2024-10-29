{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Servant.Hateoas.ContentType.HAL
( HAL
, HALResource(..)
)
where

import Servant.Hateoas.Resource
import Servant.Hateoas.Some
import Servant.API.ContentTypes
import qualified Network.HTTP.Media as M
import Servant.Links
import qualified Data.Foldable as Foldable
import Data.Kind
import Data.Proxy
import Data.Aeson
import Data.Aeson.KeyMap (singleton)
import GHC.Exts
import GHC.TypeLits
import GHC.Generics
import GHC.Records

-- | Data-Kind representing Content-Types of Hypertext Application Language (HAL).
--
--   Type parameter @t@ is the mime type suffix in @application/hal+t@.
data HAL (t :: Type)

-- | Resource wrapper for HAL.
data HALResource a = HALResource
  { resource :: a                                       -- ^ Wrapped resource
  , links    :: [(String, Link)]                        -- ^ Pairs @(rel, link)@ for relations
  , embedded :: [(String, SomeToJSON HALResource)]      -- ^ Pairs @(rel, resource)@ for embedded resources
  } deriving (Generic)

instance Resource HALResource where
  addLink l (HALResource r ls es) = HALResource r (l:ls) es

instance Accept (HAL JSON) where
  contentType _ = "application" M.// "hal+json"

instance ToJSON a => MimeRender (HAL JSON) (HALResource a) where
  mimeRender _ = encode

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (HALResource a) where
  toJSON (HALResource res ls es) = case toJSON res of
    Object kvm -> Object $ (singleton "_links" ls') <> (singleton "_embedded" es') <> kvm
    v -> v
    where
      ls' = object [fromString rel .= object ["href" .= linkURI href] | (rel, href) <- ls]
      es' = object [fromString name .= toJSON e | (name, e) <- es]

instance {-# OVERLAPPING #-} (ToJSON a, Related a, KnownSymbol (CollectionName a)) => ToJSON [HALResource a] where
  toJSON xs = object ["_links" .= (mempty :: Object), "_embedded" .= es]
    where
      es = object $
        [  fromString (symbolVal (Proxy @(CollectionName a)))
        .= (Array $ Foldable.foldl' (\xs' x -> xs' <> pure (toJSON x)) mempty xs)
        ]

instance EmbeddingResource HALResource where
  embed e (HALResource r ls es) = HALResource r ls $ fmap SomeToJSON e : es

instance {-# OVERLAPPABLE #-}
  ( Related a, HasField (IdSelName a) a id, IsElem (GetOneApi a) api
  , HasLink (GetOneApi a), MkLink (GetOneApi a) Link ~ (id -> Link)
  , BuildRels api (Relations a) a
  , Resource HALResource
  ) => ToResource api HALResource a where
  toResource x = HALResource x (defaultLinks (Proxy @api) x) mempty
