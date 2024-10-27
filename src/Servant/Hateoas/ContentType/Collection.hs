{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Servant.Hateoas.ContentType.Collection
( Collection
, CollectionResource(..)
)
where

import Servant.Hateoas.Resource
import Servant.API.ContentTypes
import qualified Network.HTTP.Media as M
import Servant.Links
import qualified Data.Foldable as Foldable
import Data.Kind
import Data.Aeson
import GHC.Exts
import GHC.Generics

-- | Data-Kind representing Content-Types of HATEOAS collections.
--
--   Type parameter @t@ is the mime type suffix in @application/vnd.collection+t@.
data Collection (t :: Type)

-- | Resource wrapper for Collection.
data CollectionResource a = CollectionResource
  { href  :: Maybe String
  , items :: [a]
  , links :: [(String, Link)]
  } deriving (Generic)

instance HasResource (Collection t) where
  type Resource (Collection t) = CollectionResource

instance Accept (Collection JSON) where
  contentType _ = "application" M.// "vnd.collection+json"

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (CollectionResource a) where
  toJSON (CollectionResource mHref is ls) = object ["collection".= collection]
    where
      collection = object $ ["version" .= ("1.0" :: String), "links" .= ls', "items" .= is'] <> maybe [] (pure . ("href" .=)) mHref
      ls' = Array $ Foldable.foldl' (\xs (rel, l) -> pure (object ["name" .= rel, "value" .= linkURI l]) <> xs) mempty ls
      is' = Array $ Foldable.foldl' (\xs i -> pure (object ["data" .= toCollectionData i, "links" .= (mempty :: Array)]) <> xs) mempty is

toCollectionData :: ToJSON a => a -> Value
toCollectionData (toJSON -> Object m) = Array $ Foldable.foldl' (\xs (k, v) -> pure (object ["name" .= k, "value" .= v]) <> xs) mempty $ toList m
toCollectionData (toJSON -> x)        = x
