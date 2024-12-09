{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ContentType.Collection
( Collection
, CollectionResource(..)
, CollectionItem(..)
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
-- Type parameter @t@ is the Mime-Type suffix in @application/vnd.collection+t@.
data Collection (t :: Type)

type instance MkResource (Collection t) = CollectionResource

-- | Resource wrapper for 'Collection'.
data CollectionResource a = CollectionResource
  { href  :: Maybe Link                   -- ^ Link to the collection
  , items :: [CollectionItem a]           -- ^ All items in the collection
  , links :: [(String, Link)]             -- ^ Pairs @(rel, link)@ for relations
  } deriving (Show, Generic, Functor)

instance Semigroup (CollectionResource a) where
  (CollectionResource _ is ls) <> (CollectionResource _ is' ls') = CollectionResource Nothing (is <> is') (ls <> ls')

instance Monoid (CollectionResource a) where
  mempty = CollectionResource Nothing [] []

-- | A single item inside a 'CollectionResource'.
data CollectionItem a = CollectionItem
  { item :: a                             -- ^ Wrapped item
  , itemLinks :: [(String, Link)]         -- ^ Links for the wrapped item
  } deriving (Show, Generic, Functor)

instance Resource CollectionResource where
  wrap x = CollectionResource Nothing [wrap x] []
  addLink l (CollectionResource h r ls) = CollectionResource h r (l:ls)

instance Resource CollectionItem where
  wrap x = CollectionItem x []
  addLink l (CollectionItem i ls) = CollectionItem i (l:ls)

instance Accept (Collection JSON) where
  contentType _ = "application" M.// "vnd.collection+json"

instance ToJSON a => MimeRender (Collection JSON) (CollectionResource a) where
  mimeRender _ = encode

collectionLinks :: [(String, Link)] -> Value
collectionLinks = Array . Foldable.foldl' (\xs (rel, l) -> pure (object ["name" .= rel, "value" .= linkURI l]) <> xs) mempty

instance ToJSON a => ToJSON (CollectionItem a) where
  toJSON (CollectionItem x ls) = object ["data" .= itemData, "links" .= collectionLinks ls]
    where
      itemData = Array
        $ Foldable.foldl' (\xs (k, v) -> pure (object ["name" .= k, "value" .= v]) <> xs) mempty
        $ case toJSON x of Object o -> toList o ; _ -> mempty

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (CollectionResource a) where
  toJSON (CollectionResource mHref is ls) = object ["collection" .= collection]
    where
      collection = object $ ["version" .= ("1.0" :: String), "links" .= collectionLinks ls, "items" .= is'] <> maybe [] (pure . ("href" .=) . linkURI) mHref
      is' = Array $ Foldable.foldl' (\xs i -> pure (toJSON i) <> xs) mempty is

instance CollectingResource CollectionResource where
  collect i (CollectionResource mHref is ls) = CollectionResource mHref (CollectionItem i mempty : is) ls
