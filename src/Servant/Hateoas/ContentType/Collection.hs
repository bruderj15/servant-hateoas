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
  { href  :: Maybe URI                   -- ^ Link to the collection
  , items :: [CollectionItem a]          -- ^ All items in the collection
  , rels  :: [(String, ResourceLink)]    -- ^ Pairs @(rel, link)@ for relations
  } deriving (Show, Generic, Functor)

instance Semigroup (CollectionResource a) where
  (CollectionResource _ is ls) <> (CollectionResource _ is' ls') = CollectionResource Nothing (is <> is') (ls <> ls')

instance Monoid (CollectionResource a) where
  mempty = CollectionResource Nothing [] []

-- | A single item inside a 'CollectionResource'.
data CollectionItem a = CollectionItem
  { item :: a                             -- ^ Wrapped item
  , itemLinks :: [(String, ResourceLink)]         -- ^ Links for the wrapped item
  } deriving (Show, Generic, Functor)

instance Resource CollectionResource where
  wrap x = CollectionResource Nothing [wrap x] []
  addRel l (CollectionResource h r ls) = CollectionResource h r (l:ls)

instance Resource CollectionItem where
  wrap x = CollectionItem x []
  addRel l (CollectionItem i ls) = CollectionItem i (l:ls)

instance Accept (Collection JSON) where
  contentType _ = "application" M.// "vnd.collection+json"

instance ToJSON (CollectionResource a) => MimeRender (Collection JSON) (CollectionResource a) where
  mimeRender _ = encode

collectionLinks :: [(String, ResourceLink)] -> Value
collectionLinks = Array . Foldable.foldl' (\xs (rel, l) -> pure (object ["name" .= rel, "value" .= l]) <> xs) mempty

-- TODO: I dont like this at all
-- CollectionResource a represents [a]
-- So CollectionResource [a] represents [[a]]
-- This is bad, when rewriting [Foo] in for the ResourceServer we get CollectionResource [Foo] but we want CollectionResource Foo
-- Best would be to handle this when rewriting but does this result in bloat?
-- It actually may not because we can match specialized cases in type families Resourcify and ResourcifyServer
-- How does this affect other parts of the procedure?
-- Ideally we do not have to adjust any HasResourceServer/BuildLayerLink instances at all

instance ToJSON a => ToJSON (CollectionItem a) where
  toJSON (CollectionItem x ls) = object ["data" .= itemData, "links" .= collectionLinks ls]
    where
      itemData = Array
        $ Foldable.foldl' (\xs (k, v) -> pure (object ["name" .= k, "value" .= v]) <> xs) mempty
        $ case toJSON x of Object o -> toList o ; _ -> mempty

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (CollectionResource a) where
  toJSON (CollectionResource mHref is ls) = object ["collection" .= collection]
    where
      collection = object $ ["version" .= ("1.0" :: String), "links" .= collectionLinks ls, "items" .= is'] <> maybe [] (pure . ("href" .=)) mHref
      is' = Array $ Foldable.foldl' (\xs i -> pure (toJSON i) <> xs) mempty is

instance CollectingResource CollectionResource where
  collect i (CollectionResource mHref is ls) = CollectionResource mHref (CollectionItem i mempty : is) ls
