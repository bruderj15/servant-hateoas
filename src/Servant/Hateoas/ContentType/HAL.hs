{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ContentType.HAL
(
  -- * Content-Type
  HAL

  -- * Resource-Type
, HALResource(..)
)
where

import Servant
import Servant.Hateoas.Resource
import Servant.Hateoas.RelationLink
import qualified Network.HTTP.Media as M
import qualified Data.Foldable as Foldable
import Data.Some.Constraint
import Data.Kind
import Data.Aeson
import Data.Aeson.KeyMap (singleton)
import qualified Data.Text as Text
import GHC.Exts
import GHC.Generics

-- | Type representing Content-Types of Hypertext Application Language (HAL).
--
--   Type parameter @t@ is the mime type suffix in @application/hal+t@.
data HAL (t :: Type)

type instance MkResource (HAL t) = HALResource

-- | HAL-resource representation.
data HALResource a = HALResource
  { resource :: a                                       -- ^ Wrapped resource
  , rels     :: [(String, RelationLink)]                -- ^ Pairs @(rel, link)@ for hypermedia relations
  , embedded :: [(String, SomeF HALResource ToJSON)]    -- ^ Pairs @(rel, resource)@ for embedded resources
  } deriving (Generic, Functor)

instance Resource HALResource where
  wrap x = HALResource x [] []
  addRel l (HALResource r ls es) = HALResource r (l:ls) es

instance Accept (HAL JSON) where
  contentType _ = "application" M.// "hal+json"

instance ToJSON (HALResource a) => MimeRender (HAL JSON) (HALResource a) where
  mimeRender _ = encode

renderHalLink :: RelationLink -> Value
renderHalLink l = object $
  [ "href" .= getHref l
  , "type" .= Text.intercalate "|" (fromString . show <$> _contentTypes l)
  ] <> if _templated l then ["templated" .= True] else []
    <> maybe mempty (\t -> ["title" .= t]) (_title l)

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (HALResource a) where
  toJSON (HALResource res ls es) = Object $ (singleton "_links" ls') <> (singleton "_embedded" es') <> (case toJSON res of Object kvm -> kvm ; _ -> mempty)
    where
      ls' = object [fromString rel .= renderHalLink l | (rel, l) <- ls]
      es' = object [fromString name .= toJSON e | (name, (Some1 e)) <- es]

instance {-# OVERLAPPING #-} ToJSON a => ToJSON (HALResource [a]) where
  toJSON (HALResource xs ls es) = object ["_links" .= ls', "_embedded" .= object (exs <> es')]
    where
      ls' = object [fromString rel .= renderHalLink l | (rel, l) <- ls]
      es' = fmap (\(eName, (Some1 e)) -> fromString eName .= toJSON e) es
      exs = [ "items"
              .= (Array $ Foldable.foldl' (\xs' x -> xs' <> pure (toJSON x)) mempty xs)
            ]

instance EmbeddingResource HALResource where
  embed e (HALResource r ls es) = HALResource r ls $ fmap Some1 e : es
