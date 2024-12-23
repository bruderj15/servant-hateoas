{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Layer.Merge where

import Servant.Hateoas.Layer.Type
import Data.Type.Bool

-- | Given two lists of 'Layer's, merges them into one list by comnbining all 'RelativeChildren' of 'Layer's with the exact same API.
type MergeLayers :: [Layer] -> [Layer] -> [Layer]
type family MergeLayers ls acc where
  MergeLayers '[] acc = acc
  MergeLayers (l ': ls) acc = MergeLayers ls (
      If (ContainsLayerApi acc (LayerApiCs l))
         (WithAllChildrenOfLayerApi acc l)
         (l ': acc)
      )

-- | Returns 'True' iff the given list of 'Layer's contains a 'Layer' with the given API.
type family ContainsLayerApi ls api where
  ContainsLayerApi '[] _ = 'False
  ContainsLayerApi ('Layer api _ _ ': ls) api = 'True
  ContainsLayerApi (_ ': ls) api = ContainsLayerApi ls api

-- | If a list of 'Layer's contains a 'Layer' with the API of the given 'Layer',
-- merge the first matching 'Layer' with the given 'Layer' by combining their 'RelativeChildren'.
type family WithAllChildrenOfLayerApi ls l where
  WithAllChildrenOfLayerApi '[] _ = '[]
  WithAllChildrenOfLayerApi (('Layer api cs verb) ': ls) ('Layer api cs' verb) = 'Layer api (cs ++ cs') verb ': ls
  WithAllChildrenOfLayerApi (l ': ls) l' = l ': WithAllChildrenOfLayerApi ls l'
