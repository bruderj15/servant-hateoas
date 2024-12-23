{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Layer.Merge where

import Servant.Hateoas.Layer.Type
import Data.Type.Bool

type MergeLayers :: [Layer] -> [Layer] -> [Layer]
type family MergeLayers ls acc where
  MergeLayers '[] acc = acc
  MergeLayers (l ': ls) acc = MergeLayers ls (
      If (ContainsLayerApi acc (LayerApiCs l))
      (WithAllChildrenOfLayerApi acc l)
        (l ': acc)
      )

type family ContainsLayerApi ls api where
  ContainsLayerApi '[] _ = 'False
  ContainsLayerApi ('Layer api _ _ ': ls) api = 'True
  ContainsLayerApi (_ ': ls) api = ContainsLayerApi ls api

type family WithAllChildrenOfLayerApi ls l where
  WithAllChildrenOfLayerApi '[] _ = '[]
  WithAllChildrenOfLayerApi (('Layer api cs verb) ': ls) ('Layer api cs' verb) = 'Layer api (cs ++ cs') verb ': ls
  WithAllChildrenOfLayerApi (l ': ls) l' = l ': WithAllChildrenOfLayerApi ls l'
