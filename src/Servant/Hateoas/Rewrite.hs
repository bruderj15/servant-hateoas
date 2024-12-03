{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Rewrite where

import Servant
import Servant.Hateoas.Resource
import Data.Kind
import Data.Type.Equality (type (==))

type family Normalize api where
  Normalize ((prefix :> a) :<|> (prefix :> b)) = Normalize (prefix :> (Normalize a :<|> Normalize b))
  Normalize (a :<|> b)                         = Normalize a :<|> Normalize b
  Normalize ((prefix :> a) :>   (prefix :> b)) = Normalize (prefix :> (Normalize a :>   Normalize b))
  Normalize (a :> b)                           = a :> Normalize b
  Normalize a                                  = a

type HateoasifyResponseType :: Type -> Type -> Type
type family HateoasifyResponseType api ct where
  HateoasifyResponseType (a :<|> b) ct           = HateoasifyResponseType a ct :<|> HateoasifyResponseType b ct
  HateoasifyResponseType (a :> b) ct             = a :> HateoasifyResponseType b ct
  -- TODO: How to handle if response-type already is some resource type res' with res /= res'?
  HateoasifyResponseType (Verb m s _ (res a)) ct = Verb m s '[ct] (If (res == MkResource ct) (res a) (MkResource ct a))
  HateoasifyResponseType (Verb m s _ a) ct       = Verb m s '[ct] (MkResource ct a)
  HateoasifyResponseType a _                     = a
