{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Rewrite where

import Servant
import Servant.Hateoas.Resource
import Data.Kind

type family Normalize api where
  Normalize ((prefix :> a) :<|> (prefix :> b)) = Normalize (prefix :> (Normalize a :<|> Normalize b))
  Normalize (a :<|> b)                         = Normalize a :<|> Normalize b
  Normalize ((prefix :> a) :>   (prefix :> b)) = Normalize (prefix :> (Normalize a :>   Normalize b))
  Normalize (a :> b)                           = a :> Normalize b
  Normalize a                                  = a

-- For now just wrap response type with the content-types resource.
-- May consider special cases where @a@ in @(Verb _ _ _ a)@ is already a resource.
-- Force overwrite?
-- Or simply with the content types resource again?
type HateoasifyResponseType :: Type -> Type -> Type
type family HateoasifyResponseType api ct where
  HateoasifyResponseType (a :<|> b) ct           = HateoasifyResponseType a ct :<|> HateoasifyResponseType b ct
  HateoasifyResponseType (a :> b) ct             = a :> HateoasifyResponseType b ct
  HateoasifyResponseType (Verb m s _ a) ct       = Verb m s '[ct] (MkResource ct a)
  HateoasifyResponseType a _                     = a
