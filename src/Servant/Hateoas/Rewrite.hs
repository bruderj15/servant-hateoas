module Servant.Hateoas.Rewrite where

import Servant
import Data.Kind
import GHC.TypeLits

type MkHatoeas :: Type -> (Type -> Type) -> Type -> Type
type family MkHatoeas api res ct :: Type where
  MkHatoeas (Verb m s _ a) res ct           = Verb m s '[ct] (res a)
  MkHatoeas (a :<|> b) res ct               = MkHatoeas a res ct :<|> MkHatoeas b res ct
  MkHatoeas ((path :: Symbol) :> b) res ct  = _
  MkHatoeas (a :> b) res ct                 = a :> MkHatoeas b res ct
