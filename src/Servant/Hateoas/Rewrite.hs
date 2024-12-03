module Servant.Hateoas.Rewrite where

import Servant
import Data.Kind

type MkHateoas :: Type -> (Type -> Type) -> Type -> Type
type family MkHateoas api res ct :: Type where
  MkHateoas (Verb m s _ a) res ct                     = Verb m s '[ct] (res a)
  MkHateoas ((prefix :> a) :<|> (prefix :> b)) res ct = prefix :> (MkHateoas a res ct :<|> MkHateoas b res ct)
  MkHateoas (a :<|> b) res ct                         = MkHateoas a res ct :<|> MkHateoas b res ct
  MkHateoas ((prefix :> a) :>   (prefix :> b)) res ct = prefix :> (MkHateoas a res ct :>   MkHateoas b res ct)
  MkHateoas (a :> b) res ct                           = a :> MkHateoas b res ct
