module Servant.Hateoas.Rewrite where

import Servant
import Data.Kind

type NormalizeApi :: Type -> Type
type family NormalizeApi api :: Type where
  NormalizeApi ((prefix :> a) :<|> (prefix :> b)) = prefix :> (NormalizeApi a :<|> NormalizeApi b)
  NormalizeApi (a :<|> b)                         = NormalizeApi a :<|> NormalizeApi b
  NormalizeApi ((prefix :> a) :>   (prefix :> b)) = prefix :> (NormalizeApi a :>   NormalizeApi b)
  NormalizeApi (a :> b)                           = a :> NormalizeApi b
