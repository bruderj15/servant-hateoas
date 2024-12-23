module Servant.Hateoas.Internal.Sym where

import Servant
import GHC.TypeLits

-- TODO: HasLink

data Sym (sym :: Symbol)

instance (HasServer api context, KnownSymbol sym) => HasServer (Sym sym :> api) context where
  type ServerT (Sym sym :> api) m = ServerT (sym :> api) m
  route _ = route (Proxy @(sym :> api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(sym :> api))

instance (HasLink api, KnownSymbol sym) => HasLink (Sym sym :> api) where
  type MkLink (Sym sym :> api) link = MkLink (sym :> api) link
  toLink f _ = toLink f (Proxy @(sym :> api))

type family Symify api where
  Symify (a :<|> b) = Symify a :<|> Symify b
  Symify ((sym :: Symbol) :> b) = Sym sym :> Symify b
  Symify (a :> b) = a :> Symify b
  Symify a = a
