module Servant.Hateoas.Internal.Sym
(
  -- * Type
  Sym,

  -- * Type family
  Symify
)
where

import Servant
import Servant.Hateoas.HasHandler
import Servant.Hateoas.RelationLink
import GHC.TypeLits

-- | A wrapper for path segments of kind 'Symbol'.
data Sym (sym :: Symbol)

instance (HasServer api context, KnownSymbol sym) => HasServer (Sym sym :> api) context where
  type ServerT (Sym sym :> api) m = ServerT (sym :> api) m
  route _ = route (Proxy @(sym :> api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(sym :> api))

instance (HasLink api, KnownSymbol sym) => HasLink (Sym sym :> api) where
  type MkLink (Sym sym :> api) link = MkLink (sym :> api) link
  toLink f _ = toLink f (Proxy @(sym :> api))

instance (HasTemplatedLink api, KnownSymbol sym) => HasTemplatedLink (Sym sym :> api) where
  toTemplatedLink _ = toTemplatedLink (Proxy @(sym :> api))

instance (KnownSymbol sym, HasRelationLink (sym :> api), HasLink api) => HasRelationLink (Sym sym :> api) where
  toRelationLink _ = toRelationLink (Proxy @(sym :> api))

instance (HasHandler api, KnownSymbol sym) => HasHandler (Sym sym :> api) where
  getHandler m _ = getHandler m (Proxy @(sym :> api))

-- | A type family that wraps all path segments of kind 'Symbol' in an API with 'Sym'.
type family Symify api where
  Symify (a :<|> b) = Symify a :<|> Symify b
  Symify ((sym :: Symbol) :> b) = Sym sym :> Symify b
  Symify (a :> b) = a :> Symify b
  Symify a = a
