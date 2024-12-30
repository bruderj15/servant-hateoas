{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Combinator.Title where

import Servant
import Servant.Hateoas.HasHandler
import Servant.Hateoas.RelationLink
import Servant.Hateoas.Internal.Polyvariadic
import Data.String (fromString)
import Control.Applicative ((<|>))
import GHC.TypeLits

data Title (sym :: Symbol)

instance HasLink b => HasLink (Title sym :> b) where
  type MkLink (Title sym :> b) link = MkLink b link
  toLink f _ = toLink f (Proxy @b)

instance HasServer api ctx => HasServer (Title desc :> api) ctx where
  type ServerT (Title desc :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt s

instance HasHandler api => HasHandler (Title desc :> api) where
  getHandler m _ = getHandler m (Proxy @api)

instance (KnownSymbol title, HasTemplatedLink api) => HasTemplatedLink (Title title :> api) where
  toTemplatedLink _  = (\l -> l { _title = _title l <|> Just title }) $ toTemplatedLink (Proxy @api)
    where
      title = fromString $ symbolVal (Proxy @title)

instance (KnownSymbol title, RightLink api) => HasRelationLink (Title title :> api) where
  toRelationLink _  = (\l -> l { _title = _title l <|> Just title }) ... toRelationLink (Proxy @api)
    where
      title = fromString $ symbolVal (Proxy @title)
