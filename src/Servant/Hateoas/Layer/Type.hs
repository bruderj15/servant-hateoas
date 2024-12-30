{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Servant.Hateoas.Layer.Type
(
  -- * Type
  Layer(..),

  -- ** Getter
  LayerApiCs, RelativeChildren, LayerVerb,

  -- * API-construction
  LayerApi, MkPrefix, type (++),

  -- * Intermediate
  Intermediate(..), GetIntermediate
)
where

import Servant
import Servant.Server.Internal.Router
import Servant.Hateoas.Internal.Sym
import Servant.Hateoas.Resource
import Data.Aeson
import Data.Kind

-- | Convenience alias for 'AppendList'.
type (++) xs ys = AppendList xs ys

-- | Data-kind for a layer in an API.
--
-- ==== __Example__
--
-- @
-- ''Layer' '['Sym' \"api\", 'Sym' \"user\"] '['Capture' \"id\" 'Int', 'Sym' \"vip\"] 'GetIntermediate'
-- @
--
-- Represents the API
--
-- @
-- \"api\" :> \"user\" :> 'GetIntermediate'
-- @ with children
--
-- @
-- \"api\" :> \"user\" :> 'Capture' \"id\" 'Int' :> 'GetIntermediate'
-- @ and
--
-- @
-- \"api\" :> \"user\" :> \"vip\" :> 'GetIntermediate'
-- @
data Layer = Layer
  { api              :: [Type]      -- ^ The API of this layer represented as list. Folding it with ':>' results in the actual API, see 'MkPrefix'.
  , relativeChildren :: [Type]      -- ^ All immediate children of this layer.
  , verb             :: Type        -- ^ The 'Verb' for this layer.
  }

-- | Type-level getter for the API of a 'Layer'.
type family LayerApiCs (a :: Layer) where
  LayerApiCs ('Layer api _ _) = api

-- | Type-level getter for the children of a 'Layer'.
type family RelativeChildren (a :: Layer) where
  RelativeChildren ('Layer _ children _) = children

-- | Type-level getter for the verb of a 'Layer'.
type family LayerVerb (a :: Layer) where
  LayerVerb ('Layer _ _ verb) = verb

-- | Constructs the actual API of a 'Layer'.
type family LayerApi (a :: Layer) where
  LayerApi ('Layer api _ verb) = MkPrefix api verb

-- | Folds a list of path segments into an API by intercalating '(:>)'.
--
-- ==== __Example__
--
-- @
-- 'MkPrefix' '['Sym' \"api\", 'Sym' \"user\"] 'GetIntermediate'
-- @ resolves to
--
-- @
-- 'Sym' \"api\" :> 'Sym' \"user\" :> 'GetIntermediate'
-- @
type MkPrefix :: [Type] -> Type -> Type
type family MkPrefix prefix api where
  MkPrefix (Sym x      ': xs) api = x :> MkPrefix xs api
  MkPrefix (x          ': xs) api = x :> MkPrefix xs api
  MkPrefix '[]                api = api

instance HasServer (MkPrefix apiCs verb) context => HasServer ('Layer apiCs cs verb) context where
  type ServerT ('Layer apiCs cs verb) m = ServerT (MkPrefix apiCs verb) m
  route _ = route (Proxy @(MkPrefix apiCs verb))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(MkPrefix apiCs verb))

instance HasServer ('[] :: [Layer]) context where
  type ServerT '[] m = ServerT EmptyAPI m
  route _ = route (Proxy @EmptyAPI)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @EmptyAPI)

instance (HasServer l context, HasServer ls context) => HasServer (l ': ls :: [Layer]) context where
  type ServerT (l ': ls) m = ServerT l m :<|> ServerT ls m
  route _ ctx delayed = route (Proxy @l) ctx ((\(sl :<|> _) -> sl) <$> delayed) `choice` route (Proxy @ls) ctx ((\(_ :<|> sls) -> sls) <$> delayed)
  hoistServerWithContext _ ctx f (sl :<|> sls) = hoistServerWithContext (Proxy @l) ctx f sl :<|> hoistServerWithContext (Proxy @ls) ctx f sls

-- | A response type for a 'Layer' that does not contain any data.
newtype Intermediate = Intermediate ()
  deriving newtype (Show, Eq, Ord, ToJSON)
  deriving anyclass (ToResource res)

type GetIntermediate = Get '[] Intermediate
