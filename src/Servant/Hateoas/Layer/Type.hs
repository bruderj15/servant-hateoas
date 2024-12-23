{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Layer.Type where

import Servant
import Servant.Server.Internal.Router
import Servant.Hateoas.Internal.Sym
import Servant.Hateoas.Resource
import Data.Aeson
import Data.Kind

type (++) xs ys = AppendList xs ys

data Layer = Layer
  { api              :: [Type]
  , relativeChildren :: [Type]
  , verb             :: Type
  }

type family LayerApiCs (a :: Layer) where
  LayerApiCs ('Layer api _ _) = api

type family RelativeChildren (a :: Layer) where
  RelativeChildren ('Layer _ children _) = children

type family LayerVerb (a :: Layer) where
  LayerVerb ('Layer _ _ verb) = verb

type family LayerApi (a :: Layer) where
  LayerApi ('Layer api _ verb) = MkPrefix api verb

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

newtype Intermediate = Intermediate ()
  deriving newtype (Show, Eq, Ord, ToJSON)
type GetIntermediate = Get '[] Intermediate

instance Resource res => ToResource res Intermediate where
  toResource _ _ = wrap $ Intermediate ()
