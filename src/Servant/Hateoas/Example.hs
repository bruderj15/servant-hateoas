{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Example where

import Servant.Hateoas
import Servant.Hateoas.Layer
import Servant
import Data.Aeson
import GHC.Generics

data User = User { usrId :: Int, addressId :: Int, income :: Double }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, ToResource res)

type UserApi = UserGetOne :<|> UserGetAll
type UserGetOne = "user" :> Capture "id" Int :> Get '[JSON] User
type UserGetAll = "user" :> Get '[JSON] [User]

instance HasHandler UserGetOne where
  getHandler _ _ = \uId -> return (User uId 1 1000)

instance HasHandler UserGetAll where
  getHandler _ _ = return [User 1 1 1000, User 2 1 2000]

userApiServer :: Server UserApi
userApiServer = getHandler (Proxy @Handler) (Proxy @UserApi)

-- Custom instance for @ToResource@
-- instance ToResource HALResource User where
--   toResource _ u = HALResource u [("self", mkLink $ usrId u)] []
--     where
--       mkLink = safeLink (Proxy @UserApi) (Proxy @UserGetOne)

hateoasUserApiServer :: Server (Resourcify UserApi (HAL JSON))
hateoasUserApiServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @UserApi) (Proxy @(Server UserApi))

hateoasUserApiLayerServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @(Layers UserGetAll Bottom)) Proxy

-- testApp :: Application
-- testApp = serve (Proxy @(LayerApi (Layers UserApi Bottom (HAL JSON)))) hateoasUserApiLayerServer
