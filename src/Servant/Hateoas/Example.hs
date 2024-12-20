{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Example where

import Servant.Hateoas
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
  getHandler _ _ = \uId -> return $ User uId 1 1000

instance HasHandler UserGetAll where
  getHandler _ _ = return [User 1 1 1000, User 2 1 2000]

userApiServer :: Server UserApi
userApiServer = getHandler (Proxy @Handler) (Proxy @UserApi)

hateoasUserApiServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @(UserApi))

hateoasUserApiLayerServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @(MkLayers UserApi))

layerApp :: Application
layerApp = serve (Proxy @((Resourcify (MkLayers UserApi)) (HAL JSON))) hateoasUserApiLayerServer

userApiApp :: Application
userApiApp = serve (Proxy @((Resourcify UserApi) (HAL JSON))) hateoasUserApiServer
