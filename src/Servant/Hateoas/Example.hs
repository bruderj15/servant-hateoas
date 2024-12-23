{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Example where

import Servant.Hateoas
import Servant
import Data.Aeson
import GHC.Generics

data User = User { usrId :: Int, addressId :: Int, income :: Double }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON)

data Address = Address { addrId :: Int, street :: String, city :: String }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, ToResource res)

instance Resource res => ToResource res User where
  toResource _ usr = addRel ("address", CompleteLink $ mkAddrLink $ addressId usr) $ wrap usr
    where
      mkAddrLink = safeLink (Proxy @AddressGetOne) (Proxy @AddressGetOne)

type Api = UserApi :<|> AddressApi

type UserApi = UserGetOne :<|> UserGetAll :<|> UserGetAllCool
type UserGetOne     = "api" :> "user" :> Capture "id" Int :> Get '[JSON] User
type UserGetAll     = "api" :> "user" :> Get '[JSON] [User]
type UserGetAllCool = "api" :> "user" :> "cool-guys" :> Get '[JSON] [User]

type AddressApi = AddressGetOne
type AddressGetOne = "api" :> "address" :> Capture "id" Int :> Get '[JSON] Address

instance HasHandler UserGetOne where
  getHandler _ _ = \uId -> return $ User uId 1 1000

instance HasHandler UserGetAll where
  getHandler _ _ = return [User 1 1 1000, User 2 2 2000, User 42 3 3000]

instance HasHandler UserGetAllCool where
  getHandler _ _ = return [User 42 3 3000]

instance HasHandler AddressGetOne where
  getHandler _ _ = \aId -> return $ Address aId "Foo St" "BarBaz"

userApiServer :: Server UserApi
userApiServer = getHandler (Proxy @Handler) (Proxy @UserApi)

layerServer :: Server (Resourcify (MkLayers Api) (HAL JSON))
layerServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @(MkLayers Api))

layerApp :: Application
layerApp = serve (Proxy @((Resourcify (MkLayers Api)) (HAL JSON))) layerServer

apiServer :: Server (Resourcify Api (HAL JSON))
apiServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @Api)

apiApp :: Application
apiApp = serve (Proxy @((Resourcify Api) (HAL JSON))) apiServer
