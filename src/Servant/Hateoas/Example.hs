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

type Api = "api" :> (AddressApi :<|> UserApi)

type UserApi = "user" :> (UserGetOne :<|> UserGetAll :<|> UserGetAllCool)
type UserGetOne = Capture "id" Int :> Get '[JSON] User
type UserGetAll = Get '[JSON] [User]
type UserGetAllCool = "cool-guys" :> Get '[JSON] [User]

type AddressApi = "address" :> AddressGetOne
type AddressGetOne = Capture "id" Int :> Get '[JSON] Address

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

apiServer :: ServerT (Resourcify Api (HAL JSON)) Handler
apiServer = getResourceServer @Api @Handler @(HAL JSON) (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @Api)

layerServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @(MkLayers Api))

layerApp :: Application
layerApp = serve (Proxy @((Resourcify (MkLayers Api)) (HAL JSON))) layerServer

apiApp :: Application
apiApp = serve (Proxy @((Resourcify Api) (HAL JSON))) apiServer
