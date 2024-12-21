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

type Api = (AddressApi :<|> UserApi)

type UserApi = UserGetOne :<|> UserGetAll
type UserGetOne = "user" :> Capture "id" Int :> Get '[JSON] User
type UserGetAll = "user" :> Get '[JSON] [User]

type AddressApi = AddressGetOne
type AddressGetOne = "address" :> Capture "id" Int :> Get '[JSON] Address

instance HasHandler UserGetOne where
  getHandler _ _ = \uId -> return $ User uId 1 1000

instance HasHandler UserGetAll where
  getHandler _ _ = return [User 1 1 1000, User 2 1 2000]

instance HasHandler AddressGetOne where
  getHandler _ _ = \aId -> return $ Address aId "Foo St" "BarBaz"

userApiServer :: Server UserApi
userApiServer = getHandler (Proxy @Handler) (Proxy @UserApi)

apiServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @Api)

layerServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @(MkLayers Api))

layerApp :: Application
layerApp = serve (Proxy @((Resourcify (MkLayers Api)) (HAL JSON))) layerServer

apiApp :: Application
apiApp = serve (Proxy @((Resourcify Api) (HAL JSON))) apiServer
