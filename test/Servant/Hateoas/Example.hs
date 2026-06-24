{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Example where

import           Data.Aeson      (ToJSON)
import           GHC.Generics
import           Servant
import           Servant.Hateoas

data User = User { usrId :: Int, addressId :: Int, income :: Double, friends :: [Int] }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON)

data Address = Address { addrId :: Int, street :: String, city :: String }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, ToResource res)

instance Resource res => ToResource res User where
  toResource _ ct usr = addRel ("self", mkSelfLink $ usrId usr)
                        . addRel ("address", mkAddrLink $ addressId usr)
                        . addRel ("friends", mkFriendsLink $ usrId usr)
                        $ wrap usr
    where
      mkAddrLink = toRelationLink $ resourcifyProxy (Proxy @AddressGetOne) ct
      mkSelfLink = toRelationLink $ resourcifyProxy (Proxy @UserGetOne) ct
      mkFriendsLink = toRelationLink $ resourcifyProxy (Proxy @UserGetFriends) ct

type Api = UserApi :<|> AddressApi

type UserApi = UserGetOne :<|> UserGetAll :<|> UserGetQuery :<|> UserGetFriends
type UserGetOne     = "api" :> "user" :> Title "The user with the given id" :> Capture "id" Int :> Get '[JSON] User
type UserGetAll     = "api" :> "user" :> Get '[JSON] [User]
type UserGetQuery   = "api" :> "user" :> "query" :> QueryParam "addrId" Int :> QueryParam "income" Double :> Get '[JSON] User
type UserGetFriends = "api" :> "user" :> Capture "id" Int :> "friends" :> Get '[JSON] [User]

type AddressApi = AddressGetOne
type AddressGetOne = "api" :> "address" :> Capture "id" Int :> Get '[JSON] Address

instance Monad m => HasHandler m UserGetOne where
  getHandler _ _ = \uId -> return $ User uId 0 0 []

instance Monad m => HasHandler m UserGetAll where
  getHandler _ _ = return [User 1 1 1000 [2,3], User 2 2 2000 [], User 3 3 3000 []]

instance Monad m => HasHandler m UserGetQuery where
  getHandler _ _ = \mAddrId mIncome -> return $ User 3 (maybe 0 id mAddrId) (maybe 0 id mIncome) []

instance Monad m => HasHandler m AddressGetOne where
  getHandler _ _ = \aId -> return $ Address aId "Foo St" "BarBaz"

instance Monad m => HasHandler m UserGetFriends where
  getHandler _ _ = \uId -> return [User 1 1 1000 [2,3], User 2 2 2000 [], User 3 3 3000 []]

layerServer :: Server (Resourcify (MkLayers Api) (HAL JSON))
layerServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @(MkLayers Api))

layerApp :: Application
layerApp = serve (Proxy @((Resourcify (MkLayers Api)) (HAL JSON))) layerServer

apiServer :: Server (Resourcify Api (HAL JSON))
apiServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @Api)

apiApp :: Application
apiApp = serve (Proxy @((Resourcify Api) (HAL JSON))) apiServer
