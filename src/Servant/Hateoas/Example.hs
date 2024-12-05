{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Example where

import Servant.Hateoas
import Servant.Hateoas.Rewrite
import Servant
import Data.Aeson
import GHC.Generics

data User = User { usrId :: Int, addressId :: Int, income :: Double }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass ToJSON

data Address = Address { addrId :: Int, street :: String, number :: Int}
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass ToJSON

type CompleteApi = AddressApi :<|> UserApi

type AddressApi = AddressGetOne
type AddressGetOne = "address" :> Capture "id" Int :> Get '[JSON] Address

type UserApi = UserGetOne :<|> UserGetAll
type UserGetOne = "user" :> Capture "id" Int :> Get '[JSON] User
type UserGetAll = "user" :> Get '[JSON] [User]

instance HasHandler UserGetOne where
  getHandler _ _ = \uId -> return (User uId 1 1000)

instance HasHandler UserGetAll where
  getHandler _ _ = return [User 1 1 1000, User 2 1 2000]

userApiHandler :: Server UserApi
userApiHandler = getHandler (Proxy @Handler) (Proxy @UserApi)

resourciyfiedUserApi :: Server (Resourcify UserApi (HAL JSON))
resourciyfiedUserApi = getResourceServer (Proxy @Handler) (Proxy @CompleteApi) (Proxy @UserApi) (Proxy @(Server UserApi))

instance Related User where
  type IdSelName User = "usrId"
  type GetOneApi User = Resourcify UserGetOne (HAL JSON)
  type CollectionName User = "users"
  type Relations User =
    '[ 'HRel "address" "addressId" (Resourcify AddressGetOne (HAL JSON))
     ]
