{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Example where

import Servant.Hateoas
import Servant
import Data.Aeson
import GHC.Generics

-------------- Example for dev --------------
data User = User { usrId :: Int, addressId :: Int, income :: Double }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass ToJSON

data Address = Address { addrId :: Int, street :: String, number :: Int}
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass ToJSON

type CompleteApi = AddressApi :<|> UserApi

type AddressApi = AddressGetOne
type AddressGetOne = "address" :> Capture "id" Int :> Get '[HAL JSON] (HALResource Address)

type UserApi = UserGetOne :<|> UserGetAll
type UserGetOne = "user" :> Capture "id" Int :> Get '[HAL JSON] (HALResource User)
type UserGetAll = "user" :> Get '[HAL JSON] (HALResource [User])

instance Related User where
  type IdField User = "usrId"
  type GetOneApi User = UserGetOne
  type CollectionName User = "users"
  type Relations User = '[ 'HRel "address" "addressId" AddressGetOne ]
