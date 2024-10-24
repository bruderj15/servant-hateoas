{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Example where

import Servant.Hateoas
import Servant
import Data.Aeson
import GHC.Generics

-------------- Example for dev --------------
data User = User { usrId :: Int, addressId :: Int }
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

instance Resty User where
  type Id User = Int
  type GetOneApi User = UserGetOne
  type CollectionName User = "users"
  getId = usrId

instance ToResource (HAL JSON) CompleteApi User where
  toResource _ api u = HALResource u
    [ selfLink api u
    , ("address", mkAddr (addressId u))
    ]
    []
    where
      mkAddr = safeLink api (Proxy @AddressGetOne)
