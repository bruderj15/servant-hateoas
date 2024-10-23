{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Example where

import Servant.Hateoas.Resource
import Servant.Hateoas.Some
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
type AddressGetOne = "address" :> Capture "id" Int :> Get '[HALJSON] (HALResource Address)

type UserApi = UserGetOne
type UserGetOne = "user" :> Capture "id" Int :> Get '[HALJSON] (HALResource User)

instance ToResource HALJSON CompleteApi User where
  toResource _ api u@(User uId aId) = HALResource u
    [ ("self", mkSelf uId)
    , ("address", mkAddr (aId))
    ]
    [("address", SomeToJSON $ Address 100 "Foo-Bar-Street" 42)]
    where
      mkSelf = safeLink api (Proxy @UserGetOne)
      mkAddr = safeLink api (Proxy @AddressGetOne)
