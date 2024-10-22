{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Example where

import Servant.Hateoas.Resource
import Servant.Hateoas.ContentType
import Servant
import Data.Aeson
import GHC.Generics

-------------- Example for dev --------------
data User = User { userId :: Int, address :: Address }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, ToResource CompleteApi)

data Address = Address { addressId :: Int, street :: String, number :: Int}
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, ToResource CompleteApi)

instance HasResource Address where
  type GetOneApi Address = AddressApi
  type Id Address = Int
  getId = addressId

instance HasResource User where
  type GetOneApi User = UserApi
  type Id User = Int
  getId = userId

type CompleteApi = AddressApi :<|> UserApi
type AddressApi = "address" :> Capture "id" Int :> Get '[HALJSON] (Resource Address)
type UserApi = "user" :> Get '[HALJSON] (Resource User)
