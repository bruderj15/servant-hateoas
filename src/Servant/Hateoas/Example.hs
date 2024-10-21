module Servant.Hateoas.Example where

import Servant.Hateoas.Resource
import Servant.Hateoas.ContentType
import Servant
import Data.Aeson
import GHC.Generics

-------------- Example for dev --------------
data User = User { address :: Address } deriving (Generic, Show, Eq, Ord)
instance ToJSON User

data Address = Address { addressId :: Int, street :: String, number :: Int} deriving (Generic, Show, Eq, Ord)
instance ToJSON Address
instance ToResource api Address where

instance HasResource Address where
  type GetOneApi Address = AddressApi
  type Id Address = Int
  getId = addressId

instance ToResource api User where

type CompleteApi = AddressApi :<|> UserApi
type AddressApi = "address" :> Capture "id" Int :> Get '[HALJSON] (Resource Address)
type UserApi = "user" :> Get '[HALJSON] (Resource User)
