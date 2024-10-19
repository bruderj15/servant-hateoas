module Servant.Hateoas where

import Servant.Hateoas.Resource
import Servant.Hateoas.ContentType
import Servant
import Data.Aeson
import GHC.Generics

-------------- Example --------------
data User = User
  { userId :: Int
  , addressId :: Int
  } deriving (Generic, Show, Eq, Ord)

instance ToJSON User

instance ToResource User api where
  toResource _ u@(User _ addrId) = Resource u userLinks
    where
      userLinks = pure ("address", "http://host:port/addresses/" <> show addrId)

type UserApi = "user" :> Capture "id" Int :> Get '[HALJSON] (Resource User)

userServer :: Server UserApi
userServer = \i -> return $ toResource (Proxy @UserApi) $ User i 42
