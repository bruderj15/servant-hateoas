{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ExampleSpec (spec) where

import           Data.Type.Equality      ((:~:) (Refl))
import           Servant                 (Capture, Capture', Get, JSON,
                                          QueryParam, (:<|>), (:>))
import           Servant.Hateoas         (Title)
import           Servant.Hateoas.Example (Address, Api, User, apiApp)
import           Servant.Hateoas.Layer   (Normalize)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher  (bodyEquals)

spec :: Spec
spec = with (pure apiApp) $
  describe "Example HAL API" $
    describe "UserGetOne (GET /api/user/:id)" $
      it "returns the requested user as a HAL resource" $
        get "/api/user/1" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/0\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/1\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":0,\"income\":0,\"usrId\":1}" }

checkNormalize :: Normalize Api :~:
  ( "api"
    :> ( "user"
      :> (     Get '[JSON] [User] 
          :<|> Title "The user with the given id" :> Capture "id" Int :> Get '[JSON] User
          :<|> "query" :> QueryParam "addrId" Int :> QueryParam "income" Double :> Get '[JSON] User)
    :<|> "address" :> Capture "id" Int :> Get '[JSON] Address)
  )
checkNormalize = Refl
