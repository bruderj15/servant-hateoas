{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Servant.Hateoas.ExampleSpec (spec) where

import           Data.Type.Equality      ((:~:) (Refl))
import           Servant                 (Capture, Get, JSON,
                                          QueryParam, (:<|>), (:>))
import           Servant.Hateoas         (Title)
import           Servant.Hateoas.Example (Address, Api, User, apiApp, layerApp)
import           Servant.Hateoas.Layer   (Normalize)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher  (bodyEquals)

spec :: Spec
spec = do
  with (pure apiApp) $
    describe "Example HAL API" $
      describe "UserGetOne (GET /api/user/:id)" $
        it "returns the requested user as a HAL resource" $
          get "/api/user/1" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/0\",\"type\":\"application/hal+json\"},\"friends\":{\"href\":\"/api/user/1/friends\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/1\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":0,\"friends\":[],\"income\":0,\"usrId\":1}" }
  with (pure layerApp) $
    describe "Test Layer API" $ do
      it "returns the api layer" $
        get "/api" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api\",\"type\":\"application/hal+json\"},\"user\":{\"href\":\"/api/user\",\"type\":\"application/hal+json\"}}}" }
      it "returns the user layer" $
        get "/api/user" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"id\":{\"href\":\"/api/user/{id}\",\"templated\":true,\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}}}" }
      it "returns the query operation" $
        get "/api/user/query" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"addrId\":{\"href\":\"/api/user/query{?addrId}\",\"templated\":true,\"type\":\"application/hal+json\"},\"income\":{\"href\":\"/api/user/query{?income}\",\"templated\":true,\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/query\",\"type\":\"application/hal+json\"}}}" }

-- This is intentionally unused, but it is a compile-time test that the Normalize type family works as expected.
checkNormalize :: Normalize Api :~:
  ( "api"
    :> ( "user"
      :> ( "query" :> QueryParam "addrId" Int :> QueryParam "income" Double :> Get '[JSON] User
        :<|> (Get '[JSON] [User]
        :<|> Title "The user with the given id" :> Capture "id" Int :> Get '[JSON] User
        :<|> Capture "id" Int :> "friends" :> Get '[JSON] [User]))
    :<|> "address" :> Capture "id" Int :> Get '[JSON] Address)
  )
checkNormalize = Refl
