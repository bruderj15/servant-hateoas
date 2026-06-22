{-# LANGUAGE OverloadedStrings #-}

module Servant.Hateoas.ExampleSpec (spec) where

import           Servant
import           Servant.Hateoas                        (getResourceServer)
import           Servant.Hateoas.ContentType.Collection (Collection)
import           Servant.Hateoas.Example                (UserGetAll, apiApp)
import           Servant.Hateoas.ResourceServer         (Resourcify)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher                 (bodyEquals)

collectionServer :: Server (Resourcify UserGetAll (Collection JSON))
collectionServer = getResourceServer (Proxy @Handler) (Proxy @(Collection JSON)) (Proxy @UserGetAll)

collectionApp :: Application
collectionApp = serve (Proxy @(Resourcify UserGetAll (Collection JSON))) collectionServer


spec :: Spec
spec = do
  with (pure apiApp) $ do
    describe "HAL API" $
      describe "UserGetOne (GET /api/user/:id)" $
        it "returns the requested user as a HAL resource" $
          get "/api/user/1" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/0\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/1\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":0,\"income\":0,\"usrId\":1}" }
    describe "Collection API" $
      describe "UserGetAll (GET /api/user)" $
        it "returns all users as a HAL collection" $
          get "/api/user" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{\"items\":[{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/1\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/1\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":1,\"income\":1000,\"usrId\":1},{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/2\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/2\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":2,\"income\":2000,\"usrId\":2},{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/3\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/42\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":3,\"income\":3000,\"usrId\":42}]},\"_links\":{\"self\":{\"href\":\"/api/user\",\"type\":\"application/hal+json\"}}}" }
  with (pure collectionApp) $
    describe "Collection API" $
      describe "UserGetAll (GET /api/user)" $
        it "returns all users as a HAL collection" $
          get "/api/user" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"collection\":{\"items\":[{\"data\":[{\"name\":\"usrId\",\"value\":42},{\"name\":\"income\",\"value\":3000},{\"name\":\"addressId\",\"value\":3}],\"links\":[{\"name\":\"address\",\"value\":\"/api/address/3\"},{\"name\":\"self\",\"value\":\"/api/user/42\"}]},{\"data\":[{\"name\":\"usrId\",\"value\":2},{\"name\":\"income\",\"value\":2000},{\"name\":\"addressId\",\"value\":2}],\"links\":[{\"name\":\"address\",\"value\":\"/api/address/2\"},{\"name\":\"self\",\"value\":\"/api/user/2\"}]},{\"data\":[{\"name\":\"usrId\",\"value\":1},{\"name\":\"income\",\"value\":1000},{\"name\":\"addressId\",\"value\":1}],\"links\":[{\"name\":\"address\",\"value\":\"/api/address/1\"},{\"name\":\"self\",\"value\":\"/api/user/1\"}]}],\"links\":[{\"name\":\"self\",\"value\":\"/api/user\"}],\"version\":\"1.0\"}}" }
