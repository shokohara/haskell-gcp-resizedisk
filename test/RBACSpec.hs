{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module RBACSpec where

import Api
import App (makeTestApp)
import Data.Aeson hiding (json)
import Models
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

ar i r o = AuthorizeRequest i [PermissionRequest r o]

spec = with (makeTestApp ":memory:") $ do
  describe "GET /api/authority" $ do
    it "responds with 405" $ do
      get "/api/authority" `shouldRespondWith` 405
  describe "POST /api/authority" $ do
    it "responds with 200" $ do
      request methodPost "/api/authority" [(hContentType, "application/json")] (encode $ ar 1 "*" "*") `shouldRespondWith` 200
    it "responds with 400" $ do
      request methodPost "/api/authority" [(hContentType, "application/json")] "" `shouldRespondWith` 400
      request methodPost "/api/authority" [(hContentType, "application/json")] [json|{}|] `shouldRespondWith` 400
    it "responds with 403" $ do
      request methodPost "/api/authority" [(hContentType, "application/json")] (encode $ ar 1 "" "") `shouldRespondWith` 403
    it "responds with 415" $ do
      post "/api/authority" "" `shouldRespondWith` 415
      post "/api/authority" "{}" `shouldRespondWith` 415
      post "/api/authority" [json|{}|] `shouldRespondWith` 415
    it "responds with 201" $ do
      post "/api/users" [json|{"name": "firstUserName"}|] `shouldRespondWith` 201

