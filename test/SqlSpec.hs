{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module SqlSpec where

import Api
import App
import Models
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Test.Hspec

main :: IO ()
main = hspec spec

spec = describe "sql" $ do
  it "" $ do
    "" `shouldBe` ""
--    (show hello) `shouldBe` ""
--    (show selectAccount) `shouldBe` ""
--    (show account1) `shouldBe` ""
