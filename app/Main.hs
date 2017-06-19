{-# LANGUAGE OverloadedStrings #-}
module Main where

import App
import           Data.Text                (Text)
import Control.Monad
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)

main :: IO ()
--main = forever App.run
main = App.run

