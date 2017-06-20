{-# LANGUAGE OverloadedStrings #-}
module Main where

import App
import           Data.Text                (Text)
import Control.Monad
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import Options.Applicative
import Data.Semigroup ((<>))
import Option
import GHC.Int
import qualified Option as Option

percentOpt :: Parser Float
percentOpt = option auto (long "percent" <> help "Float")

gbOpt :: Parser Int64
gbOpt = option auto (long "gb" <> help "Int64")

sample :: Parser Option
sample = Option <$> gbOpt <*> percentOpt

opts :: ParserInfo Option
opts = info (sample <**> helper)   ( fullDesc
  <> progDesc "Print a greeting for TARGET"
    <> header "hello - a test for optparse-applicative" )

main :: IO ()
--main = forever App.run
--main = App.run
main = do
  options <- execParser opts
  App.loop options

