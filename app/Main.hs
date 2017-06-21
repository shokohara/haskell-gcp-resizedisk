{-# LANGUAGE OverloadedStrings #-}
module Main where

import App
import Control.Monad
import Options.Applicative
import Data.Semigroup ((<>))
import Option
import GHC.Int
import Control.Concurrent

percentOpt :: Parser Float
percentOpt = option auto (long "percent" <> help "Float")

gbOpt :: Parser Int64
gbOpt = option auto (long "gb" <> help "Int64")

sample :: Parser Option
sample = Option <$> gbOpt <*> percentOpt

opts :: ParserInfo Option
opts = info (sample <**> helper) ( fullDesc
  <> progDesc "Print a greeting for TARGET"
    <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
  options <- execParser opts
  forever $ do
    _ <- App.run options
    threadDelay (10 * 1000000)

