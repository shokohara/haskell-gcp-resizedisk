{-# LANGUAGE DeriveGeneric #-}

module Tf where

import GHC.Generics (Generic)

data Tf = Tf { carNo1 :: Int, carNo2 :: Int, carNo3 :: Int, carNo4 :: Int, carNo5 :: Int, carNo6 :: Int, carNo7 :: Int, carNo8 :: Int, carNo9 :: Int, carNo10 :: Int, carNo11 :: Int, nextStationId :: Int, stationId :: Int } deriving (Show, Generic)
