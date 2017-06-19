module MinTf where

import Data.Text hiding (minimum)

data MinTf = MinTf { carNo :: [Int], value :: Int, destStationName :: Text, nextStationId :: Int, stationId :: Int } deriving Show
