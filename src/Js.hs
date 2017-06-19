{-# LANGUAGE DeriveGeneric #-}

module Js where

import GHC.Generics (Generic)
import Data.Text hiding (minimum)
import Tf (Tf)

data Js = Js { accessDayCd :: Int, at :: Int, accessTime :: Text, destStationName :: Text, direction :: Int, trainFeedbackList :: [Tf] } deriving (Show, Generic)
