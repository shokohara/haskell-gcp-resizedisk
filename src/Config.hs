{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Char
import Data.String.Here
import Control.Lens ((&), (.~), (<&>), (?~), (^.), (^..), (^?))
import Data.Text hiding (minimum, concat)
import GHC.Generics (Generic)
import Network.Google
import Network.Google.Compute
import Network.Google.Compute.Types
import Network.Google.PubSub
import Network.Google.PubSub.Types
import Network.Google.Resource.Compute.Instances.Insert
import Network.Google.Resource.PubSub.Projects.Subscriptions.Pull
import System.IO (stdout, hGetContents)
import qualified Data.Text as T
import qualified Network.Google as Google
import qualified Network.Google.Compute as Compute
import qualified Network.Google.PubSub as PubSub
import Data.Aeson.Encode.Pretty hiding (Config)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import GHC.Generics
import System.Process
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

data Config =  Config {
          project :: Text
            , bucket :: Text
            , directory :: Text
            , subscription  :: Text
                      } deriving (Eq, Show, Generic)

instance FromJSON Config

