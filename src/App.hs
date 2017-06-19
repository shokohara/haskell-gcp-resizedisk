{-# LANGUAGE OverloadedStrings #-}

module App where

import Data.Char
import Data.String.Here
import Control.Lens ((&), (.~), (<&>), (?~), (^.), (^..), (^?))
import Data.Text (Text)
import Network.Google
import Network.Google.Compute
import Control.Monad.Trans.Resource (liftResourceT, runResourceT)
import Network.Google.Compute.Types
import Network.Google.PubSub
import Network.Google.PubSub.Types
import Data.UUID
import System.Random
import Network.Google.Resource.Compute.Instances.Insert
import Network.Google.Resource.PubSub.Projects.Subscriptions.Pull
import System.IO (stdout, hGetContents)
import qualified Data.Text as T
import qualified Network.Google as Google
import qualified Network.Google.Compute as Compute
import qualified Network.Google.PubSub as PubSub
import qualified Network.Google.Storage as Storage
import Data.Aeson.Encode.Pretty hiding (Config)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import GHC.Generics
import System.Process
import qualified Data.Yaml as Y
import Data.Yaml.Config
import Data.Yaml (FromJSON(..), (.:))
import Config
import qualified Config as C
import System.Exit
import Data.Conduit (($$+-))
import qualified Data.Conduit.Binary as Conduit
import Network.Google.Resource.Storage.Objects.List
import Network.HTTP.Client
import Network.Google.Compute.Metadata
import Network.Google.Resource.Compute.Disks.Get
import Network.Google.Resource.Compute.Disks.Resize
import Network.Google.Compute.Types

run :: IO ()
run = do
  mConfig <- run2
  case mConfig of
    Just config -> do
      exampleDiskResize (C.project config) "shokoharatest" "asia-northeast1-a"
--      exampleGetDisks (C.project config) "shokoharatest" "asia-northeast1-a"
--      exampleInstanceId
--      testUuid
--      testStorage config
--      run3 config
--      run1 config
--      run4 config
    Nothing -> die "error"

exampleDiskResize p d z = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Compute.computeScope)
  r <- runResourceT . Google.runGoogle env $ Google.send $ disksResize p d z (disksResizeRequest & drrSizeGb ?~ 10)
  print r

exampleGetDisks p d z = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Compute.computeReadOnlyScope)
  r <- runResourceT . Google.runGoogle env $ Google.send $ disksGet p d z
  print r

exampleInstanceId :: IO ()
exampleInstanceId = do
  m <- newManager defaultManagerSettings
  isGCE m >>= print
  getInstanceId m >>= print

newUUID :: IO UUID
newUUID = randomIO

testUuid :: IO ()
testUuid = sequence_ $ replicate 3 $ newUUID >>= print
--testUuid = sequence_ $ replicate 3 $ do
--  x <- newUUID
--  y <- newUUID
--  seq (x == y) $ return ()

key :: Text
key = T.pack "input"

run4 :: Config -> IO ()
run4 config = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Storage.storageReadWriteScope)
  body <- Google.sourceBody "input"
  let bucket = C.bucket config
  r <- runResourceT . Google.runGoogle env $ do
    _ <- Google.upload (Storage.objectsInsert bucket Storage.object' & Storage.oiName ?~ key) body
    stream <- Google.download (Storage.objectsGet bucket key)
    liftResourceT (stream $$+- Conduit.sinkFile "output")
  return ()

run3 :: Config -> IO ()
run3 config = do
  (_, Just hout, _, _) <- createProcess (proc "mkdir" ["-p", T.unpack $ directory config]) { cwd = Just ".", std_out = CreatePipe }
  b <- hGetContents hout
  putStrLn b
  (_, Just hout, _, _) <- createProcess (proc "ls" []) { cwd = Just $ T.unpack $ C.directory config, std_out = CreatePipe }
  a <- hGetContents hout
  putStrLn a
  (_, Just hout, _, _) <- createProcess (proc "ls" []) { cwd = Just ".", std_out = CreatePipe }
  c <- hGetContents hout
  putStrLn c

run1 :: Config -> IO ()
run1 config = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ PubSub.pubSubScope)
  r <- runResourceT . Google.runGoogle env $ Google.send $ projectsSubscriptionsPull (pullRequest & prMaxMessages ?~ 1) $ C.subscription config
  BS.putStrLn $ encodePretty (catMaybes (flip (^.) pmAttributes <$> catMaybes (flip (^.) rmMessage <$> concat (r ^.. prReceivedMessages))))
  return ()

run2 :: IO (Maybe Config)
run2 = loadYamlSettings ["config.yaml"] [] useEnv :: IO (Maybe Config)

testStorage :: Config -> IO ()
testStorage config = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Storage.storageReadWriteScope)
  let bucket = C.bucket config
  r <- runResourceT . Google.runGoogle env $ do
    Google.send $ objectsList $ C.bucket config
  print $ show r

