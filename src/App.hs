{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module App where

import Control.Lens ((&), (.~), (<&>), (?~), (^.))
import Network.Google
import Control.Monad.IO.Class  (MonadIO (..))
import qualified Network.HTTP.Client as Client
import GHC.Int
import Network.Google.Compute
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Maybe
import System.IO (stdout, hGetContents)
import qualified Network.Google as Google
import qualified Network.Google.Compute as Compute
import System.Process
import GHC.Generics
import Option
import qualified Option as O
import Network.HTTP.Client
import Network.Google.Compute.Metadata
import Data.Text (Text)
import qualified Data.Text as T

data MetaDisk = MetaDisk { deviceName :: Text } deriving (Show, Generic)

instance FromJSON MetaDisk

run :: Option -> IO ()
run config = storategy (O.percent config) (fromIntegral $ O.gb config) >>= \x ->
  case x of
    Just gbR -> do
      projectId <- exampleProjectId
      zoneR <- exampleZone
      diskNameM <- listToMaybe <$> exampleListDisks
      _ <- print projectId
      _ <- print zoneR
      _ <- print diskNameM
      case diskNameM of
        Just disks -> do
          _ <- exampleDiskResizeGCP projectId (deviceName disks) zoneR gbR
          exampleDiskResizeOS
        Nothing -> putStrLn "no Disk"
    Nothing -> return ()

exampleDiskResizeOS :: IO ()
exampleDiskResizeOS = do
  (_, Just parted, _, _) <- createProcess (proc "sudo" ["parted", "/dev/sda", "resizepart", "1", "yes", "100%"]) { std_out = CreatePipe }
  a <- hGetContents parted
  print a
  (_, Just resize2fs, _, _) <- createProcess (proc "sudo" ["resize2fs", "/dev/sda1"]) { std_out = CreatePipe }
  b <- hGetContents resize2fs
  print b

exampleDiskResizeGCP :: Text -> Text -> Text -> GHC.Int.Int64 -> IO Operation
exampleDiskResizeGCP p d z gbR = do
  lgr <- Google.newLogger Google.Debug stdout
  envR <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Compute.computeScope)
  runResourceT . Google.runGoogle envR $ Google.send $ disksResize p d z (disksResizeRequest & drrSizeGb ?~ gbR)

getDisk :: MonadIO m => Manager -> m [MetaDisk]
getDisk m = do
  rs <- getMetadata "instance/disks/?alt=json&recursive=true" [] m
  case eitherDecode' (Client.responseBody rs) of
    Left  _  -> pure []
    Right xs -> pure xs

--[{"deviceName":"instance-1","index":0,"mode":"READ_WRITE","type":"PERSISTENT"}]
exampleListDisks :: IO [MetaDisk]
exampleListDisks = do
  m <- newManager defaultManagerSettings
  isGCE m >>= \x -> if x then getDisk m else fail "no GCE"

exampleGetDisks :: Text -> Text -> Text -> IO (Maybe Text)
exampleGetDisks p d z = do
  lgr <- Google.newLogger Google.Debug stdout
  envR <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Compute.computeReadOnlyScope)
  flip (^.) dName <$> (runResourceT . Google.runGoogle envR $ Google.send $ disksGet p d z)

storategy :: Float -> Int64 -> IO (Maybe Int64)
storategy a b = (\x -> if a < (fromIntegral x * 100) then Just b else Nothing) <$> used

-- return 0-100 percent
used :: IO Int
used = do
  (_, Just df, _, ph1) <- createProcess (proc "df" ["/dev/sda1"]) { std_out = CreatePipe }
  (_, Just tl, _, ph2) <- createProcess (proc "tail" ["-n", "1"]) { std_out = CreatePipe, std_in = UseHandle df }
  (_, Just awk, _, ph3) <- createProcess (proc "awk" ["{print $5}"]) { std_out = CreatePipe, std_in = UseHandle tl }
  c <- hGetContents awk
  _ <- waitForProcess ph1
  _ <- waitForProcess ph2
  _ <- waitForProcess ph3
  return $ (read :: String -> Int) $ reverse . drop 2 . reverse $ c -- drop "%\n"

-- exampleInstanceId :: IO Text
-- exampleInstanceId = do
--   m <- newManager defaultManagerSettings
--   isGCE m >>= \x -> if x then getInstanceId m else fail "no GCE"

exampleProjectId :: IO Text
exampleProjectId = do
  m <- newManager defaultManagerSettings
  isGCE m >>= \x -> if x then getProjectId m else fail "no GCE"

exampleZone :: IO Text
exampleZone = do
  m <- newManager defaultManagerSettings
  T.reverse . T.takeWhile (\x -> x /= '/') . T.reverse <$> (isGCE m >>= \x -> if x then getZone m else fail "no GCE")

