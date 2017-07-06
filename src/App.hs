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
import Control.Monad
import Debug.Trace

data MetaDisk = MetaDisk { deviceName :: Text } deriving (Show, Generic)

instance FromJSON MetaDisk

run :: Option -> IO ()
run config = strategy (O.percent config) >>= \x ->
  if x then do
    m <- newManager defaultManagerSettings
    projectId <- exampleProjectId m
    zoneR <- exampleZone m
    diskNameM <- listToMaybe <$> exampleListDisks m
    _ <- traceIO $ show diskNameM
    case diskNameM of
      Just disks -> do
        gbM <- exampleGetDisks projectId (deviceName disks) zoneR
        _ <- traceIO $ show gbM
        _ <- traceIO $ show (O.percent config)
        case gbM of
          Just gbRR -> void $ exampleDiskResizeGCP projectId (deviceName disks) zoneR ((fromIntegral $ O.gb config) + gbRR)
          Nothing -> putStrLn "no Disk"
        exampleDiskResizeOS
      Nothing -> putStrLn "no Disk"
  else return ()

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
    Left  _  -> fail "fail to decode"
    Right xs -> pure xs

exampleListDisks :: Manager -> IO [MetaDisk]
exampleListDisks m = isGCE m >>= \x -> if x then getDisk m else fail "no GCE"

exampleGetDisks :: Text -> Text -> Text -> IO (Maybe Int64)
exampleGetDisks p d z = do
  lgr <- Google.newLogger Google.Debug stdout
  envR <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ Compute.computeReadOnlyScope)
  flip (^.) dSizeGb <$> (runResourceT . Google.runGoogle envR $ Google.send $ disksGet p d z)

--strategy :: Float -> IO Bool
--strategy a = (\x -> a < (fromIntegral x * 100)) <$> used

strategy :: Float -> IO Bool
strategy a = do
  x <- used
  _ <- traceIO $ "used" ++ show x
  return $ (a * 100) < (fromIntegral x)

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

exampleProjectId :: Manager -> IO Text
exampleProjectId m = isGCE m >>= \x -> if x then getProjectId m else fail "no GCE"

exampleZone :: Manager -> IO Text
exampleZone m = (T.reverse . T.takeWhile (\x -> x /= '/') . T.reverse) <$> (isGCE m >>= \x -> if x then getZone m else fail "no GCE")

