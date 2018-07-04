{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Tester.Build (
  Arg(Arg),
  ArgType(Value, File),
  BuildId,
  BuildStatus(Ok, Build, Delete),
  getBuildStatus,
  setProblemBuild,
  removeProblemBuild,
  getOptConfArgs,
  getOptMetaArgs,
  concatArgs
  ) where

import           Codex.Tester
import           Control.Concurrent.MVar
import           Data.Hashable
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Database.SQLite.Simple
import           GHC.Generics              (Generic)
import           System.Directory


type BuildId = Int
data BuildStatus = Ok BuildId
           | Build BuildId
           | Delete BuildId
           | Ignore
           deriving Show


data ArgType = Value | File deriving Generic
data Arg = Arg { _type  :: ArgType
               , _opt   :: String
               , _value :: String
               } deriving Generic
instance Hashable ArgType
instance Hashable Arg


-- | calculate the build id of the current problem
--   it hashes the tester name, arguments and if
--   an argument is a file is also uses the file
--   modification date
calculateBuildId :: [Arg] -> Tester BuildId
calculateBuildId args = do
  meta <- testMetadata
  let testerName = lookupFromMeta "tester" meta :: Maybe String
  args' <- mapM setMT args
  return $ hashWithSalt (hash testerName) args'
  where
    setMT (Arg File opt file) = do
      mt <- liftIO $ getModificationTime file
      return (Arg File opt (file++(show mt)))
    setMT arg = return arg


-- | returns the current build status and the problem old build status
getBuildStatus :: [Arg] -> Tester (BuildStatus, BuildStatus)
getBuildStatus args = do
  path <- testPath
  currBuildId <- calculateBuildId args
  dbConn <- testDbConn
  qOldBuildId <- liftIO $ withMVar dbConn (\conn -> (query conn
        "SELECT build_id FROM builds WHERE path=?" (Only path) :: IO [Only BuildId]
        ))
  let oldBuildId = case qOldBuildId of
        (Only h):_ -> Just h
        _          -> Nothing
  if oldBuildId == Just currBuildId then
    return (Ok currBuildId, Ignore)
  else do
    qAlreadyBuild <- liftIO $ withMVar dbConn (\conn -> (query conn
          "SELECT build_id FROM builds WHERE build_id=? LIMIT 1" (Only currBuildId) :: IO [Only BuildId]
          ))
    let currStatus = case qAlreadyBuild of
          _:_ -> Ok currBuildId
          _   -> Build currBuildId
    oldStatus <- case oldBuildId of
          Nothing -> return Ignore
          Just h  -> do
            qOldBuildUsage <- liftIO $ withMVar dbConn (\conn -> (query conn
                  "SELECT count(*) FROM builds WHERE build_id=?" (Only h) :: IO [Only BuildId]
                  ))
            case qOldBuildUsage of
                  (Only 1):_ -> return (Delete h)
                  _          -> return Ignore
    return (currStatus, oldStatus)


-- | sets in the database the current problem build to the given build_id
setProblemBuild :: Int -> Tester ()
setProblemBuild buildId = do
  path <- testPath
  dbConn <- testDbConn
  liftIO $ withMVar dbConn (\conn -> (execute conn
    "INSERT INTO builds (path, build_id) VALUES (?, ?)ON DUPLICATE KEY UPDATE build_id=?" (path, buildId, path)))


-- | remove the problem from the builds database
removeProblemBuild :: Tester ()
removeProblemBuild = do
  path <- testPath
  dbConn <- testDbConn
  liftIO $ withMVar dbConn (\conn -> (execute conn
    "DELETE FROM builds WHERE path=?" (Only path)))


-- | from a list of (Arg opt conf) return a list of (Arg opt value)
--   where value comes from the configuration file path "prefix.conf"
getOptConfArgs :: Text -> [Arg] -> Tester [Arg]
getOptConfArgs prefix args =
  catMaybes <$> mapM optConfArg args
  where
    optConfArg arg = do
      cnf <- maybeConfigured (prefix<>"."<>(T.pack (_value arg)))
      return $ maybe Nothing (\x -> Just(arg{_value=x})) cnf


-- | from a list of (Arg Type opt meta) return a list of (Arg Type opt value)
--   where value comes from the problem metadata
getOptMetaArgs :: [Arg] -> Tester [Arg]
getOptMetaArgs args = do
  meta <- testMetadata
  let optMetaArg arg = do
      cnf <- lookupFromMeta (_value arg) meta
      return arg{_value=cnf}
  return $ mapMaybe (optMetaArg) args


-- | from a list of (Arg Type opt value) returns a list of [opt1, value1, ...]
concatArgs :: [Arg] -> [String]
concatArgs args = concatMap (\Arg{..} -> [_opt, _value]) args
