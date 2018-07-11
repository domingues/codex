{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Build (
  BuildId,
  setup,
  ) where

import           Codex.Tester
import           Control.Concurrent.MVar
import           Database.SQLite.Simple


type BuildId = Int
data BuildStatus = Ok BuildId
                 | Build BuildId
                 deriving Show
data CleanStatus = Clean BuildId
                 | Ignore
                 deriving Show


setup :: (BuildId -> Tester ()) -> (BuildId -> Tester ()) -> Tester BuildId
setup fBuild fClean = do
  buildStatus <- getBuildStatus
  case buildStatus of
    (Ok buildId, Clean cleanBuildId) -> do
--      removeProblemBuild
      fClean cleanBuildId
      return buildId
    (Build buildId, Clean cleanBuildId) -> do
--      removeProblemBuild
      fClean cleanBuildId
      fBuild buildId
--      setProblemBuild buildId
      return buildId
    (Build buildId, Ignore) -> do
      fBuild buildId
--      setProblemBuild buildId
      return buildId
    (Ok buildId, Ignore) -> return buildId


-- | returns the current build status and the problem old build status
getBuildStatus :: Tester (BuildStatus, CleanStatus)
getBuildStatus = do
  path <- testPath
  currBuildId <- problemBuildId
  dbConn <- testDbConn
  qOldBuildId <- liftIO $ withMVar dbConn (\conn -> query conn
        "SELECT build_id FROM builds WHERE path=?" (Only path) :: IO [Only BuildId])
  let oldBuildId = case qOldBuildId of
        Only h:_ -> Just h
        _        -> Nothing
  if oldBuildId == Just currBuildId then
    return (Ok currBuildId, Ignore)
  else do
    qAlreadyBuild <- liftIO $ withMVar dbConn (\conn -> query conn
          "SELECT build_id FROM builds WHERE build_id=? LIMIT 1" (Only currBuildId) :: IO [Only BuildId])
    let currStatus = case qAlreadyBuild of
          _:_ -> Ok currBuildId
          _   -> Build currBuildId
    oldStatus <- case oldBuildId of
          Nothing -> return Ignore
          Just h  -> do
            qOldBuildUsage <- liftIO $ withMVar dbConn (\conn -> query conn
                  "SELECT count(*) FROM builds WHERE build_id=?" (Only h) :: IO [Only BuildId])
            case qOldBuildUsage of
                  Only 1:_ -> return (Clean h)
                  _        -> return Ignore
    return (currStatus, oldStatus)


-- | sets in the database the current problem build to the given build_id
setProblemBuild :: Int -> Tester ()
setProblemBuild buildId = do
  path <- testPath
  dbConn <- testDbConn
  liftIO $ withMVar dbConn (\conn -> execute conn
    "INSERT INTO builds (path, build_id) VALUES (?, ?) ON DUPLICATE KEY UPDATE build_id=?" (path, buildId, path))


-- | remove the problem from the builds database
removeProblemBuild :: Tester ()
removeProblemBuild = do
  path <- testPath
  dbConn <- testDbConn
  liftIO $ withMVar dbConn (\conn -> execute conn
    "DELETE FROM builds WHERE path=?" (Only path))
