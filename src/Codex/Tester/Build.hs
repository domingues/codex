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
                 | Set BuildId
                 deriving Show
data CleanStatus = Clean BuildId
                 | Ignore
                 deriving Show


setup :: (BuildId -> Tester ()) -> (BuildId -> Tester ()) -> Tester BuildId
setup fBuild fClean = do
  buildStatus <- getBuildStatus
  case buildStatus of
    (Ok buildId, Ignore)            ->    return buildId
    (Set buildId, Ignore)           -> do setProblemBuild buildId
                                          return buildId
    (Build buildId, Ignore)         -> do fBuild buildId
                                          setProblemBuild buildId
                                          return buildId
    (Set buildId, Clean cBuildId)   -> do setProblemBuild buildId
                                          fClean cBuildId
                                          return buildId
    (Build buildId, Clean cBuildId) -> do fBuild buildId
                                          setProblemBuild buildId
                                          fClean cBuildId
                                          return buildId


-- | returns the current build status and the problem old build status
getBuildStatus :: Tester (BuildStatus, CleanStatus)
getBuildStatus = do
  path <- testPath
  currBuildId <- testHash
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
          _:_ -> Set currBuildId
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
    "INSERT OR REPLACE INTO builds (path, build_id) VALUES (?, ?)" (path, buildId))
