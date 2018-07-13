{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Build (
  setup,
  ) where

import           Codex.Tester
import           Control.Concurrent.MVar
import           Database.SQLite.Simple


type BuildId = Int
data BuildStatus = Ok
                 | Build BuildId
                 deriving Show


setup :: Tester () -> Tester ()
setup fBuild = do
  buildStatus <- getBuildStatus
  case buildStatus of
    Ok              -> return ()
    (Build buildId) -> fBuild >> setProblemBuild buildId


-- | returns the current build status
getBuildStatus :: Tester BuildStatus
getBuildStatus = do
  path <- testPath
  buildId <- testHash
  dbConn <- testDbConn
  qIsBuild <- liftIO $ withMVar dbConn (\conn -> query conn
        "SELECT build_id FROM builds WHERE path=? and build_id=? LIMIT 1" (path, buildId) :: IO [Only BuildId])
  case qIsBuild of
        [] -> return (Build buildId)
        _  -> return Ok


-- | sets in the database the current problem build to the given build_id
setProblemBuild :: BuildId -> Tester ()
setProblemBuild buildId = return ()
setProblemBuild buildId = do
  path <- testPath
  dbConn <- testDbConn
  liftIO $ withMVar dbConn (\conn -> execute conn
    "INSERT OR REPLACE INTO builds (path, build_id) VALUES (?, ?)" (path, buildId))
