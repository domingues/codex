{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Build (
  buildRun,
  ) where

import           Codex.Tester.Monad
import           Control.Concurrent.MVar
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Data.Map.Strict as Map
import           Control.Monad.IO.Class
import           Control.Exception
import           Codex.Tester.Result (miscError)


buildRun :: IO () -> IO a -> Tester a
buildRun build run = do
  cache <- testBuildCache
  path <- testPath
  hash <- testHash
  liftIO $ print (hash)
  liftIO $ buildRun' cache path hash build run


buildRun' :: BuildCache -> String -> TestHash -> IO () -> IO a -> IO a
buildRun' buildCache path hash buildProblem runTest = do
  cache <- takeMVar buildCache -- lock cache table
  case Map.lookup path cache of
    Just mv -> do
      (buildStatus, rw) <- takeMVar mv -- lock curr prob
      putMVar buildCache cache -- release cache table
      case buildStatus of
        Right oldHash | oldHash==hash -> run mv rw -- no build
                      | otherwise     -> build mv rw -- rebuild
        Left oldHash  | oldHash==hash -> noRebuild mv rw
                      | otherwise     -> build mv rw -- rebuild
    Nothing -> do
      mv <- newEmptyMVar -- lock curr prob
      putMVar buildCache $ Map.insert path mv cache -- release cache table
      rw <- RWL.new
      build mv rw -- new build
  where
    build mv rw = do
      print ("build" :: String)
      RWL.acquireWrite rw -- wait until can build
      buildProblem
      RWL.releaseWrite rw -- build done
      run mv rw -- build success
      `onException` do
        RWL.releaseWrite rw -- build done
        putMVar mv (Left hash, rw) -- build fail
    run mv rw = do
      print ("run" :: String)
      RWL.acquireRead rw -- block building
      putMVar mv (Right hash, rw) -- release curr prob
      runTest `finally` RWL.releaseRead rw
    noRebuild mv rw = do
      putMVar mv (Left hash, rw) -- release curr prob
      throwIO $ miscError "previous build attempt failed"

