{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Build (
  buildRun,
  ) where

import           Codex.Tester.Monad
import           Control.Concurrent.MVar
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Data.Map.Strict as Map
import           Control.Exception
import           Codex.Tester.Result (miscError)


buildRun :: BuildCache -> String -> TestHash -> IO () -> IO a -> IO a
buildRun buildCache path hash buildProblem run = do
  rw <- setup buildCache path hash buildProblem
  RWL.acquireRead rw -- block building
  finally run (RWL.releaseRead rw)


setup :: BuildCache -> String -> TestHash -> IO () -> IO (RWL.RWLock)
setup buildCache path hash buildProblem = do
  cache <- takeMVar buildCache -- lock cache table
  case Map.lookup path cache of
    Just mv -> do
      (buildStatus, rw) <- takeMVar mv -- lock curr prob
      putMVar buildCache $ cache -- release cache table
      let noBuild = do
          putMVar mv (Right hash, rw) -- release curr prob
          return rw
      let rebuild = do
          RWL.acquireWrite rw -- wait until can build
          onException (do
             buildProblem
             RWL.releaseWrite rw -- build done
             putMVar mv (Right hash, rw) -- release curr prob
            ) (do
             RWL.releaseWrite rw -- build done
             putMVar mv (Left hash, rw) -- build fail, release curr prob
            )
          return rw
      let noRebuild = do
          putMVar mv (Left hash, rw) -- release curr prob
          throwIO $ miscError "previous build failed"
      case buildStatus of
        Right oldHash | oldHash==hash -> noBuild
                      | otherwise     -> rebuild
        Left oldHash  | oldHash==hash -> noRebuild
                      | otherwise     -> rebuild
    Nothing -> do
      mv <- newEmptyMVar -- lock curr prob
      rw <- RWL.newAcquiredWrite -- init build
      putMVar buildCache $ Map.insert path mv cache -- release cache table
      onException (do
          buildProblem
          RWL.releaseWrite rw -- build done
          putMVar mv (Right hash, rw) -- release curr prob
        ) (do
          RWL.releaseWrite rw -- build done
          putMVar mv (Left hash, rw) -- build fail, release curr prob
        )
      return rw

