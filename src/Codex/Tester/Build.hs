{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Build (
  setup,
  ) where

import           Control.Monad.IO.Class
import           Codex.Tester.Monad
import           Control.Concurrent.MVar
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Data.Map.Strict as Map
import           Control.Exception


buildRun :: IO (RWL.RWLock) -> IO a -> IO a
buildRun build run = do
  rw <- build
  r <- run
  RWL.releaseRead rw
  return r


setup :: BuildCache -> String -> TestHash -> IO () -> IO (RWL.RWLock)
setup buildCache path hash buildProblem = do
  cache <- takeMVar buildCache                            -- lock cache table
  case Map.lookup path cache of
    Just (mv, rw) -> do
      hash' <- takeMVar mv                                -- lock curr prob
      putMVar buildCache $ cache                          -- release cache table
      if hash'==hash then do
        print ("no build"::String)
        putMVar mv hash                                   -- release curr prob
        RWL.acquireRead rw                                -- block building
        return rw -- releaseRead after the test finishes
      else do
        print ("rebuilding"::String)
        RWL.acquireWrite rw                               -- wait until can build
        onException (do
            buildProblem
            RWL.releaseWrite rw                           -- build done
            putMVar mv hash                               -- release curr prob
          ) (do
            RWL.releaseWrite rw                           -- build done
            putMVar mv hash'                              -- build fail
          )
        RWL.acquireRead rw                                -- block building
        return rw -- releaseRead after the test finishes
    Nothing -> do
      print ("building"::String)
      mv <- newEmptyMVar                                  -- lock curr prob
      rw <- RWL.newAcquiredWrite                          -- init build
      putMVar buildCache $ Map.insert path (mv, rw) cache -- release cache table
      onException (do
          buildProblem
          RWL.releaseWrite rw                             -- build done
          putMVar mv hash                                 -- release curr prob
        ) (do
          RWL.releaseWrite rw                             -- build done
          putMVar mv 0                                    -- build fail
        )
      RWL.acquireRead rw                                  -- block building
      return rw -- releaseRead after the test finishes

