{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Build (
  setup,
  ) where

import           Control.Monad.IO.Class
import           Codex.Tester.Monad
import           Control.Concurrent.MVar
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Data.Map.Strict as Map


setup :: Tester () -> Tester (RWL.RWLock)
setup buildProblem = do
  path <- testPath
  hash <- testHash
  cache <- acquireCache                            -- lock cache table
  case Map.lookup path cache of
    Just (mv, rw) -> do
      hash' <- liftIO $ takeMVar mv                -- lock curr prob
      releaseCache cache                           -- release cache table
      if hash'==hash then do
        liftIO $ print ("no build"::String)
        noBuild mv rw hash
      else do
        liftIO $ print ("rebuilding"::String)
        liftIO $ RWL.acquireWrite rw               -- wait until can build
        build mv rw hash
    Nothing -> do
      liftIO $ print ("building"::String)
      mv <- liftIO newEmptyMVar                    -- lock curr prob
      rw <- liftIO RWL.newAcquiredWrite            -- init build
      releaseCache $ Map.insert path (mv, rw) cache-- release cache table
      build mv rw hash
  where
    noBuild mv rw hash = do
      liftIO $ RWL.acquireRead rw                  -- block building
      liftIO $ putMVar mv hash                     -- release curr prob
      return rw -- <- we need to releaseRead when the test finishes
    build mv rw hash = do
      buildProblem
      liftIO $ RWL.releaseWrite rw                 -- build done
      liftIO $ RWL.acquireRead rw                  -- block building
      liftIO $ putMVar mv hash                     -- release curr prob
      return rw -- <- we need to releaseRead when the test finishes
    acquireCache = do
      mv <- testBuildCache
      liftIO $ takeMVar mv
    releaseCache cache = do
      mv <- testBuildCache
      liftIO $ putMVar mv cache

