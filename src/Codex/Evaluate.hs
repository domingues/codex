{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Evaluate(
  -- * evaluate a single submission
  evaluate,
  reevaluate,
  cancelPending
  ) where

import qualified Data.Text as T
import           Data.Maybe
import           Data.Time.LocalTime
import           Control.Monad.State
import           Control.Concurrent(ThreadId) 
import           Control.Exception  (SomeException, catch)
import           System.FilePath

import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple as S

import           Data.Configurator.Types(Config)

import           Codex.Application
import           Codex.Tasks
import           Codex.Utils
import           Codex.Page
import           Codex.Submission
import           Codex.Tester
import           Codex.Time



-- | evaluate a single submissions
evaluate :: Submission -> Codex ThreadId
evaluate sub = do
  tester <- gets _tester
  semph <- gets _semph
  action <- evaluatorWith tester sub
  liftIO $ forkSingle semph action

-- | re-evaluate a list of submissions
reevaluate :: [Submission] -> Codex ()
reevaluate subs = do
  tester <- gets _tester
  semph <- gets _semph
  tasks <- gets _tasks
  actions <- mapM (evaluatorWith tester) subs
  liftIO $ forkMany semph tasks actions

-- | cancel pending evaluations
cancelPending :: Codex ()
cancelPending = reevaluate []


-- | evaluate a submission with a specific tester
-- (1st time or re-evaluation);
-- runs code tester in separate thread
-- uses a semaphore for limiting concurrency 
evaluatorWith :: Tester Result -> Submission -> Codex (IO ())
evaluatorWith tester Submission{..} = do
  sqlite <- S.getSqliteState
  buildCache <- gets _buildCache
  events <- getEvents
  root <- getDocumentRoot
  conf <- getSnapletUserConfig
  return $ do                           -- ^ return evaluation IO action
    let filepath = root </> submitPath  -- ^ file path to exercise 
    page <- readMarkdownFile filepath
    tz <- getCurrentTimeZone
    let optInt = evalInterval tz events (metaInterval $ pageMeta page)
    case optInt of
      Left err ->
        updateSubmission sqlite submitId (wrongInterval err) Valid
      Right int -> do
        let timing = timeInterval submitTime int
        result <- testWrapper conf buildCache page filepath submitCode submitUser tester
                  `catch`
                  (\(e::SomeException) ->
                      return (miscError $ T.pack $ show e))
        updateSubmission sqlite submitId result timing

-- | wrapper to set default limits and run a tester
testWrapper cfg buildCache page path code user action
  = fromMaybe invalidTester <$> runTester cfg buildCache page path code user action

wrongInterval :: String -> Result
wrongInterval msg = miscError ("invalid time interval: " <> T.pack msg)
 
invalidTester :: Result
invalidTester = miscError "no acceptable tester configured"


