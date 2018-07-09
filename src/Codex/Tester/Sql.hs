{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Sql (
  sqlTester,
  sqlSelectTester,
  sqlEditTester,
  sqlSchemaTester,
  ) where

import           Codex.Tester
import           Data.Text(Text)
import qualified Data.Text as T
import           Control.Exception
import           Control.Applicative
import           Codex.Tester.Build


sqlTester :: Tester Result
sqlTester = sqlSelectTester <|> sqlEditTester <|> sqlSchemaTester


sqlSelectTester :: Tester Result
sqlSelectTester = tester "select" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluator.select"
  confArgs <- getOptConfArgs "language.sql.args"
              [ Arg Value "-H" "host"
              , Arg Value "-P" "port"
              , Arg Value "-u" "user_guest"
              , Arg Value "-p" "pass_guest"
              ]
  metaArgs <- getOptMetaArgs
              [ Arg Value "-d" "db-name" ]
  buildConfArgs <- getOptConfArgs "language.sql.args"
              [ Arg Value "-H" "host"
              , Arg Value "-P" "port"
              , Arg Value "-u" "user_schema"
              , Arg Value "-p" "pass_schema"
              ]
  buildMetaArgs <- getOptMetaArgs
              [ Arg Value "-i" "db-init-sql"
              , Arg File "-I" "db-init-file"
              ]
  answer <- liftIO (getSqlAnswer meta)
  buildSelectProblem =<< getBuildStatus (confArgs ++ metaArgs ++ buildConfArgs ++ buildMetaArgs)
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath -> do
      classify <$> unsafeExec evaluator
        ((concatArgs $ confArgs ++ metaArgs) ++ ["-A", answerFilePath, "-S", submittedFilePath]) ""


buildSelectProblem :: (BuildStatus, BuildStatus) -> Tester ()
buildSelectProblem x = do
  liftIO $ print x
  return ()


sqlEditTester :: Tester Result
sqlEditTester = tester "edit" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluator.edit"
  confArgs <- getOptConfArgs "language.sql.args"
              [ Arg Value "-H" "host"
              , Arg Value "-P" "port"
              , Arg Value "-uS" "user_schema"
              , Arg Value "-pS" "pass_schema"
              , Arg Value "-uE" "user_edit"
              , Arg Value "-pE" "pass_edit"
              , Arg Value "-D" "prefix"
              ]
  metaArgs <- getOptMetaArgs
              [ Arg Value "-i" "db-init-sql"
              , Arg File "-I" "db-init-file"
              ]
  answer <- liftIO (getSqlAnswer meta)
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath -> do
      classify <$> unsafeExec evaluator
        ((concatArgs $ confArgs ++ metaArgs) ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


sqlSchemaTester :: Tester Result
sqlSchemaTester = tester "schema" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluator.schema"
  confArgs <- getOptConfArgs "language.sql.args"
              [ Arg Value "-H" "host"
              , Arg Value "-P" "port"
              , Arg Value "-u" "user_schema"
              , Arg Value "-p" "pass_schema"
              , Arg Value "-D" "prefix"
              ]
  metaArgs <- getOptMetaArgs
              [ Arg Value "-i" "db-init-sql"
              , Arg File "-I" "db-init-file"
              ]
  answer <- liftIO (getSqlAnswer meta)
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath -> do
      classify <$> unsafeExec evaluator
        ((concatArgs $ confArgs ++ metaArgs) ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _)
  | match "Accepted" stdout              = accepted (dropFirstLn stdout)
  | match "Wrong Answer" stdout          = wrongAnswer (dropFirstLn stdout)
  | match "Runtime Error" stdout         = runtimeError (dropFirstLn stdout)
  | match "Compile Error" stdout         = compileError (dropFirstLn stdout)
  | match "Time Limit Exceeded" stdout   = timeLimitExceeded (dropFirstLn stdout)
  | match "Memory Limit Exceeded" stdout = memoryLimitExceeded (dropFirstLn stdout)
  where
    dropFirstLn = T.dropWhile (/='\n')
classify (_, stdout, stderr)             = miscError (stdout <> stderr)


getSqlAnswer :: Tester String
getSqlAnswer  = do
  opt <- metadata "answer-sql"
  case opt of
    Nothing ->
      liftIO $ throwIO $ miscError "missing answer-sql in metadata"
    Just answer ->
      return answer
