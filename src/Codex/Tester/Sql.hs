{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.Sql (
  sqlTester,
  sqlSelectTester,
  sqlEditTester,
  sqlSchemaTester,
  ) where

import           Codex.Tester
import           Data.Text(Text)
import           Control.Exception
import           Control.Applicative


sqlTester :: Tester Result
sqlTester = sqlSelectTester <|> sqlEditTester <|> sqlSchemaTester


sqlSelectTester :: Tester Result
sqlSelectTester = tester "select" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.select.evaluator"
  confArgs <- getOptConfArgs "language.sql.select.args"
              [ ("-H", "host")
              , ("-P", "port")
              , ("-u", "user")
              , ("-p", "pass")
              ]
  metaArgs <- getOptMetaArgs
              [ ("-d", "db-name") ]
  answer <- getSqlAnswer 
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (confArgs ++ metaArgs ++ ["-a", answer, "-S", submittedFilePath]) ""


sqlEditTester :: Tester Result
sqlEditTester = tester "edit" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.edit.evaluator"
  confArgs <- getOptConfArgs "language.sql.edit.args"
             [ ("-H", "host")
             , ("-P", "port")
             , ("-uS", "user_schema")
             , ("-pS", "pass_schema")
             , ("-uE", "user_edit")
             , ("-pE", "pass_edit")
             , ("-D", "prefix")
             ]
  metaArgs <- getOptMetaArgs
             [ ("-i", "db-init-sql")
             , ("-I", "db-init-file")
             ]
  answer <- getSqlAnswer 
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (confArgs ++ metaArgs ++ ["-a", answer,"-S", submittedFilePath]) ""


sqlSchemaTester :: Tester Result
sqlSchemaTester = tester "schema" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.schema.evaluator"
  confArgs <- getOptConfArgs "language.sql.schema.args"
             [ ("-H", "host")
             , ("-P", "port")
             , ("-u", "user")
             , ("-p", "pass")
             , ("-D", "prefix")
             ]
  metaArgs <- getOptMetaArgs
             [ ("-i", "db-init-sql")
             , ("-I", "db-init-file")
             ]
  answer <- getSqlAnswer 
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (confArgs ++ metaArgs ++ ["-a", answer, "-S", submittedFilePath]) ""


getOptConfArgs :: Text -> [(String, Text)] -> Tester [String]
getOptConfArgs prefix opts =
  concat <$> mapM optConfArg opts
  where
    optConfArg (opt, key) = do
      cnf <- maybeConfigured (prefix<>"."<>key)
      return $ maybe [] (\x -> [opt, x]) cnf

getOptMetaArgs :: [(String, String)] -> Tester [String]
getOptMetaArgs opts
  = concat <$> mapM optMetaArg opts
  where
    optMetaArg (opt,key) = maybe [] (\x -> [opt, x]) <$> metadata key




classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | match "Accepted" stderr            = accepted stdout
  | match "Wrong Answer" stderr        = wrongAnswer stdout
  | match "Runtime Error" stderr       = runtimeError stdout
  | match "Compile Error" stderr       = compileError stdout
  | match "Time Limit" stderr          = timeLimitExceeded stdout
  | match "Memory Limit" stderr        = memoryLimitExceeded stdout
  | otherwise                          = miscError (stderr <> stdout)


getSqlAnswer :: Tester String
getSqlAnswer  = do
  opt <- metadata "answer-sql"
  case opt of
    Nothing ->
      liftIO $ throwIO $ miscError "missing answer-sql in metadata"
    Just answer ->
      return answer
