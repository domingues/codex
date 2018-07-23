{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Sql (
  sqlTester,
  sqlSelectTester,
  sqlEditTester,
  sqlSchemaTester,
  ) where

import           Codex.Tester
import           Data.Char
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
  (dbNameArg, run) <- do
    initFile <- dependsMetadataFile "db-init-file"
    case initFile of
      Nothing -> return ([], liftIO)
      Just file -> do
        args <- getDependsOptConfArgs "language.sql.args"
                  [ ("-h", (\a b->[a, b]), "host")
                  , ("-P", (\a b->[a, b]), "port")
                  , ("-u", (\a b->[a, b]), "user_schema")
                  , ("-p", (\a b->[a++b]), "pass_schema")
                  ]
        dbName <- do
          path <- testPath
          dbPrefix <- maybeConfigured "language.sql.args.prefix"
          let name = map (\x -> if isAlphaNum x then x else '_') path
          return $ maybe name (\p -> p ++ "_" ++ name) dbPrefix
        let run = buildRun (setupProblem dbName file args)
        return (["-d", dbName], run)
  args <- getOptConfArgs "language.sql.args"
            [ ("-H", "host")
            , ("-P", "port")
            , ("-u", "user_guest")
            , ("-p", "pass_guest")
            ]
  answer <- getSqlAnswer
  run $ withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (args ++ dbNameArg ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""
  where
    setupProblem :: String -> String -> [String] -> IO ()
    setupProblem dbName initFilePath args = do
      let sql = concat [ "DROP DATABASE IF EXISTS `", dbName, "`;\n"
                       , "CREATE DATABASE `", dbName, "`;\n"
                       , "USE `", dbName, "`;\n"
                       , "SOURCE ", initFilePath, ";\n"
                       ]
      exec <- unsafeExec "mysql" args (T.pack sql)
      case exec of
        (ExitSuccess, _, _) -> return ()
        (_, stdout, stderr) -> throwIO $ miscError (stdout <> stderr)


sqlEditTester :: Tester Result
sqlEditTester = tester "edit" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluator.edit"
  confArgs <- getOptConfArgs "language.sql.args"
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
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (confArgs ++ metaArgs ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


sqlSchemaTester :: Tester Result
sqlSchemaTester = tester "schema" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluator.schema"
  confArgs <- getOptConfArgs "language.sql.args"
             [ ("-H", "host")
             , ("-P", "port")
             , ("-u", "user_schema")
             , ("-p", "pass_schema")
             , ("-D", "prefix")
             ]
  metaArgs <- getOptMetaArgs
             [ ("-i", "db-init-sql")
             , ("-I", "db-init-file")
             ]
  answer <- getSqlAnswer
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (confArgs ++ metaArgs ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


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


getDependsOptConfArgs :: Text -> [(String, String->String->[String], Text)] -> Tester [String]
getDependsOptConfArgs prefix opts =
  concat <$> mapM optConfArg opts
  where
    optConfArg (opt, glue, key) = do
      cnf <- dependsMaybeConfigured (prefix<>"."<>key)
      return $ maybe [] (opt `glue`) cnf


classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _)
  | T.isPrefixOf "Accepted" stdout              = accepted (dropFirstLn stdout)
  | T.isPrefixOf "Wrong Answer" stdout          = wrongAnswer (dropFirstLn stdout)
  | T.isPrefixOf "Runtime Error" stdout         = runtimeError (dropFirstLn stdout)
  | T.isPrefixOf "Compile Error" stdout         = compileError (dropFirstLn stdout)
  | T.isPrefixOf "Time Limit Exceeded" stdout   = timeLimitExceeded (dropFirstLn stdout)
  | T.isPrefixOf "Memory Limit Exceeded" stdout = memoryLimitExceeded (dropFirstLn stdout)
  where
    dropFirstLn = T.dropWhile (/='\n')
classify (_, stdout, stderr)                    = miscError (stdout <> stderr)


getSqlAnswer :: Tester String
getSqlAnswer  = do
  opt <- metadata "answer-sql"
  case opt of
    Nothing ->
      liftIO $ throwIO $ miscError "missing answer-sql in metadata"
    Just answer ->
      return answer
