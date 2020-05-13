{-# LANGUAGE OverloadedStrings #-}
--
-- | Test SQL queries using MySQL interpreter
--
module Codex.Tester.Sql (
  sqlTester
  ) where

import           Codex.Tester
import           Control.Applicative ((<|>))
import qualified Data.Text as T
import           Control.Exception (throwIO)
import           Data.Char
import           Data.Text (Text)

sqlTester :: Tester Result
sqlTester = sqlSelectTester <|> sqlEditTester <|> sqlSchemaTester


sqlSelectTester :: Tester Result
sqlSelectTester = tester "select" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluators.select"
  (dbNameArg, run) <- do
    initFile <- metadataFile "db-init-file"
    case initFile of
      Nothing -> return ([], liftIO)
      Just file -> do
        dependsOn ("db-init-file" :: String)
        dependsOnFile file
        args <- concat <$> sequence [
                  joinMaybe "-h" <$> confArg "host"
                , joinMaybe "-P" <$> confArg "port"
                , joinMaybe "-u" <$> confArg "user_schema"
                , fuseMaybe "-p" <$> confArg "pass_schema"
                ]
        dependsOn args
        dbName <- do
          path <- testFilePath
          dbPrefix <- confArg "prefix"
          let name = map (\x -> if isAlphaNum x then x else '_') path
          return $ maybe name (\p -> p ++ "_" ++ name) dbPrefix
        let run = buildRun (setupProblem dbName file args)
        return (["-d", dbName], run)
  args <- concat <$> sequence [
            joinMaybe "-H" <$> confArg "host"
          , joinMaybe "-P" <$> confArg "port"
          , joinMaybe "-u" <$> confArg "user_guest"
          , joinMaybe "-p" <$> confArg "pass_guest"
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
  evaluator <- configured "language.sql.evaluators.edit"
  args <- concat <$> sequence [
            joinMaybe "-H" <$> confArg "host"
          , joinMaybe "-P" <$> confArg "port"
          , joinMaybe "-uS" <$> confArg "user_schema"
          , joinMaybe "-pS" <$> confArg "pass_schema"
          , joinMaybe "-uE" <$> confArg "user_edit"
          , joinMaybe "-pE" <$> confArg "pass_edit"
          , joinMaybe "-D" <$> confArg "prefix"
          , joinMaybe "-i" <$> metadata "db-init-sql"
          , joinMaybe "-I" <$> metadataFile "db-init-file"
          ]
  answer <- getSqlAnswer
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (args ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


sqlSchemaTester :: Tester Result
sqlSchemaTester = tester "schema" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluators.schema"
  args <- concat <$> sequence [
            joinMaybe "-H" <$> confArg "host"
          , joinMaybe "-P" <$> confArg "port"
          , joinMaybe "-u" <$> confArg "user_schema"
          , joinMaybe "-p" <$> confArg "pass_schema"
          , joinMaybe "-D" <$> confArg "prefix"
          , joinMaybe "-i" <$> metadata "db-init-sql"
          , joinMaybe "-I" <$> metadataFile "db-init-file"
          ]
  answer <- getSqlAnswer
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (args ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


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


metadataFile :: String -> Tester (Maybe String)
metadataFile path = do
  value <- metadata path
  case value of
    Nothing -> return Nothing
    Just v -> do
      tp <- testFilePath
      return $ Just (takeDirectory tp </> v)


confArg key = maybeConfigured $ "language.sql.args." <> key
joinMaybe key = maybe [] (\v -> [key, v])
fuseMaybe key = maybe [] (\v -> [key ++ v])
