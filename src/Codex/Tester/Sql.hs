{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Sql (
  sqlTester,
  sqlSelectTester,
  sqlEditTester,
  sqlSchemaTester,
  ) where

import           Codex.Tester
import           Data.Maybe
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
  buildArgs <- concatOptArgs
                [ "-H" `optConfArg` "host"
                , "-P" `optConfArg` "port"
                , "-u" `optConfArg` "user_guest"
                , "-p" `optConfArg` "pass_guest"
                , "-i" `optMetaArg` "db-init-sql"
                , "-I" `optMetaFileArg` "db-init-file"
                ]
  buildSelectProblem buildArgs =<< getBuildStatus
  args <- concatOptArgs
                [ "-H" `optConfArg` "host"
                , "-P" `optConfArg` "port"
                , "-u" `optConfArg` "user_guest"
                , "-p" `optConfArg` "pass_guest"
                , "-d" `optMetaArg` "db-name"
                ]
  answer <- getSqlAnswer
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (args ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


buildSelectProblem :: [String] -> (BuildStatus, BuildStatus) -> Tester ()
buildSelectProblem args x = do
  liftIO $ print args
  liftIO $ print x
  return ()


sqlEditTester :: Tester Result
sqlEditTester = tester "edit" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluator.edit"
  args <- concatOptArgs
                [ "-H" `optConfArg` "host"
                , "-P" `optConfArg` "port"
                , "-uS" `optConfArg` "user_schema"
                , "-pS" `optConfArg` "pass_schema"
                , "-uE" `optConfArg` "user_edit"
                , "-pE" `optConfArg` "pass_edit"
                , "-D" `optConfArg` "prefix"
                , "-i" `optMetaArg` "db-init-sql"
                , "-I" `optMetaArg` "db-init-file"
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
  evaluator <- configured "language.sql.evaluator.schema"
  args <- concatOptArgs
                [ "-H" `optConfArg` "host"
                , "-P" `optConfArg` "port"
                , "-u" `optConfArg` "user_schema"
                , "-p" `optConfArg` "pass_schema"
                , "-D" `optConfArg` "prefix"
                , "-i" `optMetaArg` "db-init-sql"
                , "-I" `optMetaArg` "db-init-file"
                ]
  answer <- getSqlAnswer
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (args ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


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


-- | deal with optional args
type OptArg = Maybe (String, String)


concatOptArgs :: [Tester OptArg] -> Tester [String]
concatOptArgs l = do
  l' <- sequence l
  return $ concatMap (\(x, y) -> [x, y]) (catMaybes l')


optConfArg :: String -> Text -> Tester OptArg
optConfArg arg key = arg `optArg` maybeConfigured ("language.sql.args." <> key)
  where optArg x y = fmap (\v -> (x, v)) <$> y


optMetaArg :: String -> String -> Tester OptArg
optMetaArg arg key = arg `optArg` metadata key
  where optArg x y = fmap (\v -> (x, v)) <$> y


optMetaFileArg :: String -> String -> Tester OptArg
optMetaFileArg arg key = arg `optFileArg` metadata key
  where
    optFileArg x y = do
      y' <- y
      case y' of
        Nothing -> return Nothing
        Just path -> do
          dependFile path
          return (Just (x, path))

