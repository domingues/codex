{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Sql (
  sqlTester,
  sqlSelectTester,
  sqlEditTester,
  sqlSchemaTester,
  ) where

import           Codex.Tester
import           Data.Char
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
  (argsFromBuild, run) <- do
    initFile <- metadataFile "db-init-file"
    case initFile of
      Nothing -> return ([], id)
      Just f -> do
        path <- testPath
        args <- concatArgs
            [ "-h" `joinConfArg` "host"
            , "-P" `joinConfArg` "port"
            , "-u" `joinConfArg` "user_schema"
            , "-p" `fuseConfArg` "pass_schema"
            ]
        dbName <- do
          dbPrefix <- maybeConfigured "language.sql.args.prefix"
          let name = map (\x -> if isAlphaNum x then x else '_') path
          return $ maybe name (\p -> p ++ "_" ++ name) dbPrefix
        hash <- testHash
        buildCache <- testBuildCache
        let run = buildRun buildCache path hash (setupProblem dbName f args)
        return (["-d", dbName], run)
  args <- concatArgs
            [ "-H" `joinConfArg` "host"
            , "-P" `joinConfArg` "port"
            , "-u" `joinConfArg` "user_guest"
            , "-p" `joinConfArg` "pass_guest"
            ]
  answer <- getSqlAnswer
  liftIO $ run $ withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
        withTemp "submit.sql" src $ \submittedFilePath ->
          classify <$> unsafeExec evaluator
            (args ++ argsFromBuild ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""
  where
    setupProblem :: String -> String -> [String] -> IO ()
    setupProblem dbName initFilePath args = do
      let sql = concat [ "DROP DATABASE IF EXISTS `", dbName, "`;"
                       , "CREATE DATABASE `", dbName, "`;"
                       , "USE `", dbName, "`;"
                       , "SOURCE ", initFilePath, ";"
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
  args <- concatArgs
            [ "-H" `joinConfArg` "host"
            , "-P" `joinConfArg` "port"
            , "-uS" `joinConfArg` "user_schema"
            , "-pS" `joinConfArg` "pass_schema"
            , "-uE" `joinConfArg` "user_edit"
            , "-pE" `joinConfArg` "pass_edit"
            , "-D" `joinConfArg` "prefix"
            , "-i" `joinMetaArg` "db-init-sql"
            , "-I" `joinMetaFileArg` "db-init-file"
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
  args <- concatArgs
            [ "-H" `joinConfArg` "host"
            , "-P" `joinConfArg` "port"
            , "-u" `joinConfArg` "user_schema"
            , "-p" `joinConfArg` "pass_schema"
            , "-D" `joinConfArg` "prefix"
            , "-i" `joinMetaArg` "db-init-sql"
            , "-I" `joinMetaFileArg` "db-init-file"
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


-- | deal with optional args
type OptArg = Maybe String
type OptArgs = Maybe [String]


concatArgs :: [Tester OptArgs] -> Tester [String]
concatArgs l = do
  l' <- sequence l
  return $ concat (catMaybes l')


joinArg :: String -> Tester OptArg -> Tester OptArgs
joinArg x y = fmap (\v -> [x, v]) <$> y


joinConfArg :: String -> Text -> Tester OptArgs
joinConfArg arg key = arg `joinArg` maybeConfigured ("language.sql.args." <> key)


fuseConfArg :: String -> Text -> Tester OptArgs
fuseConfArg arg key = arg `fuseArg` maybeConfigured ("language.sql.args." <> key)
  where fuseArg x y = fmap (\v -> [x++v]) <$> y


joinMetaArg :: String -> String -> Tester OptArgs
joinMetaArg arg key = arg `joinArg` metadata key


joinMetaFileArg :: String -> String -> Tester OptArgs
joinMetaFileArg arg key = arg `joinArg` metadataFile key
