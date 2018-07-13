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
  args <- concatArgs
            [ "-H" `joinConfArg` "host"
            , "-P" `joinConfArg` "port"
            , "-u" `joinConfArg` "user_guest"
            , "-p" `joinConfArg` "pass_guest"
            , "-d" `joinArg` getDatabaseName
            ]
  answer <- getSqlAnswer
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (args ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


getDatabaseName :: Tester (Maybe String)
getDatabaseName = do
  args <- concatArgs
            [ "-i" `joinMetaArg` "db-init-sql"
            , "-I" `joinMetaFileArg` "db-init-file"
            ]
  case args of
    [] -> metadata "db-name"
    _  -> do buildId <- setup (buildSelectProblem args) (cleanSelectProblem args)
             dbName <- buildIdToDbName buildId
             return (Just dbName)


buildIdToDbName :: BuildId -> Tester (String)
buildIdToDbName buildId = do
  dbPrefix <- maybeConfigured "language.sql.args.prefix"
  case dbPrefix of
    Nothing -> dbName ""
    Just v -> dbName v
  where
    dbName p = do
      case show buildId of
        '-':xs -> return $ p++('_':'_':xs)
        hash -> return $ p++('_':hash)


buildSelectProblem :: [String] -> BuildId -> Tester ()
buildSelectProblem args buildId = do
  args' <- concatArgs
            [ "-h" `joinConfArg` "host"
            , "-P" `joinConfArg` "port"
            , "-u" `joinConfArg` "user_schema"
            , "-p" `fuseConfArg` "pass_schema"
            ]
  let args'' = args
  liftIO $ print ("build"::String, args, args', buildId)
  return ()


cleanSelectProblem :: [String] -> BuildId -> Tester ()
cleanSelectProblem args buildId =  do
  liftIO $ print ("clean"::String, args, buildId)
  return ()


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
type OptArg = Maybe String
type OptArgs = Maybe [String]


concatArgs :: [Tester OptArgs] -> Tester [String]
concatArgs l = do
  l' <- sequence l
  return $ concat (catMaybes l')


joinArg :: String -> Tester (OptArg) -> Tester OptArgs
joinArg x y = fmap (\v -> [x, v]) <$> y


joinConfArg :: String -> Text -> Tester OptArgs
joinConfArg arg key = arg `joinArg` maybeConfigured ("language.sql.args." <> key)


fuseConfArg :: String -> Text -> Tester OptArgs
fuseConfArg arg key = arg `fuseArg` maybeConfigured ("language.sql.args." <> key)
  where fuseArg x y = fmap (\v -> [x++v]) <$> y


joinMetaArg :: String -> String -> Tester OptArgs
joinMetaArg arg key = arg `joinArg` metadata key


joinMetaFileArg :: String -> String -> Tester OptArgs
joinMetaFileArg arg key = arg `joinFileArg` metadata key
  where
    joinFileArg x y = do
      y' <- y
      case y' of
        Nothing -> return Nothing
        Just p -> do
          tPath <- testPath
          let path = takeDirectory tPath </> p
          dependFile path
          return (Just [x, path])

