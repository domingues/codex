{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Tester.Utils where


import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Control.Exception
import           Control.Monad       (when, unless)
import           Control.Monad.Trans
import           System.IO
import           System.IO.Temp
import           System.Exit
import           System.Directory
import           System.Posix.Files
import           System.Posix.Types (FileMode)
import           System.Process.Text (readProcessWithExitCode)
 
import           Data.Bits
import           Data.Maybe(catMaybes)

import           Text.Pandoc(Meta)
import           Codex.Page(lookupFromMeta)
import           Codex.Tester.Result
import           Codex.Tester.Limits

-- | match a piece of text
match :: Text -> Text -> Bool
match = T.isInfixOf

-- | aquire and release temporary files and directories
withTemp :: MonadIO m => FilePath -> Text -> (FilePath -> IO a) -> m a
withTemp name contents k
  = liftIO $ withSystemTempFile name (\f h -> T.hPutStr h contents >> hClose h >> k f)


withTempDir :: MonadIO m => FilePath -> (FilePath -> IO a) -> m a
withTempDir name k
  = liftIO $ withSystemTempDirectory name k

-- | ensure an test assertion; throws MiscError otherwise
assert :: MonadIO m => IO Bool -> String -> m ()
assert chk msg = liftIO $ do
  c <- chk
  unless c $ throwIO (AssertionFailed msg)

fileExists :: FilePath -> IO Bool
fileExists = doesFileExist


-- | remove files if they exist, silently ignore otherwise
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = do b<-doesFileExist f; when b (removeFile f)

cleanupFiles :: [FilePath] -> IO ()
cleanupFiles = mapM_ removeFileIfExists


chmod :: MonadIO m => (FileMode -> FileMode) -> FilePath -> m ()
chmod chg filepath = liftIO $ do
  mode <- fileMode <$> getFileStatus filepath
  setFileMode filepath (chg mode)


readable :: FileMode -> FileMode
readable mode =
  mode .|. ownerReadMode .|. groupReadMode .|. otherReadMode

executable :: FileMode -> FileMode
executable mode =
  mode .|. ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode

writeable :: FileMode -> FileMode
writeable mode =
  mode .|. ownerWriteMode .|. groupWriteMode .|. otherWriteMode




runCompiler :: FilePath -> [String] -> IO ()
runCompiler cmd args = do
  (exitCode, _, err) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitFailure _ ->
      throwIO (compileError err)
    ExitSuccess ->
      return ()


-- | run a command under SafeExec
safeExec :: Limits
          -> FilePath            -- ^ command
          -> [String]           -- ^ arguments
          -> Text               -- ^ stdin
          -> IO (ExitCode, Text, Text) -- ^ code, stdout, stderr
safeExec Limits{..} cmd args stdin
  = let mkArg opt = maybe [] (\c -> [opt, show c])
        args' = mkArg "--cpu" maxCpuTime
                ++
                mkArg "--clock" maxClockTime
                ++
                mkArg "--mem" maxMemory
                ++
                mkArg "--stack" maxStack
                ++
                mkArg "--fsize" maxFSize
                ++
                mkArg "--core" maxCore
                ++
                mkArg "--nproc" numProc
                ++
                ["--exec", cmd] ++
                args
    in
      readProcessWithExitCode "safeexec" args' stdin

unsafeExec :: FilePath                 -- ^ command
          -> [String]                  -- ^ arguments
          -> Text                      -- ^ stdin
          -> IO (ExitCode, Text, Text) -- ^ code, stdout, stderr
unsafeExec = readProcessWithExitCode

-- get QuickCheck runner command line arguments
getQuickCheckArgs :: Meta -> [String]
getQuickCheckArgs meta
  = catMaybes
    [ lookupArg "maxSuccess"
    , lookupArg "maxSize"
    , lookupArg "maxDiscardRatio"
    , lookupArg "randSeed"
    ]
  where
    lookupArg key
      = fmap (\val -> "--" ++ key ++ "=" ++ val) (lookupFromMeta key meta)


