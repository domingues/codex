--------------------------------------------------------------------------
-- Test C code using Haskell QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.ClangQC (
  clangQCTester
  ) where

import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)

import           Control.Exception (catch)
import           Codex.Tester
import           System.Directory (copyFile)

clangQCTester :: Tester Result
clangQCTester = tester "quickcheck" $ do
  Code lang src <- testCode
  guard (lang == "c")
  path <- testFilePath
  let dir = takeDirectory path
  qcpath <- fromMaybe (replaceExtension path ".hs")
            <$> metadataPath "properties"
  assert (fileExists qcpath)
    ("properties file not found: " <> show qcpath)
  props  <- liftIO (T.readFile qcpath)
  ghc    <- configured "language.haskell.compiler"
  gcc    <- configured "language.c.compiler"
  limits <- configLimits "language.haskell.limits"
  qcArgs <- getQuickCheckArgs <$> testMetadata
  -- optional extra files to copy over
  files <- globPatterns dir =<< metadataWithDefault "files" []
  -- optional header for includes, prototypes, etc.
  header <- metadataWithDefault "header" ""
  let code = header <> "\n" <> src
  liftIO (clangRunner limits gcc ghc qcArgs files code props `catch` return)

clangRunner :: Limits
            -> String
            -> String
            -> [String]
            -> [FilePath]
            -> Text
            -> Text -> IO Result
clangRunner limits gcc_cmd ghc_cmd qcArgs files c_code props =
  withTempDir "codex" $ \dir -> do
      -- copy extra files to temp dir
      mapM_ (\f -> copyFile f (dir</>takeFileName f)) files
      let c_file  = dir </> "submit.c"
      let hs_file = dir </> "Main.hs"
      let obj_file = dir </> "submit.o"
      let exe_file = dir </> "Main"
      T.writeFile c_file c_code
      T.writeFile hs_file props
      gcc:cc_args <- parseArgs gcc_cmd
      ghc:hc_args <- parseArgs ghc_cmd
      let cc_args'= cc_args ++ ["-c", c_file, "-o", obj_file]
      let hc_args'= hc_args ++ ["-i"++dir, obj_file, hs_file, "-o", exe_file]
      chmod executable dir
      chmod writeable dir
      chmod readable c_file
      -- compile C code to object file
      runCompiler (Just limits) gcc cc_args'
      -- compile Haskell quickcheck driver
      runCompiler Nothing ghc hc_args'
      -- allow anyone to execute the binary (for safeExec)
      chmod readable exe_file
      -- execute and under safeExec and classify result
      classify <$> safeExec limits exe_file Nothing qcArgs ""

classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _)  = accepted stdout
classify (ExitFailure _, stdout, stderr)
  | match "Time Limit" stderr   = timeLimitExceeded msg
  | match "Memory Limit" stderr = memoryLimitExceeded msg
  | match "Command terminated by signal" stderr
                                = runtimeError msg
  | match "Failed!" stdout       = wrongAnswer stdout 
  | match "Command exited with non-zero status" stderr
                                = runtimeError msg
  | otherwise                  = miscError msg
  where msg = stdout <> stderr


