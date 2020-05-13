{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
-- | Tester monad for writing testers for submissions
-- | allows access to an environment, running IO and failure (i.e. passing)
--
-}
module Codex.Tester.Monad (
  Tester,
  runTester,
  configured,
  maybeConfigured,
  configLimits,
  testConfig,
  testFilePath,
  testCode,
  testPage,
  testMetadata,
  testUser,
  metadata,
  metadataWithDefault,
  metadataPath,
  BuildCache,
  initBuildCache,
  buildRun,
  dependsOn,
  dependsOnFile,
  ) where



import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe 
import           Control.Monad.Trans.Reader

import           Codex.Types (Page, Code, UserLogin)
import           Codex.Submission.Types
import           Text.Pandoc (Meta)
import           Codex.Page
import           Codex.Tester.Limits
import           System.FilePath
import           Data.Maybe (fromMaybe)

import           Data.Hashable
import qualified Data.Map.Strict                  as Map
import           Data.Time.Clock                  (UTCTime)
import           Control.Concurrent.MVar
import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Exception
import           Control.Monad.Trans.State
import qualified System.Directory                 as D

-- | a monad for testing scripts
-- allows access to a test environment, IO and failure (i.e. passing)
newtype Tester a
  = Tester { unTester :: ReaderT TestEnv (StateT TestHash (MaybeT IO)) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

-- | the testing environment
data TestEnv
   = TestEnv { _testConfig :: Config       -- ^ configuration record
             , _testFilePath :: FilePath   -- ^ file path to exercise page
             , _testPage :: Page           -- ^ exercise page
             , _testSubmission :: Submission 
             , _testBuildCache :: BuildCache
             } 


-- | run function for testers
runTester :: Tester a
          -> Config -> BuildCache -> FilePath -> Page -> Submission
          -> IO (Maybe a)
runTester m cfg cache filepath page sub 
  = let env = TestEnv cfg filepath page sub cache
    in runMaybeT $ evalStateT (runReaderT (unTester m) env) 0


-- | fetch parameters from environment
testConfig :: Tester Config
testConfig = Tester (asks _testConfig)

testFilePath :: Tester FilePath
testFilePath = Tester (asks _testFilePath)

testCode :: Tester Code
testCode = Tester (submitCode <$> asks _testSubmission)

testUser :: Tester UserLogin
testUser = Tester (submitUser <$> asks _testSubmission)

testPage :: Tester Page
testPage = Tester (asks _testPage)

testMetadata :: Tester Meta
testMetadata = pageMeta <$> testPage


-- | get a medata value given the key
metadata :: FromMetaValue a => String -> Tester (Maybe a)
metadata key = do
  meta <- testMetadata
  return (lookupFromMeta key meta)

-- | get an relative path from metadata 
metadataPath :: String -> Tester (Maybe FilePath)
metadataPath key = do
  dir <- takeDirectory <$> testFilePath  -- directory for exercise page
  fmap (dir </>) <$> metadata key     -- lookup and make a relative path


-- | get a optional value given the default
metadataWithDefault :: FromMetaValue a => String -> a -> Tester a
metadataWithDefault key def = fromMaybe def <$> metadata key

-- | fetch a configured value; return Nothing if key not present
maybeConfigured :: Configured a => Name -> Tester (Maybe a)
maybeConfigured key = do
  cfg <- testConfig
  liftIO $ Conf.lookup cfg key

-- | fetch a configuration value
-- throws an exception if key is not present
configured :: Configured a => Name -> Tester a
configured key = do
  cfg <- testConfig
  liftIO $ Conf.require cfg key


-- | ask configured limits from the tester environment
-- overrides default config with the specific one
configLimits :: Name -> Tester Limits
configLimits key = do
  cfg <- testConfig
  liftIO $ do
    def  <- lookupLimits (Conf.subconfig "limits" cfg)
    spec <- lookupLimits (Conf.subconfig key cfg)
    return (spec <> def)


--
-- hashes and build system
--
instance Hashable UTCTime where
  hashWithSalt s t = hashWithSalt s (show t)

type TestHash = Int
type BuildCache
  = MVar (Map.Map String (MVar (Either TestHash TestHash, RWL.RWLock)))


-- | create a BuildCache
initBuildCache :: IO BuildCache
initBuildCache = newMVar Map.empty


-- | adds value to the problem hash
dependsOn :: Hashable a => a -> Tester ()
dependsOn value = Tester (lift $ modify (\x -> hashWithSalt x value))


-- | adds path, and file modification date to the problem hash
dependsOnFile :: String -> Tester ()
dependsOnFile path = do
  mt <- liftIO $ D.getModificationTime path
  dependsOn (path, mt)


-- | run a test, build if necessary
buildRun :: IO () -> IO a -> Tester a
buildRun buildProblem runTest = do
  tester <- metadata "tester" :: Tester (Maybe String)
  dependsOn ("tester" :: String, tester)
  buildCache <- Tester (asks _testBuildCache)
  hash <- Tester (lift get)
  path <- testFilePath
  liftIO $ buildRunIO buildCache path hash buildProblem runTest


buildRunIO :: BuildCache -> String -> TestHash -> IO () -> IO a -> IO a
buildRunIO buildCache path hash buildProblem runTest = do
  cache <- takeMVar buildCache -- lock cache table
  case Map.lookup path cache of
    Just mv -> do
      (buildStatus, rw) <- takeMVar mv -- lock curr prob
      putMVar buildCache cache -- release cache table
      case buildStatus of
        Right oldHash | oldHash==hash -> run mv rw -- no build
                      | otherwise     -> build mv rw -- rebuild
        Left oldHash  | oldHash==hash -> noRebuild mv rw
                      | otherwise     -> build mv rw -- rebuild
    Nothing -> do
      mv <- newEmptyMVar -- lock curr prob
      putMVar buildCache $ Map.insert path mv cache -- release cache table
      rw <- RWL.new
      build mv rw -- new build
  where
    build mv rw = do
      RWL.acquireWrite rw -- wait until can build
      buildProblem
      RWL.releaseWrite rw -- build done
      run mv rw -- build success
      `onException` do
        RWL.releaseWrite rw -- build done
        putMVar mv (Left hash, rw) -- build fail
    run mv rw = do
      RWL.acquireRead rw -- block building
      putMVar mv (Right hash, rw) -- release curr prob
      runTest `finally` RWL.releaseRead rw
    noRebuild mv rw = do
      putMVar mv (Left hash, rw) -- release curr prob
      throwIO $ AssertionFailed "Previous build attempt failed."

