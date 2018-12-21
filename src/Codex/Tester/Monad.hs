{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Monad (
  Tester,
  runTester,
  configured,
  maybeConfigured,
  testLimits,
  testConfig,
  testPath,
  testCode,
  testPage,
  testMetadata,
  testUser,
  metadata,
  BuildCache,
  initBuildCache,
  buildRun,
  dependsOn,
  dependsOnFile,
  ) where



import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Data.Monoid
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

import           Codex.Types (Code, UserLogin)
import           Text.Pandoc (Meta)
import           Codex.Page
import           Codex.Tester.Limits

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

-- | testing environment
data TestEnv
   = TestEnv { _testConfig :: Config   -- ^ static configuration file
             , _testPage :: Page       -- ^ exercise page
             , _testPath :: FilePath   -- ^ file path to exercise page
             , _testCode :: Code       -- ^ submited language & code
             , _testUser :: UserLogin  -- ^ user
             , _testBuildCache :: BuildCache
             } 


-- | run function for testers
runTester ::
  Config -> BuildCache -> Page -> FilePath -> Code -> UserLogin -> Tester a
  -> IO (Maybe a)
runTester cfg cache page path code user action
  = runMaybeT $ evalStateT (runReaderT (unTester action) (TestEnv cfg page path code user cache)) 0


-- | fetch parameters from enviroment
testConfig :: Tester Config
testConfig = Tester (asks _testConfig)

testPath :: Tester FilePath
testPath = Tester (asks _testPath)

testCode :: Tester Code
testCode = Tester (asks _testCode)

testPage :: Tester Page
testPage = Tester (asks _testPage)

testMetadata :: Tester Meta
testMetadata = pageMeta <$> testPage

testUser :: Tester UserLogin
testUser = Tester (asks _testUser)


-- | fetch a metadata value; return Nothing if key not present
metadata :: FromMetaValue a => String -> Tester (Maybe a)
metadata key = do
  meta <- testMetadata
  return (lookupFromMeta key meta)


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


-- | get configured limits from the tester environment
-- overrides default config with the specific one
testLimits :: Name -> Tester Limits
testLimits key = do
  cfg <- testConfig
  liftIO $ do
    def  <- configLimits (Conf.subconfig "limits" cfg)
    spec <- configLimits (Conf.subconfig key cfg)
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
  path <- testPath
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

