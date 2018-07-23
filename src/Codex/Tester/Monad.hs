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
  testMetadata,
  metadata,
  testHash,
  dependsMetadata,
  dependsMetadataFile,
  dependsMaybeConfigured,
  BuildCache,
  TestHash,
  initBuildCache,
  testBuildCache,
  ) where


import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Data.Hashable
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Control.Applicative
import           Control.Concurrent.MVar
import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State

import           Codex.Types (Code)
import           Text.Pandoc (Meta)
import           Codex.Page
import           Codex.Tester.Limits
import qualified System.Directory as D
import           Data.Time.Clock(UTCTime)


type TestHash = Int
type CacheMap = Map.Map String (MVar (Either TestHash TestHash, RWL.RWLock))
type BuildCache = MVar CacheMap

-- | a monad for testing scripts
-- allows access to a test environment, IO and failure (i.e. passing)
newtype Tester a
  = Tester { unTester :: ReaderT TestEnv (StateT TestHash (MaybeT IO)) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

-- | testing environment
data TestEnv
   = TestEnv { _testConfig :: Config   -- ^ static configuration file
             , _testMeta :: Meta       -- ^ exercise metadata
             , _testPath :: FilePath   -- ^ file path to exercise page
             , _testCode :: Code       -- ^ submited language & code
             , _testBuildCache :: BuildCache
             }

instance Hashable UTCTime where
  hashWithSalt s t = hashWithSalt s (show t)


-- | run a tester
runTester :: Config -> BuildCache -> Meta -> FilePath -> Code -> Tester a
          -> IO (Maybe a)
runTester cfg cache meta path code action
  = runMaybeT $ evalStateT (runReaderT (unTester action) (TestEnv cfg meta path code cache)) 0

initBuildCache :: IO BuildCache
initBuildCache = newMVar Map.empty

addToHash :: Hashable a => a -> Tester ()
addToHash v = Tester (lift $ modify (\x -> hashWithSalt x v))


-- | fetch paramaters from the enviroment
testConfig :: Tester Config
testConfig = Tester (asks _testConfig)

testPath :: Tester FilePath
testPath = Tester (asks _testPath)

testCode :: Tester Code
testCode = Tester (asks _testCode)

testMetadata :: Tester Meta
testMetadata = Tester (asks _testMeta)

testBuildCache :: Tester BuildCache
testBuildCache = Tester (asks _testBuildCache)


-- | returns the test current hash
-- it is calculated from all the metadata and configurations fetched
testHash :: Tester TestHash
testHash = Tester (lift get)


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


-- | build depends on metadata key-value
-- adds it key and value to the problem hash
dependsMetadata :: (Hashable a, FromMetaValue a) => String -> Tester (Maybe a)
dependsMetadata key = do
  value <- metadata key
  case value of
    Nothing -> return Nothing
    Just v -> do
      addToHash (key, v)
      return (Just v)


-- | build depends on metadata key-value-file
-- adds it key, value, and file modification date to the problem hash
dependsMetadataFile :: String -> Tester (Maybe FilePath)
dependsMetadataFile key = do
  value <- metadata key
  case value of
    Nothing -> return Nothing
    Just v -> do
      mt <- liftIO $ D.getModificationTime v
      addToHash (key, v, mt)
      return (Just v)


-- | build depends on config key-value
-- adds it key and value to the problem hash
dependsMaybeConfigured :: (Hashable a, Configured a) => Name -> Tester (Maybe a)
dependsMaybeConfigured key = do
  value <- maybeConfigured key
  case value of
    Nothing -> return Nothing
    Just v -> do
      addToHash (key, v)
      return (Just v)


-- | get configured limits from the tester environment
-- overrides default config with the specific one
testLimits :: Name -> Tester Limits
testLimits key = do
  cfg <- testConfig
  liftIO $ do
    def  <- configLimits (Conf.subconfig "limits" cfg)
    spec <- configLimits (Conf.subconfig key cfg)
    return (spec <> def)





