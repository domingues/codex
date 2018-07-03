{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Tester.Monad (
  Tester,
  runTester,
  configured,
  maybeConfigured,
  testLimits,
  testConfig,
  testDb,
  testPath,
  testCode,
  testMetadata,
  ) where



import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Data.Monoid
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe 
import           Control.Monad.Trans.Reader

import           Codex.Types (Code)
import           Text.Pandoc (Meta)
import           Codex.Tester.Limits
import           Snap.Snaplet.SqliteSimple(Sqlite)


-- | a monad for testing scripts
-- allows access to a test environment, IO and failure (i.e. passing)
newtype Tester a
  = Tester { unTester :: ReaderT TestEnv (MaybeT IO) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

-- | testing environment
data TestEnv
   = TestEnv { _testConfig :: Config   -- ^ static configuration file
             , _testDb   :: Sqlite   -- ^ codex database
             , _testMeta :: Meta       -- ^ exercise metadata
             , _testPath :: FilePath   -- ^ file path to exercise page
             , _testCode :: Code       -- ^ submited language & code 
             } 


-- | run a tester
runTester :: Config -> Sqlite -> Meta -> FilePath -> Code -> Tester a
          -> IO (Maybe a)
runTester cfg db meta path code action
  = runMaybeT $ runReaderT (unTester action) (TestEnv cfg db meta path code)


-- | fetch paramaters from the enviroment
testConfig :: Tester Config
testConfig = Tester (asks _testConfig)

testDb :: Tester Sqlite
testDb = Tester (asks _testDb)

testPath :: Tester FilePath
testPath = Tester (asks _testPath)

testCode :: Tester Code
testCode = Tester (asks _testCode)

testMetadata :: Tester Meta
testMetadata = Tester (asks _testMeta)


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
                  




