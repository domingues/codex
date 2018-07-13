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
  testDbConn,
  testPath,
  testCode,
  testMetadata,
  testHash,
  metadata,
  metadataFile,
  tester,
  ) where


import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Data.Hashable
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Concurrent.MVar
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State

import           Codex.Types (Code)
import           Text.Pandoc (Meta)
import           Codex.Page
import           Codex.Tester.Limits
import           Database.SQLite.Simple (Connection)
import qualified System.Directory as D
import           Data.Time.Clock(UTCTime)
import           System.FilePath


type Hash = Int

-- | a monad for testing scripts
-- allows access to a test environment, IO and failure (i.e. passing)
newtype Tester a
  = Tester { unTester :: ReaderT TestEnv (StateT Hash (MaybeT IO)) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

-- | testing environment
data TestEnv
   = TestEnv { _testConfig :: Config   -- ^ static configuration file
             , _testDbConn :: MVar Connection   -- ^ codex database
             , _testMeta :: Meta       -- ^ exercise metadata
             , _testPath :: FilePath   -- ^ file path to exercise page
             , _testCode :: Code       -- ^ submited language & code
             }

instance Hashable UTCTime where
  hashWithSalt s t = hashWithSalt s (show t)


-- | run a tester
runTester :: Config -> MVar Connection -> Meta -> FilePath -> Code -> Tester a
          -> IO (Maybe a)
runTester cfg dbConn meta path code action
  = runMaybeT $ evalStateT (runReaderT (unTester action) (TestEnv cfg dbConn meta path code)) (0::Hash)


addToHash :: Hashable a => a -> Tester ()
addToHash v = Tester (lift $ modify (\h -> hashWithSalt h v))


-- | fetch paramaters from the enviroment
testConfig :: Tester Config
testConfig = Tester (asks _testConfig)

testDbConn :: Tester (MVar Connection)
testDbConn = Tester (asks _testDbConn)

testPath :: Tester FilePath
testPath = Tester (asks _testPath)

testCode :: Tester Code
testCode = Tester (asks _testCode)

testMetadata :: Tester Meta
testMetadata = Tester (asks _testMeta)


-- | returns the test current hash
-- it is calculated from all the metadata and configurations fetched
testHash :: Tester Hash
testHash = Tester (lift get)


-- | fetch a metadata value; return Nothing if key not present
-- adds it key and value to the problem hash
metadata :: (Hashable a, FromMetaValue a) => String -> Tester (Maybe a)
metadata key = do
  meta <- testMetadata
  case lookupFromMeta key meta of
    Nothing -> return Nothing
    Just v -> do
      addToHash (key, v)
      return (Just v)


-- | fetch a metadata absolute file path; return Nothing if key not present
-- adds it key, value and file modification date to the problem hash
metadataFile :: String -> Tester (Maybe FilePath)
metadataFile key = do
  meta <- testMetadata
  case lookupFromMeta key meta of
    Nothing -> return Nothing
    Just v -> do
      tp <- testPath
      let path = takeDirectory tp </> v
      mt <- liftIO $ D.getModificationTime path
      addToHash (key, v, mt)
      return (Just path)


-- | fetch a configured value; return Nothing if key not present
-- adds it key and value to the problem hash
maybeConfigured :: (Hashable a, Configured a) => Name -> Tester (Maybe a)
maybeConfigured key = do
  cfg <- testConfig
  c <- liftIO $ Conf.lookup cfg key
  case c of
    Nothing -> return Nothing
    Just v -> do
      addToHash (key, v)
      return (Just v)


-- | fetch a configuration value
-- throws an exception if key is not present
-- adds it key and value to the problem hash
configured :: (Hashable a, Configured a) => Name -> Tester a
configured key = do
  cfg <- testConfig
  v <- liftIO $ Conf.require cfg key
  addToHash (key, v)
  return v


-- | get configured limits from the tester environment
-- overrides default config with the specific one
testLimits :: Name -> Tester Limits
testLimits key = do
  cfg <- testConfig
  liftIO $ do
    def  <- configLimits (Conf.subconfig "limits" cfg)
    spec <- configLimits (Conf.subconfig key cfg)
    return (spec <> def)


-- | label a tester and ignore submissions that don't match
tester :: String -> Tester a -> Tester a
tester name cont = do
  t <- metadata "tester"
  guard (t == Just name)
  cont
