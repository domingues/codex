{-# LANGUAGE OverloadedStrings #-}
{-
  Some helper handlers for our application monad
-}
module Utils where

import           Control.Concurrent.MVar
import           Control.Monad.State
import           Data.Text
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

-- import           Data.Text (Text)
import qualified Data.Text as T

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
-- import Snap.Snaplet.Session

import           Heist.SpliceAPI
import qualified Heist.Interpreted as I


import Data.Configurator
import Data.Configurator.Types


import Control.Exception (SomeException)

-- import System.Process
-- import System.Exit (ExitCode)
import System.Remote.Monitoring
import System.Remote.Counter as Counter

import Paths_pythondo(version)

import Application
import Types
import LdapAuth



-- | run an IO action under mutual exclusion from other threads
liftMutexIO :: IO a -> AppHandler a
liftMutexIO action 
  = do mvar<-gets appLock; liftIO $ withMVar mvar $ \_ -> action

{-
-- | run a command under safeexec sandbox
-- this is not really used for now
runSafeExec :: FilePath      -- filename of executable
               -> [String]   -- arguments
               -> String     -- stdin
               -> AppHandler (ExitCode,String,String) -- exitcode, stdout, stderr
runSafeExec cmd args stdin 
  = do safeexec <- getSafeExec 
       let args' = ["--cpu", show (maxCpuTime safeexec),
                    "--clock", show (maxClockTime safeexec),
                    "--mem", show (maxMemory safeexec)] 
       liftIO $ readProcessWithExitCode (safeExec safeexec) (args'++cmd:args) stdin
-}

getConfigured :: Configured a => Name -> a -> AppHandler a
getConfigured key def = do 
  conf <- gets config
  liftIO $ lookupDefault def conf key 



-- | get current SafeExec parameters from configurator
getSafeExec :: AppHandler SafeExec
getSafeExec = do 
  conf<-gets config
  liftIO $ getSafeExec' conf

getSafeExec' :: Config -> IO SafeExec
getSafeExec' conf = do 
  python <- require conf "submissions.python"
  safeexec<-require conf "submissions.safeexec"
  cpu <- require conf "submissions.max_cpu"
  clock <- require conf "submissions.max_clock"
  mem <- require conf "submissions.max_memory"
  return SafeExec { safeExec = safeexec
                  , pythonExec= python
                  , maxCpuTime = cpu 
                  , maxClockTime = clock
                  , maxMemory = mem 
                  }


-- | get LDAP configuration parameters
getLdapConf :: AppHandler LdapConf
getLdapConf = do 
  conf <- gets config
  liftIO $ do host <- require conf "ldap.host"
              bases <- require conf "ldap.bases"
              return (LdapConf host bases)

-- | get printout configuration parameters
getPrintout :: AppHandler Printout              
getPrintout = do
  conf <- gets config
  liftIO $ do enabled <- lookupDefault False conf "printouts.enabled"
              header <- lookupDefault defaultHeader conf "printouts.header"
              opts <- lookupDefault [] conf "printouts.options"
              return (Printout enabled header opts)
  where
    defaultHeader = T.pack ("Pythondo "++show version)

-- | increment an EKG counter
incrCounter :: Text -> AppHandler ()
incrCounter name 
  = gets ekgServer >>= 
    maybe (return ())  (\ekg -> liftIO $ getCounter name ekg >>= Counter.inc) 


-- | Get current logged in user; fail with unauthorized error if missing
getUser :: AppHandler UID
getUser = do 
  mb <- with auth currentUser
  case mb of
    Nothing -> unauthorized
    Just au -> return (UID . B.fromString $ T.unpack $ userLogin au)


getFullName :: AppHandler Text
getFullName = do 
  mb <- with auth currentUser
  case mb of 
    Nothing -> unauthorized
    Just au -> return (userName au)



-- | Get a required parameter; fails if missing
getRequiredParam :: ByteString -> AppHandler ByteString    
getRequiredParam p = do 
  mv <- getParam p
  case mv of 
    Nothing -> badRequest
    Just v -> return v



-- | error handlers
unauthorized, badRequest, notFound :: AppHandler a
unauthorized = render "_unauthorized" >> finishError 401 "Unauthorized"
badRequest = render "_badrequest" >> finishError 400 "Bad request"
notFound = render "_notfound" >> finishError 404 "Not found"


internalError :: SomeException -> AppHandler a
internalError e
  = do renderWithSplices  "_internalerror" ("errorMsg" ## I.textSplice (T.pack $ show e))
       finishError 500 "Internal Server Error"


-- | finish with an http error
finishError :: MonadSnap m => Int -> ByteString -> m a
finishError code msg = do
  modifyResponse (setResponseStatus code msg)
  r <- getResponse
  finishWith r
  
