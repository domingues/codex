{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-
  Some helper handlers for our application monad
-}
module Codex.Utils where

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Char(toUpper)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import qualified Data.ByteString.Lazy as LB

import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Aeson as Aeson
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Map.Syntax

import           Snap.Core hiding (path)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Router

import           Heist
import qualified Heist.Splices as I
import qualified Heist.Interpreted as I

import qualified Text.XmlHtml as X


import           Control.Monad (join)
import           Control.Monad.State
import           Control.Applicative 
import           Control.Exception (SomeException(..))

import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator

import           Codex.Types
import           Codex.Page
import           Codex.Policy
import           Codex.Application

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format hiding (parseTime)

import           System.FilePath
import qualified System.FastLogger as FastLogger
import           System.Directory (doesFileExist, doesDirectoryExist)

-- interpreted splices for handlers
type ISplices = Splices (I.Splice Codex)

-- | fetch document root directory from config file
getDocumentRoot :: Codex FilePath
getDocumentRoot = do
  conf <- getSnapletUserConfig
  liftIO (Configurator.require conf "system.document_root")

getStaticRoot :: Codex FilePath
getStaticRoot = do
  conf <- getSnapletUserConfig
  liftIO (Configurator.require conf "system.static_root")


-- | lookup full name for a user login in Db
queryFullname :: UserLogin -> Codex (Maybe Text)
queryFullname uid = do
  -- this hugly hack is needed because the Sqlite backend inserts NULL
  -- fields for empty metadata; maybe we could handle this better?
  Only check:_ <- query "SELECT meta_json is NULL from snap_auth_user WHERE login = ?" (Only uid) 
  if check  then
    return Nothing
    else do
    list <- query "SELECT meta_json FROM snap_auth_user WHERE login = ?" (Only uid)
    return $ case list of
      (txt:_) -> do
        hm <- Aeson.decodeStrict (T.encodeUtf8 txt) :: Maybe (HM.HashMap Text Text)
        HM.lookup "fullname" hm
      _ -> Nothing


-- | Get current logged in user ID (if any)
--   from the authentication snaplet
getUserLogin :: Codex (Maybe UserLogin)
getUserLogin = do
  mAu <- with auth currentUser
  return (fmap authUserLogin mAu)

getFullname :: Codex (Maybe Text)
getFullname = do
  mAu <- with auth currentUser
  return (mAu >>=  authFullname)


authUserLogin :: AuthUser -> UserLogin
authUserLogin = UserLogin . userLogin

authFullname :: AuthUser -> Maybe Text
authFullname au
  = case HM.lookup "fullname" (userMeta au) of
    Just (Aeson.String name) -> Just name
    _                  -> Nothing


isAdmin :: AuthUser -> Bool
isAdmin au = Role "admin" `elem` userRoles au

-- | ensure that the logged user has administrative privileges
requireAdmin :: Codex () 
requireAdmin = do
  usr <- require (with auth currentUser) <|> unauthorized
  unless (isAdmin usr) unauthorized


-- | get all events from the configuration 
----------------------------------------------------------------
getTimeEnv :: Codex TimeEnv
getTimeEnv = do
  evcfg <- gets _eventcfg
  hm <- liftIO $ Configurator.getMap evcfg
  let hm' = HM.map (\v -> Configurator.convert v >>= parseTimeExpr) hm
  tz <- liftIO $ getCurrentTimeZone
  return $ makeTimeEnv tz (\k -> join (HM.lookup k hm'))


-- | get submission id from request parameters
getSubmitId :: Codex (Maybe SubmitId)
getSubmitId = fmap SubmitId <$> readParam "sid"


-- |  use Read instance to decode an HTTP parameter 
readParam :: (Read a, MonadSnap m) => ByteString -> m (Maybe a)
readParam name = do
  opt <- getParam name
  return $ do bs <- opt
              case reads (B.toString bs) of
                [(x, "")] -> Just x
                _   -> Nothing


-- | try a Maybe-handler and "pass" if it yields Nothing
require :: Handler m m' (Maybe a) -> Handler m m' a
require handler = do
  opt <- handler
  case opt of
    Nothing -> pass
    Just v -> return v


-- | get a text parameter from a POST request
getTextParam :: MonadSnap m => ByteString -> m (Maybe Text)
getTextParam name =
  do opt <- getPostParam name
     return ((T.filter (/='\r') . T.decodeUtf8With T.ignore) <$> opt)


-- | get a parameter with a default value
getParamDef :: MonadSnap m => ByteString -> ByteString -> m ByteString
getParamDef name def = fromMaybe def <$> getParam name


guardFileExists  :: (MonadIO m, Alternative m) => FilePath -> m ()
guardFileExists f = guard =<< liftIO (doesFileExist f) 

guardDirectoryExists  :: (MonadIO m, Alternative m) => FilePath -> m ()
guardDirectoryExists f = guard =<< liftIO (doesDirectoryExist f) 

---------------------------------------------------------------------
-- | error handlers
---------------------------------------------------------------------
unauthorized, badRequest, notFound :: Codex a
unauthorized = render "_unauthorized" >> finishError 401 "Unauthorized"
badRequest   = render "_badrequest" >> finishError 400 "Bad request"
notFound     = render "_notfound" >> finishError 404 "Not found"


internalError :: SomeException -> Codex a
internalError e
  = do renderWithSplices  "_internalerror"
             ("errorMsg" ## I.textSplice (T.pack $ show e))
       finishError 500 "Internal Server Error"


-- | finish with an http error
finishError :: MonadSnap m => Int -> ByteString -> m a
finishError code msg = do
  modifyResponse (setResponseStatus code msg)
  r <- getResponse
  finishWith r



-- | format an UTC time as a local time string
localTimeSplice :: Monad m => TimeZone -> UTCTime -> I.Splice m
localTimeSplice tz t =
  I.textSplice $ T.pack $
  formatTime defaultTimeLocale "%c" $ utcToLocalTime tz t


-----------------------------------------------------------------------------

-- | splices related to a page
pageSplices  :: Monad m => Page -> Splices (I.Splice m)
pageSplices page = do
  "page-description" ## return (pageToHtml page)
  "page-title" ## return (blocksToHtml $ pageTitleBlocks page)


pageUrlSplices :: FilePath -> ISplices
pageUrlSplices rqpath = do
  let paths = splitDirectories rqpath
  let paths' = parent paths
  "page-url" ## urlSplice (Page paths)
  "page-parent-url" ## urlSplice (Page paths')
  "if-parent" ## I.ifElseISplice (paths' /= paths)

parent :: [FilePath] -> [FilePath]
parent [] = []
parent fs
  | last fs == "index.md" = drop2 fs ++ ["index.md"]
  | otherwise             = init fs ++ ["index.md"]
  where drop2 = reverse . drop 2 . reverse

fileUrlSplices :: FilePath -> ISplices
fileUrlSplices rqpath = do
  let path = splitDirectories rqpath
  let parent = if null path then [] else init path
  "file-path" ## I.textSplice (T.pack rqpath)
  "file-url" ## urlSplice (Files path)
  "file-parent-url" ## urlSplice (Files parent)

urlSplices :: FilePath -> ISplices
urlSplices rqpath = pageUrlSplices rqpath >> fileUrlSplices rqpath
  




-- list of messages
messageSplices :: Monad m => [Text] -> Splices (I.Splice m)
messageSplices mesgs = do
  "message-list" ## I.mapSplices (I.runChildrenWith . splice) mesgs
  where splice msg = "message" ## I.textSplice msg


tagCaseSplice :: Monad m => Text -> I.Splice m
tagCaseSplice tag = getParamNode >>= (I.runNodeList . select . X.childNodes)
   where
     select nodes = case filter (\n -> X.tagName n==Just tag ||
                                       X.tagName n==Just "default") nodes of
       [] -> []
       (n:_) -> X.childNodes n

caseSplice :: (Monad m, Show a) => a -> I.Splice m
caseSplice v = tagCaseSplice (T.pack $ show v)



--------------------------------------------------------------

checkboxInput :: [(Text,Text)] -> [X.Node] -> X.Node
checkboxInput attrs contents = X.Element "input" attrs' contents
  where attrs' = ("type","checkbox") : attrs


jsTimer :: String -> NominalDiffTime -> [X.Node]
jsTimer id secs
  = [X.Element "span" [("id",T.pack id),
                       ("class", "js-timer")] [],
     javascript $ T.pack $
     "start_countdown(" ++ show id ++ "," ++ show (floor secs :: Int) ++ ");"]

javascript :: Text -> X.Node
javascript txt
  = X.Element "script" [("type","text/javascript")] [X.TextNode txt]

javascriptSrc :: Text -> X.Node
javascriptSrc  url
  = X.Element "script" [("type","text/javascript"), ("src",url)] []

-------------------------------------------------------------------------------

-- | Wrap a handler with method override support. This means that if
-- (and only if) the request is a POST, _method param is passed, and
-- it is a parsable method name, it will change the request method to
-- the supplied one. This works around some browser limitations.
-- Adapted from Snap.Extras.MethodOverride
handleMethodOverride :: MonadSnap m =>
                        m a
                      -- ^ Internal handler to call
                      -> m a
handleMethodOverride = (modifyRequest (methodOverride "_method") >>)


-------------------------------------------------------------------------------
methodOverride :: ByteString -> Request -> Request
methodOverride param r
  | rqMethod r == POST = r { rqMethod = overridden }
  | otherwise          = r
  where
    overridden = fromMaybe POST $ do
      meth <- listToMaybe =<< rqParam param r
      case map toUpper (B.toString meth) of
       "HEAD"    -> Just HEAD
       "POST"    -> Just POST
       "PUT"     -> Just PUT
       "DELETE"  -> Just DELETE
       "TRACE"   -> Just TRACE
       "OPTIONS" -> Just OPTIONS
       "CONNECT" -> Just CONNECT
       "PATCH"   -> Just PATCH
       ""        -> Nothing
       _         -> Just (Method meth)



-- | log an authentication message
logMsg :: ByteString -> Codex ()
logMsg msg = do
  logger <- gets _logger
  liftIO $ FastLogger.logMsg logger =<< FastLogger.timestampedLogEntry msg

{-
withTimeSplices :: Page -> Codex a -> Codex a
withTimeSplices page action = do
  tz  <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  env <- getTimeEnv
  flip withSplices action $ 
    case evalPolicy env (pageValid page) of
      Left err -> do
        "valid-from" ## I.textSplice err
        "valid-until" ## I.textSplice err
        "time-left" ## I.textSplice err        
      Right constr -> do
        "if-early" ## I.ifElseISplice (early now constr)
        "if-late" ##  I.ifElseISplice (late now constr)
        "if-valid" ## I.ifElseISplice (not (early now constr) &&
                                       not (late now constr))
        "if-limited" ## I.ifElseISplice (isJust $ higher constr)
        "if-max-attempts" ## I.ifElseISplice (isJust $ maxAttempts constr)
        "valid-from" ##
          I.textSplice $ maybe "N/A" (showTime tz) (lower constr)
        "valid-until" ##
          I.textSplice $ maybe "N/A" (showTime tz) (higher constr)
        "time-left" ##
          I.textSplice $
          maybe "N/A" (\t -> T.pack $ formatNominalDiffTime t)
          (timeLeft now constr)
-}


getPolicy :: Page -> Codex (Policy UTCTime)
getPolicy page = do
  env <- getTimeEnv
  case evalPolicy env =<< pagePolicy page of
    Left err -> internalError (SomeException $ userError $ T.unpack err)
    Right pol -> return pol

    
feedbackSplices :: Page -> ISplices
feedbackSplices page = do
  "if-feedback" ## I.ifElseISplice (pageFeedback page)


decodeText :: FromJSON a => Text -> Maybe a
decodeText = Aeson.decode . LB.fromStrict . T.encodeUtf8

encodeText :: ToJSON a  => a -> Text
encodeText = T.decodeUtf8 . LB.toStrict . Aeson.encode
