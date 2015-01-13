{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
--import           Prelude hiding (catch)
import           Control.Monad.CatchIO (catch)
import           Control.Monad.State
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens

import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
--import qualified Data.ByteString as B
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
--import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format

import           Data.Configurator
import           Data.Configurator.Types

import           System.Locale

import           System.Remote.Monitoring

------------------------------------------------------------------------------
import           Application
import           Db
import           Utils
import           Types
import qualified Interval as Interval
import           Problem
import           Submission
import           LdapAuth
import           Printout

import           Paths_pythondo(version)
import           Data.Version (showVersion)  

-- my application splices
type AppSplices = Splices (I.Splice AppHandler)



------------------------------------------------------------------------------
-- | Handle login requests 
handleLogin :: AppHandler ()
handleLogin 
  = method GET (with auth $ handleLoginForm Nothing) <|>
    method POST (do { user <- getRequiredParam "login"
                    ; passwd <- getRequiredParam "password"
                    ; ldapconf <- getLdapConf
                    ; with auth $ handleLoginSubmit ldapconf user passwd
                    })


-- | Render login form
handleLoginForm :: Maybe Text -> Handler App (AuthManager App) ()
handleLoginForm authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = "loginError" ## maybe (return []) I.textSplice authError


------------------------------------------------------------------------------
{-
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLoginForm err) (redirect "/problems")
  where
    err = Just "Incorrect user or password"

-}

-- | Handle login submit using LDAP authentication
handleLoginSubmit :: 
  LdapConf -> ByteString -> ByteString -> Handler App (AuthManager App) ()
handleLoginSubmit ldapConf user passwd = do
  optAuth <- withBackend (\r -> liftIO $ ldapAuth r ldapConf user passwd)
  -- optAuth <- withBackend (\r -> liftIO $ dummyAuth r ldapConf user passwd)
  case optAuth of 
    Nothing -> handleLoginForm err
    Just u -> forceLogin u >> redirect "/problems"
  where 
    err = Just "Utilizador ou password incorreto"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: AppHandler ()
handleLogout = method GET $ do 
    uid <- getLoggedUser   --  ensure user is logged in   
    -- procedeed to printout in exam mode
    exam <- getConfigured "exam" False
    when exam $ handlePrintout uid
    with auth logout 
    redirect "/" 


{-
------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "register"
    handleFormSubmit = do
      login <- reqParam "login" 
      b <- usernameExists (T.pack $ B.toString login)
      if b then render "user_exists"
        else 
        registerUser "login" "password" >> redirect "/"
-}

-----------------------------------------------------------------------------
-- | problem description request 
handleProblem :: AppHandler ()
handleProblem = method GET $ do
  uid <- getLoggedUser -- ensure a user is logged in
  pid <- PID <$> getRequiredParam "pid"
  prob <- liftIO $ getProblem pid
  now <- liftIO getCurrentTime
  subs <- getSubmissions uid pid
  exam <- getConfigured "exam" False
  -- if running in exam mode, check that the problem is available 
  when (exam && not (now `Interval.elem` probOpen prob) && null subs)
       badRequest
  -- now list all submissions
  renderWithSplices "problem" $ do problemSplices prob 
                                   submissionsSplice subs



-- | problem listing handler
handleProblemList :: AppHandler ()
handleProblemList = method GET $ do
  uid <- getLoggedUser
  exam <- getConfigured "exam" False
  now <- liftIO getCurrentTime  
  allprobs<- liftIO getProblems
  -- summary counts for all previous submissions 
  summary <- getSubmissionsSummary uid 
  -- filter available problems and add dynamic tags (solved, unsolved, etc.)
  let allprobs' = map (dynamicTags summary) $ 
                  if exam then 
                  filter (\p -> now `Interval.elem` probOpen p ||
                            probID p `Map.member` summary) allprobs
                  else allprobs

  tags <- getQueryTags
  --  filter problems by query 
  let probs = filter (isTagged tags) allprobs'
  
  -- collect all tags (including dynamics)
  let alltags = Set.toList $ Set.unions $ map (Set.fromList . probTags) allprobs'
  
  -- convert submissions summary into list
  let list = [(p, r) | p<-probs,
              let r = Map.findWithDefault (0,0) (probID p) summary]
  
  -- context-dependent splices for each tag
  let tagSplices tag = 
        let checked = tag`elem`tags
            disabled = null (filter (isTagged [tag]) probs)
        in do "tagText" ## I.textSplice tag
              "tagCheckbox" ## return (checkboxInput tag checked disabled)
  
  -- render problem list page
  renderWithSplices "problemlist" $ do
    "tagList" ## I.mapSplices (I.runChildrenWith . tagSplices) alltags
    "problemList" ##  I.mapSplices (I.runChildrenWith . splices) list
    "totalProblems" ## I.textSplice (T.pack $ show (length allprobs))
    "visibleProblems" ## I.textSplice (T.pack $ show (length probs))
  where splices (prob,(nsub,nacc))
          = do problemSplices prob 
               counterSplices nsub
               "ifAccepted" ## conditionalSplice (nacc>0) 
               
               
-- fetch tag list from query string
getQueryTags :: AppHandler [Text]
getQueryTags = fmap (map (T.pack . B.toString) . 
                     Map.findWithDefault [] "tag") getParams

-- add dynamic tags for a problem
dynamicTags :: Map PID (Int,Int) -> Problem UTCTime -> Problem UTCTime
dynamicTags summary prob = prob { probTags = tags ++ probTags prob }
  where (sub,acc) = Map.findWithDefault (0,0) (probID prob) summary
        tags = [if acc>0 then "*accepted*" else "*not accepted*",
                if sub>0 then "*submitted*" else "*not submitted*"] 
               


problemSplices :: Problem UTCTime -> AppSplices
problemSplices p = do
  "problemID" ## I.textSplice (T.pack $ show $ probID p)
  "problemTitle" ## I.textSplice (probTitle p)
  "description" ## return (probDescr p)
  "submitText" ## I.textSplice (probSubmit p)
  "tags" ## I.textSplice (T.unwords $ probTags p)
  "startTime" ##  maybe (return []) timeSplice (Interval.start $ probOpen p)
  "endTime" ##  maybe (return []) timeSplice (Interval.end $ probOpen p)
  "ifOpen" ## do now <- liftIO getCurrentTime
                 conditionalSplice (isOpen now p)
  "ifEarly" ## do now <- liftIO getCurrentTime
                  conditionalSplice (isEarly now p)
  "ifLate" ## do now <- liftIO getCurrentTime
                 conditionalSplice (isLate now p)
  "ifLimited" ## conditionalSplice (Interval.limited $ probOpen p)
  "timeLeft"  ## case Interval.end (probOpen p) of 
    Nothing -> I.textSplice "N/A"
    Just t' -> do now <- liftIO getCurrentTime
                  I.textSplice $ T.pack $ formatNominalDiffTime $ diffUTCTime t' now

    
-- convert UTC time to local time zone
timeSplice :: UTCTime -> I.Splice AppHandler
timeSplice t = do tz <- liftIO getCurrentTimeZone
                  I.textSplice $ T.pack $ 
                    formatTime defaultTimeLocale "%c" $ 
                    utcToZonedTime tz t
    

-- format a time difference
formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime secs 
  | secs>=0  = unwords $ 
               [show d ++ "d" | d>0] ++ 
               [show (h`rem`24) ++ "h" | h>0] ++
               [show (m`rem`60) ++ "m" | m>0] ++
               ["<1m" | secs<60]
  | otherwise = "--/--"
  where m = (floor (secs / 60)) :: Int
        h = (m `div` 60) 
        d = (h `div` 24)  
        
-- splices relating to a single submission
submissionSplices :: Submission -> AppSplices
submissionSplices s = do 
  "submitID"   ## I.textSplice (T.pack $ show $ submitID s)
  "submitPID"  ## I.textSplice (T.pack $ show $ submitPID s)
  "submitTime" ## timeSplice (submitTime s)
  "submitText" ## I.textSplice (submitText s)
  "submitStatus" ## I.textSplice (T.pack $ show r)
  "submitReport" ## I.textSplice (submitReport s)
  "ifAccepted" ## conditionalSplice (r == Accepted)
  "ifOverdue" ##  conditionalSplice (r == Overdue)
  "ifRejected" ## conditionalSplice (r/= Accepted && r/=Overdue)
  where r = submitStatus s

-- splices relating to a list of submissions
submissionsSplice :: [Submission] -> AppSplices
submissionsSplice lst = do
  "submissions" ## I.mapSplices (I.runChildrenWith . submissionSplices) lst
  "ifAccepted" ## conditionalSplice (any isAccepted lst)
  counterSplices (length lst)


-- splices concerning submissions count
counterSplices :: Int -> AppSplices
counterSplices n = do 
  "count" ## I.textSplice (T.pack $ show n)
  "ifSubmitted" ## conditionalSplice (n>0)




handleGetSubmission, handlePostSubmission :: AppHandler ()
handleGetSubmission = method GET $ do
  uid <- getLoggedUser
  pid <- PID <$> getRequiredParam "pid"
  sid <- read . B.toString <$> getRequiredParam "sid"
  prob <- liftIO $ getProblem pid
  sub <- getSubmission uid pid sid
  renderWithSplices "report" $ do problemSplices prob  
                                  submissionSplices sub 


handlePostSubmission = method POST $ do
  uid <- getLoggedUser
  pid <-  PID <$> getRequiredParam "pid"
  prob <- liftIO $ getProblem pid
  -- if running in exam mode, check that the problem is available for submission
  pids <- getSubmittedPIDs uid
  now <- liftIO getCurrentTime
  exam <- getConfigured "exam" False
  when (exam && not (now `Interval.elem` probOpen prob) && pid `notElem` pids) 
       badRequest
  incrCounter "submissions"
  code <- T.decodeUtf8With T.ignore <$> getRequiredParam "code"
  sub <- postSubmission uid prob code
  renderWithSplices "report" $ do problemSplices prob 
                                  submissionSplices sub 


{-
-- handler for listing all submissions 
-- quick and dirty output routine 
handleSubmissions :: Handler App App ()
handleSubmissions = method GET $ do
  getUser    -- should be authenticated
  stats <- liftIO getAllStatus
  names <- with auth $ withBackend $ \r -> liftIO $ 
    sequence [lookup r txt | uid<-Map.keys stats, let txt=T.pack $ show uid]
  writeBS "UserID\tName\tAccepted\tSubmissions\n"
  sequence_ [ writeBS $ B.fromString $ 
              printf "%s\t%s\t%d\t%d\n" (show uid) (show name) accepts submits
            | ((uid, probs), name) <- zip (Map.assocs stats) names,
              let accepts = Map.size $ Map.filter (any (==Accepted)) probs,
              let submits = sum (Map.elems $ Map.map length probs)
            ]
  where 
    lookup r txt = do m<-lookupByLogin r txt
                      case m of Nothing -> return txt
                                Just au -> return (userName au)
-}

    


-- in exam mode show final report and confirm logout
-- otherwise, logout immediately
handleConfirmLogout :: AppHandler ()
handleConfirmLogout = do 
  exam <- getConfigured "exam" False
  if exam then handleFinalReport else handleLogout


handleFinalReport :: AppHandler ()
handleFinalReport = method GET $ do
    uid <- getLoggedUser  
    pids <- getSubmittedPIDs uid
    subs <- mapM (getBestSubmission uid) pids
    probs <- liftIO $ mapM getProblem pids
    let psubs = [(p,s) | (p, Just s)<-zip probs subs]
    renderWithSplices "finalrep" $
      "problemList" ## I.mapSplices (I.runChildrenWith . splices) psubs
  where splices (prob,sub) = do problemSplices prob 
                                submissionSplices sub




------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/login",                 handleLogin `catch` internalError)
         , ("/logout",                handleLogout `catch` internalError)
         -- , ("/register", with auth handleNewUser)
         , ("/problems/:pid",         handleProblem `catch` internalError )
         , ("/problems",              handleProblemList `catch` internalError)
         -- , ("/submissions",        handleSubmissions `catch` internalError)
         , ("/submissions/:pid",      handlePostSubmission `catch` internalError)
         , ("/submissions/:pid/:sid", handleGetSubmission  `catch` internalError)
         , ("/asklogout",             handleConfirmLogout `catch` internalError)        
         , ("",                       serveDirectory "static" <|> notFound)
         ]


-- | current logged in full user name  
loggedInName :: SnapletLens b (AuthManager b) -> SnapletISplice b
loggedInName authmgr = do
    u <- lift $ withTop authmgr currentUser
    maybe (return []) (I.textSplice . userName) u 


-- | splice for current date & time
nowSplice :: I.Splice AppHandler
nowSplice = do t <- liftIO (getCurrentTime >>= utcToLocalZonedTime)
               I.textSplice (T.pack $ formatTime defaultTimeLocale "%c" t)



versionSplice :: I.Splice AppHandler
versionSplice = I.textSplice (T.pack (showVersion version))

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = 
  makeSnaplet "pythondo" "Web system for learning Python programming." Nothing $ do
    conf <- getSnapletUserConfig
    ekg <- liftIO $ initEkg conf
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 7200)

    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    addConfig h emptyConfig { hcInterpretedSplices = do 
                                 "version" ## versionSplice
                                 "timeNow" ## nowSplice
                                 "loggedInName" ## loggedInName auth }
      
      
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> do Db.createTables conn
                                      putStrLn "Updating problem tags db table"
                                      Db.updateProblems conn =<< getProblems 

    addRoutes routes
    return $ App h s d a conf ekg 


emptyConfig :: HeistConfig m
emptyConfig = HeistConfig noSplices noSplices noSplices noSplices []


-- initialize EKG server (if configured)
initEkg :: Config -> IO (Maybe Server)
initEkg conf = do enabled <- require conf "ekg.enabled"
                  if enabled then 
                    do host <- require conf "ekg.host"
                       port <- require conf "ekg.port"
                       Just <$> forkServer host port                   
                    else return Nothing
  


