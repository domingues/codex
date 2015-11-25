{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

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
import qualified Data.ByteString      as B
import           Data.Monoid
import           Data.Maybe(listToMaybe, isJust)
-- import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import           Heist.Splices
import qualified Heist.Interpreted as I

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format

import qualified Data.Configurator as Configurator
import           Data.Configurator.Types

import           System.Locale
import           System.FilePath
import           System.IO.Error
import           System.Remote.Monitoring
import qualified Text.XmlHtml as X


------------------------------------------------------------------------------
import           Application
import           Db
import           Utils
import           Types
import           Markdown
import           Text.Pandoc.Builder
-- import qualified Interval as Interval
-- import           Interval (Interval)
import           Problem
import           Submission
import           Summary
import           LdapAuth
-- import           Report
-- import           Printout

import           Paths_pythondo(version)
import           Data.Version (showVersion)  

import           AceEditor

-- interpreted splices for Pythondo handlers
type ISplices = Splices (I.Splice Pythondo)



------------------------------------------------------------------------------
-- | Handle login requests 
handleLogin :: Pythondo ()
handleLogin 
  = method GET (with auth $ handleLoginForm Nothing) <|>
    method POST (do { user <- require (getParam "login") 
                    ; passwd <- require (getParam "password")
                    ; ldapconf <- getLdapConf
                    ; with auth $ handleLoginSubmit ldapconf user passwd
                    } <|> badRequest)


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
  -- optAuth <- withBackend (\r -> liftIO $ ldapAuth r ldapConf user passwd)
  optAuth <- withBackend (\r -> liftIO $ dummyAuth r ldapConf user passwd)
  case optAuth of 
    Nothing -> handleLoginForm err
    Just u -> forceLogin u >> redirect "/problems"
  where 
    err = Just "Utilizador ou password incorreto"


------------------------------------------------------------------------------
-- Logs out and redirects the user to the site index.
-- in exam mode procedeed to printout 
handleLogout :: Pythondo ()
handleLogout = method GET $ do
  uid <- require getUserID <|> unauthorized
  with auth logout 
  redirect "/" 
  {-
   -- handle printout if needed
   ProblemSet{..} <- fst <$> getProblemSet
   when probsetPrintout $ handlePrintout uid probsetProbs
   -}

{-
-- make a printout before ending session
handlePrintout :: UID -> [Problem] -> Pythondo ()
handlePrintout uid probs = do
  subs <- mapM (getBestSubmission uid) (map probID probs)
  report <- genReport (zip probs subs)
  liftIO $ makePrintout (show uid) report
-}


{-
------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler Pythondo (AuthManager Pythondo) ()
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



-- | handle document requests
handleDocument :: Pythondo ()
handleDocument = do
  uid <- require getUserID <|> unauthorized    -- ensure user is logged in
  path <- B.toString <$> getsRequest rqPathInfo
  when (takeExtension path /= ".md") pass
  withSplices ("documentPath" ## I.textSplice (T.pack path))
    (method GET (handleGet uid path) <|>
     method POST (handlePost uid path))

--- get a worksheet  
handleGet uid path = do
  doc <- getWorksheet path
  optPid <- getProblemID 
  case optPid of
    Nothing -> handleWorksheet doc uid
    Just pid -> handleProblem doc uid pid



handleWorksheet doc uid = do
  doc' <- getSummary uid doc   -- get submissions summary for user
  now <- liftIO getCurrentTime
  renderWithSplices "worksheet" $ do
    warningsSplices []
    "worksheetItems" ## I.mapSplices (I.runChildrenWith . wsItemSplices now) (wsItems doc')
  

handleProblem doc@Worksheet{..} uid pid 
  = case lookupProblem pid doc of
    Nothing -> notFound
    Just prob -> do
      now <- liftIO $ getCurrentTime
      renderWithSplices "problem" $ do
        problemSplices prob
        inputAceEditorSplices
        timerSplices (probID prob) now (probLimit prob)
        submissionsSplice []
        warningsSplices []
    

lookupProblem :: PID -> Worksheet Problem -> Maybe Problem
lookupProblem pid Worksheet{..} 
  = listToMaybe [p | Right p <- wsItems, probID p == pid]

handlePost uid path = undefined



{-
-----------------------------------------------------------------------------
-- | problem description request 
handleProblem :: Pythondo ()
handleProblem = method GET $ do
    uid <- require getUserID  <|> unauthorized
    pid <- require getProblemID
    (prob,mesgs) <- getProblem pid
    now <- liftIO getCurrentTime
    subs <- getSubmissions uid pid
    renderWithSplices "problem" $
      do problemSplices prob
         inputAceEditorSplices
         submissionsSplice subs
         timerSplices pid now (probDeadline prob)
         warningsSplices mesgs
-}

{-
   
-- | problem set listing handler
handleProblemList :: Pythondo ()
handleProblemList = method GET $ do
  uid <- require getUserID <|> unauthorized
  now <- liftIO getCurrentTime
  (probset, mesgs) <- getProblemSet
  tags <- getQueryTags
  -- summary of all available problems
  available <- getProblemSummary uid probset 
  -- filter by query tags
  let visible = filter (hasTags tags) available
      
  -- context-dependent splices for tags 
  let tagSplices tag = 
        let checked = tag`elem`tags
            disabled= null (filter (isTagged tag) visible)
        in do "tagText" ## I.textSplice tag
              "tagCheckbox" ## return (checkboxInput tag checked disabled)

  -- render page
  renderWithSplices "problemlist" $ do
    warningsSplices mesgs
    "problemsetDescription" ## return (renderPandoc $ probsetDescr probset)
    "problemsetPath" ## I.textSplice (T.pack $ probsetPath probset)
    "tagList" ## I.mapSplices (I.runChildrenWith . tagSplices) (taglist available)
    "problemList" ##  I.mapSplices (I.runChildrenWith . summarySplices now) visible
    "availableProblems" ## I.textSplice (T.pack $ show $ length available)
    "visibleProblems" ## I.textSplice (T.pack $ show $ length visible)
               
-}

{-
-- get tag list from query string
getQueryTags :: Pythondo [Tag]
getQueryTags = do
  params <- getParams
  return (map (T.pack . B.toString) $ Map.findWithDefault [] "tag" params)
-}


warningsSplices :: [Text] -> ISplices
warningsSplices mesgs = do
  "warnings" ## I.mapSplices (I.runChildrenWith . mesgSplice) mesgs
  where mesgSplice msg = "message" ## I.textSplice msg



-- | splices related to a single problem       
problemSplices :: Problem -> ISplices
problemSplices Problem{..} = do
  "problemID" ## I.textSplice (T.pack $ show probID)
  "problemDoctest" ## maybe (return []) I.textSplice probDoctest
  "problemDefault" ## maybe (return []) I.textSplice probDefault
  "problemHeader" ## return (blocksToHtml $ singleton probHeader)
  "problemTitle" ## return (inlinesToHtml $ headerInlines probHeader)
  "problemDescription" ## return (blocksToHtml probDescr)
  {-
   "problemTags" ## I.textSplice (T.unwords probTags)
   "problemPath" ## I.textSplice (T.pack probPath)
   -}



-- | splices related to worksheet items
wsItemSplices :: UTCTime -> Either Blocks Summary -> ISplices
wsItemSplices now (Left blocks) = do
  "ifProblem" ## conditionalSplice False
  "itemBlocks" ## return (blocksToHtml blocks)
  
wsItemSplices now (Right Summary{..}) = do
  "ifProblem" ## conditionalSplice True
  "totalSubmissions" ## I.textSplice (T.pack $ show summaryTotal)
  "acceptedSubmissions" ## I.textSplice (T.pack $ show summaryAccepted)
  "ifSubmitted" ## conditionalSplice (summaryTotal > 0)
  "ifAccepted" ## conditionalSplice (summaryAccepted > 0)
  problemSplices summaryProb
  timerSplices (probID summaryProb) now (probLimit summaryProb)
         





-- | splices related to deadlines
timerSplices ::  PID -> UTCTime -> Maybe UTCTime -> ISplices
timerSplices pid now limit = do
  "ifOpen" ## conditionalSplice (maybe True (now<=) limit)
  "ifLate"  ## conditionalSplice (maybe False (now>) limit)
  "ifLimited" ## conditionalSplice (isJust limit)
  "endTime" ## maybe (return []) timeSplice limit
  "remainingTime" ## case limit of 
                   Nothing -> return []
                   Just t -> I.textSplice $ T.pack $
                             formatNominalDiffTime $ diffUTCTime t now
  "remainingJsTimer" ## case limit of 
                            Nothing -> return []
                            Just t -> return $ jsTimer pid $ diffUTCTime t now

  
-- | splice an UTC time as local time 
timeSplice :: UTCTime -> I.Splice Pythondo
timeSplice time = do tz <- liftIO getCurrentTimeZone
                     I.textSplice $ T.pack $
                       formatTime defaultTimeLocale "%c" $ utcToZonedTime tz time
    

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



-- | splices relating to a list of submissions
submissionsSplice :: [Submission] -> ISplices
submissionsSplice list = do
  "submissions" ## I.mapSplices (I.runChildrenWith . submissionSplices) list
  "totalSubmissions" ## I.textSplice (T.pack $ show n)
  "ifAccepted" ## conditionalSplice (any (\s->submitStatus s==Accepted) list)
  "ifSubmitted" ## conditionalSplice (n>0) 
  where n = length list

        
-- | splices relating to a single submission
submissionSplices :: Submission -> ISplices
submissionSplices Submission{..} = do 
  "submitID"   ## I.textSplice (T.pack $ show submitID)
  "submitPID"  ## I.textSplice (T.pack $ show submitPID)
  "submitTime" ## timeSplice submitTime 
  "submitText" ## I.textSplice submitText
  "submitStatus" ## I.textSplice (T.pack $ show submitStatus)
  "submitReport" ## I.textSplice submitReport 
  "ifAccepted" ## conditionalSplice (submitStatus == Accepted) 
  "ifOverdue" ##  conditionalSplice (submitStatus == Overdue) 
  "ifRejected" ## conditionalSplice (submitStatus/= Accepted &&
                                      submitStatus/=Overdue) 


{-
handleGetSubmission, handlePostSubmission :: Pythondo ()
handleGetSubmission 
    = method GET $ do 
        uid <- require getUserID <|> unauthorized
        pid <- require getProblemID 
        sid <- require getSubmissionID
        (prob,mesgs) <- getProblem pid
        sub <- getSubmission uid pid sid
        renderWithSplices "report" $ do inputAceEditorSplices
                                        problemSplices prob  
                                        submissionSplices sub
                                        warningsSplices mesgs


handlePostSubmission = method POST $ do
  uid <- require getUserID <|> unauthorized
  pid <- require getProblemID
  code <- require (getTextPost "editform.editor") 
  (prob,mesgs) <- getProblem pid
  now <- liftIO getCurrentTime
  sub <- postSubmission uid prob code
  incrCounter "submissions"
  renderWithSplices "report" (inputAceEditorSplices >>
                              problemSplices prob >>
                              submissionSplices sub >>
                              timerSplices pid now (probDeadline prob) >>
                              warningsSplices mesgs)
    
-}

{-
-- in exam mode show final report before loggin out
-- otherwise, logout immediately
handleConfirmLogout :: Pythondo ()
handleConfirmLogout = method GET $ do
  uid <- require getUserID <|> badRequest
  (probset,_) <- getProblemSet
  handleFinalReport uid probset


handleFinalReport :: UID -> ProblemSet -> Pythondo ()
handleFinalReport uid ProblemSet{..} | probsetExam = do
    let pids = map probID probsetProbs
    subs <- mapM (getBestSubmission uid) pids
    let psubs = [(p,s) | (p, Just s)<-zip probsetProbs subs]
    renderWithSplices "finalrep" $
      "problemList" ## I.mapSplices (I.runChildrenWith . splices) psubs
  where splices (prob,sub) = problemSplices prob >> submissionSplices sub

-- not in exam mode: proceed to logout immediately
handleFinalReport _ _ = handleLogout
-}

-----------------------------------------------------------------------------
-- administrator interface
-----------------------------------------------------------------------------
{-
handleAdminEdit :: Pythondo ()
handleAdminEdit = do
      uid <- require getUserID
      roles <- require getUserRoles
      guard (adminRole `elem` roles)
      method GET handleGet <|> method POST handlePost 
  where
    handleGet = do
      (_, mesgs) <- getProblemSet
      path <- getsRequest rqPathInfo
      opt_pid <- getParam "pid"
      source <- liftIO (catchIOError
                        (T.readFile $ B.toString path)
                        (\_ -> return ""))
      renderWithSplices "editfile" $ do
        inputAceEditorSplices
        warningsSplices mesgs
        "editPath" ## I.textSplice (T.pack $ B.toString path)
        "editText" ## I.textSplice source
        "problemID" ## maybe (return []) (I.textSplice . T.pack . B.toString) opt_pid
    handlePost  = do
      path <- getsRequest rqPathInfo
      opt_pid <- getParam "pid"
      source <- require (getTextPost "editform.editor")
      liftIO (T.writeFile (B.toString path) source)
      redirect (B.append "/problems/" $ maybe "" id opt_pid)


handleAdminSubmissions = method GET $ do
  uid <- require getUserID
  roles <- require getUserRoles
  guard (adminRole `elem` roles)
  serveFileAs "application/x-sqlite3" "submissions.db"
        
-}

      

------------------------------------------------------------------------------
-- | The application's routes.
  {-
routes :: [(ByteString, Pythondo ())]
routes = [ ("/login",                 handleLogin `catch` internalError)
         , ("/logout",                handleLogout `catch` internalError)
         , ("/problems",              handleProblemList `catch` internalError )  
         , ("/problems/:pid",         handleProblem `catch` internalError )
         , ("/files",                 serveDirectory problemDirPath <|> notFound)
         , ("/edit",                  handleAdminEdit `catch` internalError)
         , ("/submissions/:pid",      handlePostSubmission `catch` internalError)
         , ("/submissions/:pid/:sid", handleGetSubmission  `catch` internalError)
         , ("/admin/submissions",     handleAdminSubmissions `catch` internalError)
         , ("/asklogout",             handleConfirmLogout `catch` internalError)        
         , ("",                       serveDirectory "static" <|> notFound)
         ]
-}


routes :: [(ByteString, Pythondo ())]
routes = [ ("/login",                 handleLogin `catch` internalError)
         , ("/logout",                handleLogout `catch` internalError)
         , ("/docs",                  handleDocument `catch` internalError )
         , ("",                       serveDirectory "static" <|> notFound)
         ]


-- | current logged in full user name  
loggedInName :: SnapletLens b (AuthManager b) -> SnapletISplice b
loggedInName authmgr = do
    u <- lift $ withTop authmgr currentUser
    maybe (return []) (I.textSplice . userName) u 

authRoles :: SnapletLens b (AuthManager b) ->
             ([Role] -> Bool) -> SnapletISplice b
authRoles authmgr cond = do
   u <- lift $ withTop authmgr currentUser
   maybe (return []) (\u -> conditionalSplice (cond $ userRoles u)) u

-- | splice for current date & time
nowSplice :: I.Splice Pythondo
nowSplice = do t <- liftIO (getCurrentTime >>= utcToLocalZonedTime)
               I.textSplice (T.pack $ formatTime defaultTimeLocale "%c" t)



versionSplice :: I.Splice Pythondo
versionSplice = I.textSplice (T.pack (showVersion version))

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = 
  makeSnaplet "pythondo" "Web system for learning Python programming." Nothing $ do
    conf <- getSnapletUserConfig
    e <- liftIO $ initEkg conf
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 7200)

    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth
    let sc = do 
          "version" ## versionSplice
          "timeNow" ## nowSplice
          "loggedInName" ## loggedInName auth
          "ifAdmin" ## authRoles auth (adminRole `elem`)
          
    addConfig h (mempty & scInterpretedSplices .~ sc)
    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $
      \conn -> do Db.createTables conn
                  -- putStrLn "Updating problem tags db table"
                  -- probs <- readProblemDir >>= mapM readProblem
                  -- Db.updateProblems conn probs
    addRoutes routes

    py <- liftIO $ configPython conf
    safe <- liftIO $ configSafeExec conf
    prt <- liftIO $ configPrintConf conf
    ldap <- liftIO $ configLdapConf conf
    return $ App { _heist = h
                 , _sess = s
                 , _auth = a
                 , _db   = d
                 , pythonConf= py
                 , safeExecConf = safe
                 , ldapConf = ldap
                 , printConf = prt
                 , ekg = e
                 }


-- emptyConfig :: HeistConfig m
-- emptyConfig = HeistConfig noSplices noSplices noSplices noSplices []


-- initialize EKG server (if configured)
initEkg :: Config -> IO (Maybe Server)
initEkg conf = do enabled <- Configurator.require conf "ekg.enabled"
                  if enabled then 
                    do host <- Configurator.require conf "ekg.host"
                       port <- Configurator.require conf "ekg.port"
                       Just <$> forkServer host port                   
                    else return Nothing
  

{-
-- | get the current problem set
-- TODO: avoid re-reading the same problems every time
-- with some caching mechanism
getProblemSet :: Pythondo (ProblemSet, [Text])
getProblemSet = liftIO (readProblemSet problemSetPath)

getProblem :: PID -> Pythondo (Problem, [Text])
getProblem pid = do
  (probset,mesgs) <- getProblemSet
  case lookupProblemSet pid probset of
    Nothing -> badRequest
    Just p -> return (p,mesgs)
-}

-- get a worksheet by path
getWorksheet :: String -> Pythondo (Worksheet Problem)
getWorksheet path = liftIO (readWorksheet $ documentRoot </> path)

documentRoot :: FilePath
documentRoot = "docs"


{-
lookupProblemSet :: PID -> ProblemSet -> Maybe Problem
lookupProblemSet pid ProblemSet{..} =
  listToMaybe [p | p <- probsetProbs, probID p == pid] 
-}    

