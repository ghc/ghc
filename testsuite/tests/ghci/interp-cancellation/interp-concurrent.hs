{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

-- | Several threads share one GHC session, and so one external interpreter.
--
-- Checks that concurrent requests get their own replies, that killing threads
-- waiting on the interpreter leaves it usable, that killing a thread running a
-- splice cancels the splice in the interpreter, and that when the interpreter
-- dies it is stopped and the next request starts a new one.
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Data.Maybe (isJust, isNothing)
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (ProcessHandle, getPid, getProcessExitCode)
import System.Timeout (timeout)

import GHC
import GHC.Driver.Env (hscInterp)
import GHC.Driver.Monad (Session, reflectGhc, reifyGhc)
import GHC.Driver.Session (gopt_set, xopt_set)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Runtime.Interpreter (evalString, evalStringToIOString)
import GHC.Runtime.Interpreter.Types
import GHCi.Message (PipeClosedException)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [libdir] <- getArgs
  mapM_ removeIfExists markers
  runGhc (Just libdir) $ do
    let withTH fs = foldl xopt_set fs
          [LangExt.TemplateHaskell, LangExt.TemplateHaskellQuotes]
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags (withTH (gopt_set dflags Opt_ExternalInterpreter))
           { ghcLink = LinkInMemory }
    setContext [ IIDecl (simpleImportDecl (mkModuleName m))
               | m <- [ "Prelude", "Control.Concurrent", "Control.Exception"
                      , "System.IO", "System.IO.Error", "Language.Haskell.TH" ] ]
    reifyGhc $ \s -> do
      concurrentReplies s
      killWaiting s
      killSplice s
      interpreterDies s

--------------------------------------------------------------------------------
-- Scenarios

concurrentReplies :: Session -> IO ()
concurrentReplies s = do
  echo <- compile s echoExpr
  ws <- forM [1 .. 16 :: Int] $ \n -> async $
    forM [1 .. 50 :: Int] $ \k -> do
      let x = show (n, k)
      (== x) <$> evalEcho s echo x
  rs <- mapM (waiting "concurrent requests" . readMVar . snd) ws
  check "concurrent requests each get their own reply"
        (all (either (const False) and) rs)

-- | Kill threads waiting on two gated requests, and check a third request
-- made alongside them still gets its reply.
killWaiting :: Session -> IO ()
killWaiting s = do
  echo <- compile s echoExpr
  -- Compiled up front: compiling needs the interpreter, which 'a' may hold up.
  bAct <- compile s (gated "b.started" "b.gate" "pure \"b\"")
  (a, aRes) <- async $ inSession s $
    execStmt (gated "a.started" "a.gate" "pure ()") execOptions
  awaitFileUntil "the statement to start" "a.started" aRes
  (b, bRes) <- async $ evalFHV s bAct
  (_, cRes) <- async $ evalEcho s echo "c"
  threadDelay 100000 -- let b and c send their requests
  waiting "killing the thread waiting on the statement" (killThread a)
  waiting "killing the thread waiting on the other one" (killThread b)
  aR <- waiting "the killed statement thread to finish" (readMVar aRes)
  bR <- waiting "the other killed thread to finish" (readMVar bRes)
  check "killed threads stop waiting for their replies"
        (isKilled aR && isKilled bR)
  -- Let the server side evaluation of 'a' and 'b' go through, since eval
  -- requests ignore canceling atm.
  mapM_ (`writeFile` "") ["a.gate", "b.gate"]
  cR <- waiting "the remaining request's reply" (readMVar cRes)
  check "a request made alongside cancelled ones gets its reply"
        (either (const False) (== "c") cR)
  stillAnswers s echo

-- | Splices run in their own thread in the interpreter, so they don't hold up
-- other requests, and can be cancelled there.
killSplice :: Session -> IO ()
killSplice s = do
  echo <- compile s echoExpr
  (t, tRes) <- async $ inSession s $
    compileExprRemote (blockingSplice "th.started" "th.cancelled")
  awaitFileUntil "the splice to start" "th.started" tRes
  r <- waiting "a request while a splice runs" (evalEcho s echo "during splice")
  check "a running splice does not hold up other requests"
        (r == "during splice")
  waiting "killing the thread running the splice" (killThread t)
  tR <- waiting "the killed splice thread to finish" (readMVar tRes)
  check "the thread running the splice is killed" (isKilled tR)
  awaitFile "the splice to be cancelled in the interpreter" "th.cancelled"
  -- just printing what we can infer from the previous line.
  check "the splice is cancelled in the interpreter" True
  stillAnswers s echo

-- | Kill the interpreter while one thread waits on a statement and another
-- runs a splice.
interpreterDies :: Session -> IO ()
interpreterDies s = do
  _ <- compile s echoExpr
  ph <- runningInterp s >>= maybe (failWith "no interpreter running") pure
  pid <- getPid ph >>= maybe (failWith "interpreter has no pid") pure

  -- Splice first: compiling it needs the interpreter, which the statement
  -- may hold up.
  (_, fRes) <- async $ inSession s $
    compileExprRemote (blockingSplice "f.started" "f.cancelled")
  awaitFileUntil "the splice to start" "f.started" fRes
  (_, dRes) <- async $ evalIn s (gated "d.started" "d.gate" "pure \"d\"")
  awaitFileUntil "the statement to start" "d.started" dRes

  signalProcess sigKILL pid
  dR <- waiting "the statement thread to fail" (readMVar dRes)
  check "a thread waiting on a dead interpreter gets PipeClosedException"
        (isPipeClosed dR)
  fR <- waiting "the splice thread to fail" (readMVar fRes)
  check "a thread running a splice in a dead interpreter fails" (isLeft fR)

  st <- runningInterp s
  check "the dead interpreter is stopped" (isNothing st)
  ex <- getProcessExitCode ph
  check "its process is reaped" (isJust ex)

  r <- waiting "a request after the interpreter died"
         (evalIn s "pure \"alive\" :: IO String")
  check "the next request starts a new interpreter" (r == "alive")
  ph' <- runningInterp s >>= maybe (failWith "no interpreter running") pure
  pid' <- getPid ph'
  check "the new interpreter is a different process" (pid' /= Just pid)

--------------------------------------------------------------------------------
-- Talking to the interpreter through the session

inSession :: Session -> Ghc a -> IO a
inSession = flip reflectGhc

compile :: Session -> String -> IO ForeignHValue
compile s e = inSession s (compileExprRemote e)

-- | Compile an @IO String@ and run it.
evalIn :: Session -> String -> IO String
evalIn s e = compile s e >>= evalFHV s

-- | Run a compiled @IO String@.
evalFHV :: Session -> ForeignHValue -> IO String
evalFHV s fhv = inSession s $ do
  hsc_env <- getSession
  liftIO (evalString (hscInterp hsc_env) fhv)

evalEcho :: Session -> ForeignHValue -> String -> IO String
evalEcho s echo x = inSession s $ do
  hsc_env <- getSession
  liftIO (evalStringToIOString (hscInterp hsc_env) echo x)

stillAnswers :: Session -> ForeignHValue -> IO ()
stillAnswers s echo = do
  r <- waiting "a request after the cancellations" (evalEcho s echo "after")
  check "the interpreter still answers" (r == "after")

runningInterp :: Session -> IO (Maybe ProcessHandle)
runningInterp s = do
  interp <- inSession s (hscInterp <$> getSession)
  case interpInstance interp of
    ExternalInterp (ExtIServ st) -> readMVar (interpStatus st) >>= \case
      InterpRunning inst -> pure (Just (interpHandle (instProcess inst)))
      InterpPending      -> pure Nothing
    _ -> failWith "not using an external interpreter"

--------------------------------------------------------------------------------
-- Interpreted code

echoExpr :: String
echoExpr = "(\\x -> pure x) :: String -> IO String"

-- | Creates @started@, waits for @gate@ to exist, then runs @k@.
gated :: FilePath -> FilePath -> String -> String
gated started gate k = concat
  [ "let { waitFor p = catchIOError (withFile p ReadMode (\\_ -> pure ()))"
  , " (\\_ -> threadDelay 10000 >> waitFor p) }"
  , " in writeFile ", show started, " \"\" >> waitFor ", show gate, " >> ", k ]

-- | Creates @started@, then blocks. Creates @cancelled@ if interrupted.
blockingSplice :: FilePath -> FilePath -> String
blockingSplice started cancelled = concat
  [ "$(runIO (writeFile ", show started, " \"\""
  , " >> (threadDelay maxBound `onException` writeFile ", show cancelled, " \"\"))"
  , " >> [| () |])" ]

markers :: [FilePath]
markers = [ "a.started", "a.gate", "b.started", "b.gate"
          , "th.started", "th.cancelled"
          , "d.started", "d.gate", "f.started", "f.cancelled" ]

--------------------------------------------------------------------------------
-- Utilities

async :: IO a -> IO (ThreadId, MVar (Either SomeException a))
async act = do
  v <- newEmptyMVar
  t <- forkFinally act (putMVar v)
  pure (t, v)

awaitFile :: String -> FilePath -> IO ()
awaitFile what p = waiting what loop
  where
    loop = doesFileExist p >>= \b -> unless b (threadDelay 10000 >> loop)

-- | Like 'awaitFile', but fails straight away if the thread that should
-- create the file ends first.
awaitFileUntil :: String -> FilePath -> MVar (Either SomeException a) -> IO ()
awaitFileUntil what p res = waiting what loop
  where
    loop = do
      b <- doesFileExist p
      unless b $ tryReadMVar res >>= \case
        Just r  -> failWith ("waiting for " ++ what ++ ", the thread ended: "
                             ++ either show (const "no exception") r)
        Nothing -> threadDelay 10000 >> loop

removeIfExists :: FilePath -> IO ()
removeIfExists p = removeFile p `catch` \(_ :: IOException) -> pure ()

isKilled :: Either SomeException a -> Bool
isKilled = \case
  Left e | Just ThreadKilled <- fromException e -> True
  _ -> False

isPipeClosed :: Either SomeException a -> Bool
isPipeClosed = \case
  Left e | Just (_ :: PipeClosedException) <- fromException e -> True
  _ -> False

-- | Adds a timeout so we fail with a message rather than hang the whole test.
waiting :: String -> IO a -> IO a
waiting what act =
  timeout (60 * 1000000) act >>= maybe (failWith ("timed out waiting for " ++ what)) pure

failWith :: String -> IO a
failWith what = do
  hPutStrLn stderr ("FAILED: " ++ what)
  hFlush stderr
  exitFailure

check :: String -> Bool -> IO ()
check what ok
  | ok        = putStrLn ("ok: " ++ what)
  | otherwise = void (failWith what)
