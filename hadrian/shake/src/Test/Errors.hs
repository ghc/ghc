{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

module Test.Errors(main) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Test.Type
import Data.List.Extra
import Control.Monad
import Control.Concurrent.Extra
import General.GetOpt
import General.Extra
import Data.IORef
import Control.Exception.Extra
import System.Directory as IO
import System.Time.Extra
import qualified System.IO.Extra as IO


data Args = Die deriving (Eq,Enum,Bounded,Show)

newtype BadBinary = BadBinary String deriving (NFData,Show,Eq,Hashable,Typeable)
type instance RuleResult BadBinary = BadBinary
instance Binary BadBinary where
    put (BadBinary x) = put x
    get = do x <- get; if x == "bad" then error "get: BadBinary \"bad\"" else pure $ BadBinary x

main = testBuildArgs test optionsEnum $ \args -> do
    "norule" %> \_ ->
        need ["norule_isavailable"]

    "failcreate" %> \_ ->
        pure ()

    ["failcreates", "failcreates2"] &%> \_ ->
        writeFile' "failcreates" ""

    "recursive_" %> \_ -> need ["intermediate_"]
    "intermediate_" %> \_ -> need ["recursive_"]

    "rec1" %> \_ -> need ["rec2"]
    "rec2" %> \_ -> need ["rec1"]

    "systemcmd" %> \_ ->
        cmd "random_missing_command"

    "stack1" %> \_ -> need ["stack2"]
    "stack2" %> \_ -> need ["stack3"]
    "stack3" %> \_ -> error "crash"

    "staunch1" %> \out -> do
        liftIO $ sleep 0.1
        writeFile' out "test"
    "staunch2" %> \_ -> error "crash"

    let catcher out op = out %> \out -> do
            writeFile' out "0"
            op $ do src <- IO.readFile' out; writeFile out $ show (read src + 1 :: Int)
    catcher "finally1" $ actionFinally $ fail "die"
    catcher "finally2" $ actionFinally $ pure ()
    catcher "finally3" $ actionFinally $ liftIO $ sleep 10
    catcher "finally4" $ actionFinally $ need ["wait"]
    "wait" ~> do liftIO $ sleep 10
    catcher "exception1" $ actionOnException $ fail "die"
    catcher "exception2" $ actionOnException $ pure ()

    "retry*" %> \out -> do
        ref <- liftIO $ newIORef 3
        actionRetry (read [last out]) $ liftIO $ do
            old <- readIORef ref
            writeIORef ref $ old - 1
            if old == 0 then writeFile' out "" else fail "die"

    res <- newResource "resource_name" 1
    "resource" %> \_ ->
        withResource res 1 $
            need ["resource-dep"]

    "overlap.txt" %> \out -> writeFile' out "overlap.txt"
    "overlap.t*" %> \out -> writeFile' out "overlap.t*"
    "overlap.*" %> \out -> writeFile' out "overlap.*"
    ["*.txx","*.tox"] &%> \_ -> fail "do not run"
    ["*p.txx"] &%> \_ -> fail "do not run"

    "chain.2" %> \out -> do
        src <- readFile' "chain.1"
        if src == "err" then error "err_chain" else writeFileChanged out src
    "chain.3" %> \out -> copyFile' "chain.2" out

    "tempfile" %> \out -> do
        file <- withTempFile $ \file -> do
            liftIO $ assertExists file
            pure file
        liftIO $ assertMissing file
        withTempFile $ \file -> do
            liftIO $ assertExists file
            writeFile' out file
            fail "tempfile-died"

    "tempdir" %> \out -> do
        file <- withTempDir $ \dir -> do
            let file = dir </> "foo.txt"
            liftIO $ writeFile (dir </> "foo.txt") ""
                -- will throw if the directory does not exist
            writeFile' out ""
            pure file
        liftIO $ assertMissing file

    phony "fail1" $ fail "die1"
    phony "fail2" $ fail "die2"

    when (Die `elem` args) $ action $ error "death error"

    "fresh_dir" %> \out -> liftIO $ createDirectoryRecursive out
    "need_dir" %> \out -> do
        liftIO $ createDirectoryRecursive "existing_dir"
        need ["existing_dir"]
        writeFile' out ""

    "persist_failure.1" %> \out -> do
        liftIO $ appendFile "persist_failure.log" "[pre]"
        need ["persist_failure.2"]
        liftIO $ appendFile "persist_failure.log" "[post]"
        writeFile' out ""
    "persist_failure.2" %> \out -> do
        src <- readFile' "persist_failure.3"
        liftIO $ print ("persist_failure.3", src)
        if src == "die" then do
            liftIO $ appendFile "persist_failure.log" "[err]"
            fail "die"
        else
            writeFileChanged out src

    "fast_failure" %> \_ -> do
        liftIO $ sleep 0.1
        fail "die"
    "slow_success" %> \out -> do
        liftIO $ sleep 20
        writeFile' out ""

    addOracle $ \(BadBinary x) -> pure $ BadBinary $ 'b':x
    "badinput" %> \out -> do
        askOracle $ BadBinary "bad"
        liftIO $ appendFile out "x"
    "badoutput" %> \out -> do
        askOracle $ BadBinary "ad"
        liftIO $ appendFile out "x"
    "badnone" %> \out -> do
        alwaysRerun
        liftIO $ appendFile out "x"

    "produces1" %> \out -> do
        produces [out <.> "also"]
        writeFile' (out <.> "also") ""
        writeFile' out ""
    "produces2" %> \out -> do
        produces [out <.> "also"]
        writeFile' out ""

    "finalfinal" %> \out -> do
        writeFile' out ""
        lock <- liftIO newLock
        let output = withLock lock . appendFile out
        liftIO (sleep 100)
            `actionFinally` (output "X" >> sleep 0.1)
            `actionFinally` output "Y"

    let catching out = flip actionCatch $ \(e :: SomeException) -> writeFile' out $ show e
    "catch1" %> \out -> catching out $ fail "magic1"
    "catch2" %> \out -> catching out $ liftIO $ killThread =<< myThreadId
    "catch3.1" %> \out -> fail "magic3"
    "catch3.2" %> \out -> catching out $ need ["catch3.1"]

    -- not tested by default since only causes an error when idle GC is turned on
    phony "block" $
        liftIO $ putStrLn $ let x = x in x

test build = do
    -- on Windows, file paths may end up with \ separators, make sure we can still match them
    let crash args parts = assertExceptionAfter (replace "\\" "/") parts (build $ "--quiet" : args)
    build ["clean"]

    writeFile "chain.1" "x"
    build ["chain.3","--sleep"]
    writeFile "chain.1" "err"
    crash ["chain.3"] ["err_chain"]

    crash ["norule"] ["norule_isavailable"]
    crash ["failcreate"] ["failcreate"]
    crash ["failcreates"] ["failcreates"]
    crash ["recursive_"] ["recursive_","intermediate_","recursive"]
    crash ["rec1","rec2"] ["rec1","rec2","indirect recursion","recursive"]
    notMacCI $ crash ["systemcmd"] ["systemcmd","random_missing_command", "at cmd, called at"]
    crash ["stack1"] ["stack1","stack2","stack3","crash"]

    b <- IO.doesFileExist "staunch1"
    when b $ removeFile "staunch1"
    crash ["staunch1","staunch2","-j2"] ["crash"]
    assertBoolIO (not <$> IO.doesFileExist "staunch1") "File should not exist, should have crashed first"
    crash ["staunch1","staunch2","-j2","--keep-going","--silent"] ["crash"]
    assertBoolIO (IO.doesFileExist "staunch1") "File should exist, staunch should have let it be created"

    crash ["finally1"] ["die"]
    assertContents "finally1" "1"
    build ["finally2"]
    assertContents "finally2" "1"
    crash ["exception1"] ["die"]
    assertContents "exception1" "1"
    build ["exception2"]
    assertContents "exception2" "0"

    crash ["retry0"] ["positive","0"]
    crash ["retry1"] ["die"]
    build ["retry4"]

    forM_ ["finally3","finally4"] $ \name -> do
        t <- forkIO $ ignore $ build [name,"--exception"]
        retry 10 $ sleep 0.1 >> assertContents name "0"
        throwTo t (IndexOutOfBounds "test")
        retry 10 $ sleep 0.1 >> assertContents name "1"

    crash ["resource"] ["cannot currently introduce a dependency","withResource","resource_name"]

    build ["overlap.foo"]
    assertContents "overlap.foo" "overlap.*"
    build ["overlap.txt"]
    assertContents "overlap.txt" "overlap.txt"
    crash ["overlap.txx"] $
        ["key matches multiple rules","matched:  4","overlap.txx","overlap.t*","overlap.*","*.tox"] ++
        ["Test/Errors.hs"]

    crash ["tempfile"] ["tempfile-died"]
    src <- readFile "tempfile"
    assertMissing src
    build ["tempdir"]

    crash ["--die"] ["Shake","death error","Test/Errors.hs"]

    putStrLn "## BUILD errors"
    (out,_) <- IO.captureOutput $ build []
    assertBool ("nothing to do" `isInfixOf` out) $ "Expected 'nothing to do', but got: " ++ out

    putStrLn "## BUILD errors fail1 fail2 -k -j2"
    (out,_) <- IO.captureOutput $ try_ $ build ["fail1","fail2","-k","-j2",""]
    assertBool ("die1" `isInfixOf` out && "die2" `isInfixOf` out) $ "Expected 'die1' and 'die2', but got: " ++ out

    crash ["fresh_dir"] ["expected a file, got a directory","fresh_dir"]
    crash ["need_dir"] ["expected a file, got a directory","existing_dir"]

    -- check errors don't persist to the database, #428
    writeFile "persist_failure.log" ""
    writeFile "persist_failure.3" "test"
    build ["persist_failure.1","--sleep"]
    writeFile "persist_failure.3" "die"
    crash ["persist_failure.1","--sleep"] []
    assertContents "persist_failure.log" "[pre][post][err][pre]"
    writeFile "persist_failure.3" "test"
    build ["persist_failure.1","--sleep"]
    assertContents "persist_failure.log" "[pre][post][err][pre]"
    writeFile "persist_failure.3" "more"
    build ["persist_failure.1"]
    assertContents "persist_failure.log" "[pre][post][err][pre][pre][post]"

    -- check a fast failure aborts a slow success
    (t, _) <- duration $ crash ["fast_failure","slow_success","-j2"] ["die"]
    assertBool (t < 10) $ "Took too long, expected < 10, got " ++ show t

    -- for exceptions on Key we die while reading the database, and restart from scratch
    build ["badinput"]
    build ["badinput","--silent"]
    assertContents "badinput" "xx"
    build ["badnone","--silent"] -- must be able to still run other rules
    assertContents "badnone" "x"

    -- for exceptions on Value we die while running the rule that requires it
    build ["badoutput"]
    crash ["badoutput"] ["badoutput","BadBinary"]
    build ["badnone"] -- must be able to still run other rules
    assertContents "badnone" "xx"

    -- check that produces works
    build ["produces1"]
    crash ["produces2"] ["produces","produces2.also"]

    -- check finally doesn't run twice, See https://github.com/ndmitchell/shake/issues/611
    t <- forkIO $ build ["finalfinal","--quiet"]
    sleep 0.2
    killThread t
    sleep 0.5
    assertContents "finalfinal" "XY"

    build ["catch1"]
    assertContentsInfix "catch1" "magic1"
    crash ["catch2"] [show ThreadKilled]
    crash ["catch3.2"] ["magic3"]
