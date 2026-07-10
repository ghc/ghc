{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Test.Command(main) where

import Development.Shake
import Development.Shake.FilePath
import Control.Exception.Extra
import System.Time.Extra
import General.Extra
import Control.Monad.Extra
import System.Directory
import Test.Type
import System.Exit
import System.Process
import Data.Tuple.Extra
import Data.List.Extra
import Control.Monad.IO.Class
import System.Info.Extra
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS


main = testBuild test $ do
    -- shake_helper must be in a subdirectory so we can test placing that subdir on the $PATH
    let helper = toNative $ "helper/shake_helper" <.> exe
    let name !> test = do want [name]
                          name ~> do need ["helper/shake_helper" <.> exe]; test

    let helper_source = unlines
            ["import System.Process"
            ,"import Control.Monad"
            ,"import Control.Concurrent"
            ,"import System.Directory"
            ,"import System.Environment"
            ,"import System.Exit"
            ,"import System.IO"
            ,"import qualified Data.ByteString.Lazy.Char8 as LBS"
            ,"main = do"
            ,"    args <- getArgs"
            ,"    exe <- getExecutablePath"
            ,"    forM_ args $ \\(a:rg) -> do"
            ,"        case a of"
            ,"            'o' -> putStrLn rg"
            ,"            'e' -> hPutStrLn stderr rg"
            ,"            'x' -> exitFailure"
            ,"            'c' -> putStrLn =<< getCurrentDirectory"
            ,"            'v' -> putStrLn =<< getEnv rg"
            ,"            'w' -> threadDelay $ ceiling $ (read rg :: Double) * 1000000"
            ,"            'r' -> LBS.putStr $ LBS.replicate (read rg) 'x'"
            ,"            'i' -> putStr =<< getContents"
            ,"            's' -> void $ readProcess exe [rg] \"\""
            ,"        hFlush stdout"
            ,"        hFlush stderr"
            ]

    "shake_helper.hs" %> \out -> do
        need ["../../src/Test/Command.hs"]
        writeFileChanged out helper_source
    ["helper/shake_helper" <.> exe, "shake_helper.o", "shake_helper.hi"] &%> \_ -> do
        need ["shake_helper.hs"]
        cmd "ghc --make" "shake_helper.hs -o helper/shake_helper"

    "capture" !> do
        (Stderr err, Stdout out) <- cmd helper ["ostuff goes here","eother stuff here"]
        liftIO $ out === "stuff goes here\n"
        liftIO $ err === "other stuff here\n"
        liftIO $ waits $ \w -> do
            Stdouterr out <- cmd helper Shell ["o1",w,"e2",w,"o3"]
            out === "1\n2\n3\n"

    "failure" !> do
        (Exit e, Stdout (), Stderr ()) <- cmd helper "oo ee x"
        when (e == ExitSuccess) $ error "/= ExitSuccess"
        liftIO $ assertException ["BAD"] $ cmd_ helper "oo eBAD x" (EchoStdout False) (EchoStderr False)
        liftIO $ assertException ["MORE"] $ cmd_ helper "oMORE eBAD x" (WithStdout True) (WithStderr False) (EchoStdout False) (EchoStderr False)

    "throws" ~>
        cmd Shell "not_a_process foo"

    "cwd" !> do
        -- FIXME: Linux searches the Cwd argument for the file, Windows searches getCurrentDirectory
        helper <- liftIO $ canonicalizePath $ "helper/shake_helper" <.> exe
        liftIO $ createDirectoryRecursive "helper/tests"
        Stdout out <- cmd (Cwd "helper/tests") (Cwd "..") helper "c"
        let norm = fmap dropTrailingPathSeparator . canonicalizePath . trim
        liftIO $ join $ liftM2 (===) (norm out) (norm "helper")

    let checkTimeout act = do
            offset <- liftIO offsetTime
            act
            t <- liftIO offset
            putInfo $ "Timed out in " ++ showDuration t
            when (t < 2 || t > 8) $ error $ "failed to timeout, took " ++ show t

    "timeout1" !> checkTimeout (do
        Exit exit <- cmd (Timeout 2) helper "w20"
        liftIO $ assertBool (exit /= ExitSuccess) "exit was ExitSuccess")

    "timeout2" !> do checkTimeout $ liftIO $ timeout 2 $ cmd_ helper "w20"

    -- disabled on Windows because when you abort a Shell process you get a
    -- "Do you want to terminate the batch file (Y/N)"
    unless isWindows $ "timeout3" !> do checkTimeout $ liftIO $ timeout 2 $ cmd_ Shell helper "w20"

    "timeout4" !> do checkTimeout $ liftIO $ timeout 2 $ cmd_ helper "sw20"

    "timeout5" !> do checkTimeout $ liftIO $ timeout 2 $ cmd_ helper "i w20"

    "env" !> do
        -- use liftIO since it blows away PATH which makes lint-tracker stop working
        Stdout out <- liftIO $ cmd (Env [("FOO","HELLO SHAKE")]) Shell helper "vFOO"
        liftIO $ out === "HELLO SHAKE\n"
        StdoutTrim out <- cmd (AddEnv "FOO" "GOODBYE SHAKE") Shell helper "vFOO"
        liftIO $ out === "GOODBYE SHAKE"

    "space" !> do
        Stdout out <- cmd helper ["oSPACE 1"]
        liftIO $ out === "SPACE 1\n"
        Stdout out <- cmd Shell helper "\"oSPACE 2\""
        liftIO $ out === "SPACE 2\n"
        whenM (liftIO hasTracker) $ do
            Stdout out <- cmd Shell AutoDeps helper "\"oSPACE 2\""
            liftIO $ out === "SPACE 2\n"
        (Stdout (), CmdLine x) <- cmd helper ["oSPACE 3","oDIRECT"]
        unless (" \"oSPACE 3\" oDIRECT" `isSuffixOf` replace "\'" "\"" x) $
            fail $ "Invalid CmdLine, " ++ x

    "path" !> do
        let path = AddPath [dropTrailingPathSeparator "helper"] []
        cmd_ "helper/shake_helper"
        cmd_ $ "helper/shake_helper" <.> exe
        cmd_ path Shell "shake_helper"
        cmd_ path "shake_helper"

    "file" !> do
        let file = "file.txt"
        cmd_ helper (FileStdout file) (FileStderr file) (EchoStdout False) (EchoStderr False) (WithStderr False) "ofoo ebar obaz"
        liftIO $ assertContents file "foo\nbar\nbaz\n"
        liftIO $ waits $ \w -> do
            Stderr err <- cmd helper (FileStdout file) (FileStderr file) ["ofoo",w,"ebar",w,"obaz"]
            err === "bar\n"
            assertContents file "foo\nbar\nbaz\n"

    "timer" !> do
        timer $ cmd helper

    "binary" !> do
        (Stdout str, Stdout bs) <- cmd BinaryPipes helper "ofoo"
        liftIO $ (===) (str, bs) $ second BS.pack $ dupe $ if isWindows then "foo\r\n" else "foo\n"
        (Stdout str, Stdout bs) <- cmd helper "ofoo"
        liftIO $ (str, bs) === ("foo\n", BS.pack $ if isWindows then "foo\r\n" else "foo\n")
        pure ()

    "large" !> do
        (Stdout (_ :: String), CmdTime t1) <- cmd helper "r10000000"
        (Stdout (_ :: LBS.ByteString), CmdTime t2) <- cmd helper "r10000000"
        t3 <- withTempFile $ \file -> fromCmdTime <$> cmd helper "r10000000" (FileStdout file)
        liftIO $ putStrLn $ "Capturing 10Mb takes: " ++ intercalate ","
            [s ++ " = " ++ showDuration d | (s,d) <- [("String",t1),("ByteString",t2),("File",t3)]]

    "stdin" !> do
        withTempFile $ \file -> do
            liftIO $ writeFile file " "
            Stdout (x :: String) <- cmd helper "i" (Stdin "hello") (FileStdin file) (StdinBS $ LBS.pack "world")
            liftIO $ x === "hello world"

    "async" !> do
        let file = "async.txt"
        pid <- cmd helper (FileStdout file) "w2" "ohello"
        Nothing <- liftIO $ getProcessExitCode pid
        ExitSuccess <- liftIO $ waitForProcess pid
        liftIO $ assertContents file "hello\n"


test build = do
    -- reduce the overhead by running all the tests in parallel
    -- lint can make a big different to the command lines, so test with and without
    whenM hasTracker $
        build ["-j4","--no-lint"]
    build ["-j4"]

    assertException ["not_a_process foo"] (build ["throws","--quiet"])


timer :: (CmdResult r, MonadIO m) => (forall r . CmdResult r => m r) -> m r
timer act = do
    (CmdTime t, CmdLine x, r) <- act
    liftIO $ putStrLn $ "Command " ++ x ++ " took " ++ show t ++ " seconds"
    pure r

waits :: (String -> IO ()) -> IO ()
waits op = f 0
    where f w | w > 1 = op "w10"
              | otherwise = catch_ (op $ "w" ++ show w) $ const $ f $ w + 0.1
