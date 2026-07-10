{-# LANGUAGE CPP #-}
module Test.CloseFileHandles(main) where

import Test.Type

#ifdef mingw32_HOST_OS

main = testNone -- don't know how to do this on windows

#else

import Development.Shake
import Development.Shake.FilePath
import System.Posix.IO
import Control.Monad.Extra
import System.Exit
import System.IO

main = testBuild test $ do
    let helper = toNative $ "helper/close_file_handles_helper" <.> exe
    let name !> test = do want [name]
                          name ~> do need ["helper/close_file_handles_helper" <.> exe]; test

    let helper_source = unlines
            ["import System.Environment"
            ,"import System.Posix.IO"
            ,"import System.IO"
            ,"import System.Exit"
            ,""
            ,"main = do"
            ,"  args <- getArgs"
            ,"  case args of"
            ,"    [fdString] -> do"
            ,"       handle <- fdToHandle (read fdString)"
            ,"       hClose handle"
            ,"       exitSuccess"
            ,"    _ -> do"
            ,"      progName <- getProgName"
            ,"      hPutStrLn stderr (\"usage: \" ++ progName ++ \" <file descriptor number>\\n    tries closing the file descriptor number\\n    exits successful, if the file descriptor was open\")"
            ,"      exitWith (ExitFailure 3)"]

    "close_file_handles_helper.hs" %> \out -> do
        need ["../../src/Test/CloseFileHandles.hs"]
        writeFileChanged out helper_source

    ["helper/close_file_handles_helper"<.>exe, "close_file_handles_helper.hi", "close_file_handles_helper.o"] &%> \_ -> do
        need ["close_file_handles_helper.hs"]
        cmd "ghc --make" "close_file_handles_helper.hs -o helper/close_file_handles_helper"

    let callWithOpenFile cmdWithOpts = withTempFile $
            \file -> actionBracket (openFile file AppendMode) hClose $
                \h -> do fd <- liftIO $ handleToFd h
                         (Exit c, Stdout _, Stderr _) <- cmdWithOpts helper (show fd) :: Action (Exit, Stdout String, Stderr String)
                         pure c

    "defaultbehaviour" !> do
        c <- callWithOpenFile cmd
        liftIO $ assertBool (c == ExitSuccess) "handle closed without option CloseFileHandles"

    "closing" !> do
        c <- callWithOpenFile (cmd CloseFileHandles)
        liftIO $ assertBool (c /= ExitSuccess) "handle not closed with option CloseFileHandles"

test build = do
    whenM hasTracker $
        build ["-j4", "--no-lint"]
    build ["-j4"]

#endif
