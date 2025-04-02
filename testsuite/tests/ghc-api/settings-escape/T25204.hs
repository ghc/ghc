module Main where

import GHC
import GHC.Settings
import GHC.Driver.Env
import GHC.Driver.Pipeline
import GHC.Driver.Phases

import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.Info (os)

main :: IO ()
main = do
  libdir:_args <- getArgs

  top_dir <- makeAbsolute "./ghc-install-folder/lib with spaces"

  runGhc (Just libdir) $ do

    dflags <- hsc_dflags <$> getSession

    -- Try compiling a C file with a C compiler that contains a space.

    -- Set the C compiler to something which is known to have spaces in it,
    -- and check that compilation proceeds as expected.
    let
      tool_settings = toolSettings dflags
      cc = toolSettings_pgm_c tool_settings

    -- Create a directory with spaces for our shim compiler
    let shimCcDir = top_dir </> "shim cc dir"
    liftIO $ createDirectory shimCcDir

    -- Create a shim script that calls the original C compiler based on OS
    let (shimCcPath, shimCcContent) = case os of
          "mingw32" ->
            -- Windows batch file
            ( shimCcDir </> "cc_shim.bat"
            , "@echo off\r\n\"" ++ cc ++ "\" %*\r\n" )
          _ ->
            -- Unix shell script
            ( shimCcDir </> "cc_shim.sh"
            , "#!/bin/sh\n" ++ cc ++ " \"$@\"\n" )

    liftIO $ writeFile shimCcPath shimCcContent

    -- Make the script executable on Unix-like systems
    when (os /= "mingw32") $ liftIO $ do
      perms <- getPermissions shimCcPath
      setPermissions shimCcPath (setOwnerExecutable True perms)

    -- Use the shim compiler in our settings
    let
      tool_settings' = tool_settings { toolSettings_pgm_c = shimCcPath }

    -- Compile the test C file with our modified settings
    let c_file = "./T25204_C.c"

    ghc_ver_file <- liftIO $ makeAbsolute "./ghc-install-folder/ghc version.h"

    hsc_env <- getSession

    let dflags' =
          dflags { toolSettings = tool_settings'
                 , ghcVersionFile = Just ghc_ver_file
                 }
    setSession $ hsc_env { hsc_dflags = dflags' }
    hsc_env' <- getSession

    res <- liftIO $ compileFile hsc_env' NoStop (c_file, Nothing)
    case res of
      Nothing ->
        liftIO $ putStrLn "Compilation of C file failed"
      Just {} ->
        liftIO $ putStrLn "Compilation of C file succeeded"
