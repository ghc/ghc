{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

import Control.Exception
import Control.Monad.IO.Class
import System.Environment
import System.FilePath
import System.IO.Error
#ifndef mingw32_HOST_OS
import System.Posix (readSymbolicLink)
#endif /* mingw32_HOST_OS */

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Types
import Distribution.System
import Distribution.Verbosity
import Distribution.Version

import Test.Cabal.Prelude

-- Test that foreign libraries work
-- Recording is turned off because versionedlib will or will not
-- be installed depending on if we're on Linux or not.
main = setupAndCabalTest . recordMode DoNotRecord $ do
    -- Foreign libraries don't work with GHC 7.6 and earlier
    skipUnless =<< ghcVersionIs (>= mkVersion [7,8])
    withPackageDb $ do
        setup_install []
        setup "copy" [] -- regression test #4156
        dist_dir <- fmap testDistDir getTestEnv
        lbi <- getLocalBuildInfoM
        let installDirs = absoluteInstallDirs (localPkgDescr lbi) lbi NoCopyDest

        -- Link a C program against the library
        _ <- runProgramM gccProgram
            [ "-o", "uselib"
            , "UseLib.c"
            , "-l", "myforeignlib"
            , "-L", flibdir installDirs ]

        -- Run the C program
        let ldPath = case hostPlatform lbi of
                       Platform _ OSX     -> "DYLD_LIBRARY_PATH"
                       Platform _ Windows -> "PATH"
                       Platform _ _other  -> "LD_LIBRARY_PATH"
        oldLdPath <- liftIO $ getEnv' ldPath
        withEnv [ (ldPath, Just $ flibdir installDirs ++ [searchPathSeparator] ++ oldLdPath) ] $ do
            cwd <- fmap testCurrentDir getTestEnv
            result <- runM (cwd </> "uselib") []
            assertOutputContains "5678" result
            assertOutputContains "189" result

        -- If we're on Linux, we should have built a library with a
        -- version. We will now check that it was installed correctly.
#ifndef mingw32_HOST_OS
        case hostPlatform lbi of
            Platform _ Linux -> do
                let libraryName = "libversionedlib.so.5.4.3"
                    libdir = flibdir installDirs
                    objdumpProgram = simpleProgram "objdump"
                (objdump, _) <- liftIO $ requireProgram normal objdumpProgram (withPrograms lbi)
                path1 <- liftIO $ readSymbolicLink $ libdir </> "libversionedlib.so"
                path2 <- liftIO $ readSymbolicLink $ libdir </> "libversionedlib.so.5"
                assertEqual "Symbolic link 'libversionedlib.so' incorrect"
                            path1 libraryName
                assertEqual "Symbolic link 'libversionedlib.so.5' incorrect"
                            path2 libraryName
                objInfo <- runM (programPath objdump) [
                    "-x"
                  , libdir </> libraryName
                  ]
                assertBool "SONAME of 'libversionedlib.so.5.4.3' incorrect" $
                  elem "libversionedlib.so.5" $ words $ resultOutput objInfo
            _ -> return ()
#endif /* mingw32_HOST_OS */

getEnv' :: String -> IO String
getEnv' = handle handler . getEnv
  where
    handler e = if isDoesNotExistError e
                  then return ""
                  else throw e
