-----------------------------------------------------------------------------
-- $Id: TmpFiles.hs,v 1.21 2001/05/29 17:53:59 sof Exp $
--
-- Temporary file management
--
-- (c) The University of Glasgow 2000
--
-----------------------------------------------------------------------------

module TmpFiles (
   Suffix,
   initTempFileStorage,  -- :: IO ()
   cleanTempFiles,       -- :: Int -> IO ()
   cleanTempFilesExcept, -- :: Int -> [FilePath] -> IO ()
   newTempName,		 -- :: Suffix -> IO FilePath
   addFilesToClean,	 -- :: [FilePath] -> IO ()
   removeTmpFiles,	 -- :: Int -> [FilePath] -> IO ()
   v_TmpDir,
   kludgedSystem
 ) where

-- main
import DriverUtil
import Config
import Panic
import Util

-- hslibs
import Exception
import IOExts

-- std
import System
import Directory
import IO
import Monad

#include "../includes/config.h"
#include "HsVersions.h"

GLOBAL_VAR(v_FilesToClean, [],               [String] )
GLOBAL_VAR(v_TmpDir,       cDEFAULT_TMPDIR,  String   )


initTempFileStorage = do
	-- check whether TMPDIR is set in the environment
   IO.try (do dir <- getEnv "TMPDIR" -- fails if not set
#ifndef mingw32_TARGET_OS
	      writeIORef v_TmpDir dir
#endif
	      return ()
          )

cleanTempFiles :: Int -> IO ()
cleanTempFiles verb = do
  fs <- readIORef v_FilesToClean
  removeTmpFiles verb fs

cleanTempFilesExcept :: Int -> [FilePath] -> IO ()
cleanTempFilesExcept verb dont_delete = do
  fs <- readIORef v_FilesToClean
  let leftovers = filter (`notElem` dont_delete) fs
  removeTmpFiles verb leftovers
  writeIORef v_FilesToClean dont_delete

type Suffix = String

-- find a temporary name that doesn't already exist.
newTempName :: Suffix -> IO FilePath
newTempName extn = do
  x <- myGetProcessID
  tmp_dir <- readIORef v_TmpDir
  findTempName tmp_dir x
  where findTempName tmp_dir x = do
  	   let filename = tmp_dir ++ "/ghc" ++ show x ++ '.':extn
  	   b  <- doesFileExist filename
	   if b then findTempName tmp_dir (x+1)
		else do add v_FilesToClean filename -- clean it up later
		        return filename

addFilesToClean :: [FilePath] -> IO ()
addFilesToClean files = mapM_ (add v_FilesToClean) files

removeTmpFiles :: Int -> [FilePath] -> IO ()
removeTmpFiles verb fs = do
  let verbose = verb >= 2
      blowAway f =
	   (do  when verbose (hPutStrLn stderr ("Removing: " ++ f))
		if '*' `elem` f 
#if defined(mingw32_TARGET_OS) && defined(MINIMAL_UNIX_DEPS)
		  then kludgedSystem (unwords [cRM, dosifyPath f]) "Cleaning temp files" >> return ()
#else
		  then kludgedSystem (unwords [cRM, f]) "Cleaning temp files" >> return ()
#endif
		  else removeFile f)
	    `catchAllIO`
	   (\_ -> when verbose (hPutStrLn stderr 
				("Warning: can't remove tmp file " ++ f)))
  mapM_ blowAway fs


-- system that works feasibly under Windows (i.e. passes the command line to sh,
-- because system() under Windows doesn't look at SHELL, and always uses CMD.EXE)
kludgedSystem cmd phase_name
 = do
#if !defined(mingw32_TARGET_OS) || defined(MINIMAL_UNIX_DEPS)
    -- in the case where we do want to use an MSDOS command shell, we assume
    -- that files and paths have been converted to a form that's
    -- understandable to the command we're invoking.
   exit_code <- system cmd `catchAllIO` 
		   (\_ -> throwDyn (PhaseFailed phase_name (ExitFailure 1)))
#else
   pid <- myGetProcessID
   tmp_dir <- readIORef v_TmpDir
   let tmp = tmp_dir++"/sh"++show pid
   h <- openFile tmp WriteMode
   hPutStrLn h cmd
   hClose h
   exit_code <- system ("sh - " ++ tmp) `catchAllIO` 
		   (\_ -> removeFile tmp >>
                          throwDyn (PhaseFailed phase_name (ExitFailure 1)))
   removeFile tmp
#endif
   return exit_code
