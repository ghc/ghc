-----------------------------------------------------------------------------
-- $Id: TmpFiles.hs,v 1.11 2000/12/07 08:20:46 simonpj Exp $
--
-- Temporary file management
--
-- (c) The University of Glasgow 2000
--
-----------------------------------------------------------------------------

module TmpFiles (
   Suffix,
   initTempFileStorage,  -- :: IO ()
   cleanTempFiles,       -- :: IO ()
   newTempName,		 -- :: Suffix -> IO FilePath
   addFilesToClean,	 -- :: [FilePath] -> IO ()
   v_TmpDir
 ) where

-- main
import Config
import Util

-- hslibs
import Exception
import IOExts

-- std
import System
import Directory
import IO
import Monad

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

cleanTempFiles :: Bool -> IO ()
cleanTempFiles verbose = do
  fs <- readIORef v_FilesToClean

  let blowAway f =
	   (do  when verbose (hPutStrLn stderr ("Removing: " ++ f))
		if '*' `elem` f then system ("rm -f " ++ f) >> return ()
			        else removeFile f)
	    `catchAllIO`
	   (\_ -> when verbose (hPutStrLn stderr 
				("Warning: can't remove tmp file " ++ f)))
  mapM_ blowAway fs

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

add :: IORef [a] -> a -> IO ()
add var x = do
  xs <- readIORef var
  writeIORef var (x:xs)

