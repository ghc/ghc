-----------------------------------------------------------------------------
-- $Id: PreProcess.hs,v 1.1 2000/10/11 11:54:58 simonmar Exp $
--
-- Pre-process source files
--
-- (c) The University of Glasgow 2000
--
-----------------------------------------------------------------------------

module PreProcess (
	preprocess -- :: FilePath -> IO FilePath
   ) where

import TmpFiles
import DriverState
import DriverUtil

import IOExts

-----------------------------------------------------------------------------
-- preprocess takes a haskell source file and generates a raw .hs
-- file.  This involves passing the file through 'unlit', 'cpp', or both.

preprocess :: FilePath -> IO FilePath
preprocess filename = do
  let (basename, suffix) = splitFilename filename

  unlit_file <- unlit filename
  cpp_file   <- cpp unlit_file
  return cpp_file

-------------------------------------------------------------------------------
-- Unlit phase 

unlit :: FilePath -> IO FilePath
unlit input_fn
  | suffix /= unlitInputExt = return input_fn
  | otherwise =
     do output_fn <- newTempName cppInputExt
  	unlit <- readIORef pgm_L
     	unlit_flags <- getOpts opt_L
     	run_something "Literate pre-processor"
	   ("echo '# 1 \"" ++input_fn++"\"' > "++output_fn++" && "
	   ++ unlit ++ ' ':input_fn ++ " - >> " ++ output_fn)
	return output_fn
   where
	(filename, suffix) = splitFilename input_fn

-------------------------------------------------------------------------------
-- Cpp phase 

cpp :: FilePath -> IO FilePath
cpp input_fn
  = do src_opts <- getOptionsFromSource input_fn
       _ <- processArgs dynamic_flags src_opts []

       output_fn <- newTempName hscInputExt

       do_cpp <- readState cpp_flag
       if do_cpp
          then do

       	    cpp <- readIORef pgm_P
	    hscpp_opts <- getOpts opt_P
       	    hs_src_cpp_opts <- readIORef hs_source_cpp_opts

	    cmdline_include_paths <- readIORef include_paths
	    pkg_include_dirs <- getPackageIncludePath
	    let include_paths = map (\p -> "-I"++p) (cmdline_include_paths
							++ pkg_include_dirs)

	    verb <- is_verbose
	    run_something "C pre-processor" 
		(unwords
       	    	   (["echo '{-# LINE 1 \"" ++ input_fn ++ "\" -}'", ">", output_fn, "&&",
		     cpp, verb] 
		    ++ include_paths
		    ++ hs_src_cpp_opts
	    	    ++ hscpp_opts
		    ++ [ "-x", "c", input_fn, ">>", output_fn ]
		   ))
	  else do
	    run_something "Ineffective C pre-processor"
	           ("echo '{-# LINE 1 \""  ++ input_fn ++ "\" -}' > " 
		    ++ output_fn ++ " && cat " ++ input_fn
		    ++ " >> " ++ output_fn)
       return True

-----------------------------------------------------------------------------
-- utils

splitFilename :: String -> (String,String)
splitFilename f = (reverse (stripDot rev_basename), reverse rev_ext)
  where (rev_ext, rev_basename) = span ('.' /=) (reverse f)
        stripDot ('.':xs) = xs
        stripDot xs       = xs

