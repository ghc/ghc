{-# OPTIONS -W -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.2 2000/10/11 11:54:58 simonmar Exp $
--
-- GHC Driver program
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

-- with path so that ghc -M can find config.h
#include "../includes/config.h"

module Main (main) where

#include "HsVersions.h"

import CmSummarise ( getImports )
import CmStaticInfo ( Package(..) )
import TmpFiles
import Config
import CmdLineOpts
import Util ( global )

import RegexString
import Concurrent
#ifndef mingw32_TARGET_OS
import Posix
#endif
import Directory
import IOExts
import Exception
import Dynamic

import IO
import Monad
import List
import System
import Maybe
import Char

-----------------------------------------------------------------------------
-- Changes:

-- * -fglasgow-exts NO LONGER IMPLIES -package lang!!!  (-fglasgow-exts is a
--   dynamic flag whereas -package is a static flag.)

-----------------------------------------------------------------------------
-- ToDo:

-- certain options in OPTIONS pragmas are persistent through subsequent compilations.
-- new mkdependHS doesn't support all the options that the old one did (-X et al.)
-- time commands when run with -v
-- split marker
-- mkDLL
-- java generation
-- user ways
-- Win32 support: proper signal handling
-- make sure OPTIONS in .hs file propogate to .hc file if -C or -keep-hc-file-too
-- reading the package configuration file is too slow
-- -H, -K, -Rghc-timing
-- hi-diffs

-----------------------------------------------------------------------------
-- Differences vs. old driver:

-- No more "Enter your Haskell program, end with ^D (on a line of its own):"
-- consistency checking removed (may do this properly later)
-- removed -noC
-- no hi diffs (could be added later)
-- no -Ofile

-----------------------------------------------------------------------------
-- Phases

{-
Phase of the           | Suffix saying | Flag saying   | (suffix of)
compilation system     | ``start here''| ``stop after''| output file

literate pre-processor | .lhs          | -             | -
C pre-processor (opt.) | -             | -E            | -
Haskell compiler       | .hs           | -C, -S        | .hc, .s
C compiler (opt.)      | .hc or .c     | -S            | .s
assembler              | .s  or .S     | -c            | .o
linker                 | other         | -             | a.out
-}

data Phase 
	= MkDependHS	-- haskell dependency generation
	| Unlit
	| Cpp
	| Hsc
	| Cc
	| HCc		-- Haskellised C (as opposed to vanilla C) compilation
	| Mangle	-- assembly mangling, now done by a separate script.
	| SplitMangle	-- after mangler if splitting
	| SplitAs
	| As
	| Ln 
  deriving (Eq)

-----------------------------------------------------------------------------
-- Build the Hsc command line

build_hsc_opts :: IO [String]
build_hsc_opts = do
  opt_C_ <- getOpts opt_C		-- misc hsc opts

	-- take into account -fno-* flags by removing the equivalent -f*
	-- flag from our list.
  anti_flags <- getOpts anti_opt_C
  let basic_opts = opt_C_ ++ warn_opts ++ optimisation_opts ++ stg_opts
      filtered_opts = filter (`notElem` anti_flags) basic_opts

	-- warnings
  warn_level <- readIORef warning_opt
  let warn_opts =  case warn_level of
		  	W_default -> standardWarnings
		  	W_        -> minusWOpts
		  	W_all	  -> minusWallOpts
		  	W_not     -> []

	-- optimisation
  minus_o <- readIORef opt_level
  optimisation_opts <-
        case minus_o of
	    0 -> hsc_minusNoO_flags
	    1 -> hsc_minusO_flags
	    2 -> hsc_minusO2_flags
	    _ -> error "unknown opt level"
	    -- ToDo: -Ofile
 
	-- STG passes
  ways_ <- readIORef ways
  let stg_massage | WayProf `elem` ways_ =  "-fmassage-stg-for-profiling"
	          | otherwise            = ""

  stg_stats <- readIORef opt_StgStats
  let stg_stats_flag | stg_stats = "-dstg-stats"
		     | otherwise = ""

  let stg_opts = [ stg_massage, stg_stats_flag, "-flet-no-escape" ]
	-- let-no-escape always on for now

  verb <- is_verbose
  let hi_vers = "-fhi-version="++cProjectVersionInt

  static <- (do s <- readIORef static; if s then return "-static" else return "")

  l <- readIORef hsc_lang
  let lang = case l of
		HscC    -> "-olang=C"
		HscAsm  -> "-olang=asm"
		HscJava -> "-olang=java"

  -- get hi-file suffix
  hisuf <- readIORef hi_suf

  -- hi-suffix for packages depends on the build tag.
  package_hisuf <-
	do tag <- readIORef build_tag
	   if null tag
		then return "hi"
		else return (tag ++ "_hi")

  import_dirs <- readIORef import_paths
  package_import_dirs <- getPackageImportPath
  
  let hi_map = "-himap=" ++
		makeHiMap import_dirs hisuf 
			 package_import_dirs package_hisuf
		   	 split_marker

      hi_map_sep = "-himap-sep=" ++ [split_marker]

  scale <- readIORef scale_sizes_by
  heap  <- readState specific_heap_size
  stack <- readState specific_stack_size

  return 
	(  
	filtered_opts
	-- ToDo: C stub files
	++ [ hi_vers, static, verb, lang, hi_map, hi_map_sep ]
	)

makeHiMap 
  (import_dirs         :: [String])
  (hi_suffix           :: String)
  (package_import_dirs :: [String])
  (package_hi_suffix   :: String)   
  (split_marker        :: Char)
  = foldr (add_dir hi_suffix) 
	(foldr (add_dir package_hi_suffix) "" package_import_dirs)
	import_dirs
  where
     add_dir hisuf dir str = dir ++ "%." ++ hisuf ++ split_marker : str


getOptionsFromSource 
	:: String		-- input file
	-> IO [String]		-- options, if any
getOptionsFromSource file
  = do h <- openFile file ReadMode
       catchJust ioErrors (look h)
	  (\e -> if isEOFError e then return [] else ioError e)
  where
	look h = do
	    l <- hGetLine h
	    case () of
		() | null l -> look h
		   | prefixMatch "#" l -> look h
		   | prefixMatch "{-# LINE" l -> look h   -- -}
		   | Just (opts:_) <- matchRegex optionRegex l
			-> return (words opts)
		   | otherwise -> return []

optionRegex = mkRegex "\\{-#[ \t]+OPTIONS[ \t]+(.*)#-\\}"   -- -}

-----------------------------------------------------------------------------
-- Main loop

get_source_files :: [String] -> ([String],[String])
get_source_files = partition (('-' /=) . head)

main =
  -- all error messages are propagated as exceptions
  my_catchDyn (\dyn -> case dyn of
			  PhaseFailed _phase code -> exitWith code
			  Interrupted -> exitWith (ExitFailure 1)
			  _ -> do hPutStrLn stderr (show (dyn :: BarfKind))
			          exitWith (ExitFailure 1)
	      ) $ do

   -- make sure we clean up after ourselves
   later (do  forget_it <- readIORef keep_tmp_files
	      unless forget_it $ do
	      verb <- readIORef verbose
	      cleanTempFiles verb
	 )
	-- exceptions will be blocked while we clean the temporary files,
	-- so there shouldn't be any difficulty if we receive further
	-- signals.

	-- install signal handlers
   main_thread <- myThreadId

#ifndef mingw32_TARGET_OS
   let sig_handler = Catch (raiseInThread main_thread 
				(DynException (toDyn Interrupted)))
   installHandler sigQUIT sig_handler Nothing 
   installHandler sigINT  sig_handler Nothing
#endif

   pgm    <- getProgName
   writeIORef prog_name pgm

   argv   <- getArgs

	-- grab any -B options from the command line first
   argv'  <- setTopDir argv

	-- read the package configuration
   conf_file <- readIORef package_config
   contents <- readFile conf_file
   writeIORef package_details (read contents)

	-- find the phase to stop after (i.e. -E, -C, -c, -S flags)
   (flags2, todo, stop_flag) <- getToDo argv'
   writeIORef v_todo todo

	-- process all the other arguments, and get the source files
   non_static <- processArgs static_flags flags2 []

	-- find the build tag, and re-process the build-specific options
   more_opts <- findBuildTag
   _ <- processArgs static_opts more_opts []
 
	-- give the static flags to hsc
   build_hsc_opts

	-- the rest of the arguments are "dynamic"
   srcs <- processArgs dynamic_flags non_static []

    	-- complain about any unknown flags
   let unknown_flags = [ f | ('-':f) <- srcs ]
   mapM unknownFlagErr unknown_flags

	-- get the -v flag
   verb <- readIORef verbose

   when verb (do hPutStr stderr "Glasgow Haskell Compiler, Version "
 	         hPutStr stderr version_str
	         hPutStr stderr ", for Haskell 98, compiled by GHC version "
	         hPutStr stderr booter_version
	         hPutStr stderr "\n")

   when verb (hPutStrLn stderr ("Using package config file: " ++ conf_file))

	-- mkdependHS is special
   when (todo == DoMkDependHS) beginMkDependHS

	-- for each source file, find which phases to run
   pipelines <- mapM (genPipeline todo stop_flag) srcs
   let src_pipelines = zip srcs pipelines

   o_file <- readIORef output_file
   if isJust o_file && todo /= DoLink && length srcs > 1
	then throwDyn (UsageError "can't apply -o option to multiple source files")
	else do

   if null srcs then throwDyn (UsageError "no input files") else do

	-- save the flag state, because this could be modified by OPTIONS pragmas
	-- during the compilation, and we'll need to restore it before starting
	-- the next compilation.
   saved_driver_state <- readIORef driver_state

   let compileFile (src, phases) = do
	  r <- run_pipeline phases src (todo==DoLink) True orig_base orig_suff
	  writeIORef driver_state saved_driver_state
	  return r
	  where (orig_base, orig_suff) = splitFilename src

   o_files <- mapM compileFile src_pipelines

   when (todo == DoMkDependHS) endMkDependHS

   when (todo == DoLink) (do_link o_files)


-----------------------------------------------------------------------------
-- Which phase to stop at

data ToDo = DoMkDependHS | DoMkDLL | StopBefore Phase | DoLink
  deriving (Eq)

GLOBAL_VAR(v_todo, error "todo", ToDo)

todoFlag :: String -> Maybe ToDo
todoFlag "-M" = Just $ DoMkDependHS
todoFlag "-E" = Just $ StopBefore Hsc
todoFlag "-C" = Just $ StopBefore HCc
todoFlag "-S" = Just $ StopBefore As
todoFlag "-c" = Just $ StopBefore Ln
todoFlag _    = Nothing

getToDo :: [String]
	 -> IO ( [String]   -- rest of command line
	       , ToDo	    -- phase to stop at
	       , String	    -- "stop at" flag
	       )
getToDo flags 
  = case my_partition todoFlag flags of
	([]   , rest) -> return (rest, DoLink,  "") -- default is to do linking
	([(flag,one)], rest) -> return (rest, one, flag)
	(_    , _   ) -> 
	  throwDyn (OtherError "only one of the flags -M, -E, -C, -S, -c is allowed")

-----------------------------------------------------------------------------
-- genPipeline
--
-- Herein is all the magic about which phases to run in which order, whether
-- the intermediate files should be in /tmp or in the current directory,
-- what the suffix of the intermediate files should be, etc.

-- The following compilation pipeline algorithm is fairly hacky.  A
-- better way to do this would be to express the whole comilation as a
-- data flow DAG, where the nodes are the intermediate files and the
-- edges are the compilation phases.  This framework would also work
-- nicely if a haskell dependency generator was included in the
-- driver.

-- It would also deal much more cleanly with compilation phases that
-- generate multiple intermediates, (eg. hsc generates .hc, .hi, and
-- possibly stub files), where some of the output files need to be
-- processed further (eg. the stub files need to be compiled by the C
-- compiler).

-- A cool thing to do would then be to execute the data flow graph
-- concurrently, automatically taking advantage of extra processors on
-- the host machine.  For example, when compiling two Haskell files
-- where one depends on the other, the data flow graph would determine
-- that the C compiler from the first comilation can be overlapped
-- with the hsc comilation for the second file.

data IntermediateFileType
  = Temporary
  | Persistent
  deriving (Eq)

-- the first compilation phase for a given file is determined
-- by its suffix.
startPhase "lhs"   = Unlit
startPhase "hs"    = Cpp
startPhase "hc"    = HCc
startPhase "c"     = Cc
startPhase "raw_s" = Mangle
startPhase "s"     = As
startPhase "S"     = As
startPhase "o"     = Ln     
startPhase _       = Ln	   -- all unknown file types

genPipeline
   :: ToDo		-- when to stop
   -> String		-- "stop after" flag (for error messages)
   -> String		-- original filename
   -> IO [ 		-- list of phases to run for this file
	     (Phase,
	      IntermediateFileType,  -- keep the output from this phase?
	      String)   	     -- output file suffix
         ]	

genPipeline todo stop_flag filename
 = do
   split      <- readIORef split_object_files
   mangle     <- readIORef do_asm_mangling
   lang       <- readIORef hsc_lang
   keep_hc    <- readIORef keep_hc_files
   keep_raw_s <- readIORef keep_raw_s_files
   keep_s     <- readIORef keep_s_files

   let
   ----------- -----  ----   ---   --   --  -  -  -
    (_basename, suffix) = splitFilename filename

    start_phase = startPhase suffix

    haskell_ish_file = suffix `elem` [ "hs", "lhs", "hc" ]
    c_ish_file       = suffix `elem` [ "c", "s", "S" ]  -- maybe .cc et al.??

   -- for a .hc file, or if the -C flag is given, we need to force lang to HscC
    real_lang 
	| suffix == "hc"  = HscC
	| todo == StopBefore HCc && lang /= HscC && haskell_ish_file = HscC
	| otherwise = lang

   let
   ----------- -----  ----   ---   --   --  -  -  -
    pipeline
      | todo == DoMkDependHS = [ Unlit, Cpp, MkDependHS ]

      | haskell_ish_file = 
       case real_lang of
	HscC    | split && mangle -> [ Unlit, Cpp, Hsc, HCc, Mangle, 
					SplitMangle, SplitAs ]
	        | mangle          -> [ Unlit, Cpp, Hsc, HCc, Mangle, As ]
	        | split	       	  -> not_valid
	        | otherwise       -> [ Unlit, Cpp, Hsc, HCc, As ]

	HscAsm  | split           -> [ Unlit, Cpp, Hsc, SplitMangle, SplitAs ]
	        | otherwise       -> [ Unlit, Cpp, Hsc, As ]

	HscJava	| split	          -> not_valid
		| otherwise       -> error "not implemented: compiling via Java"

      | c_ish_file      = [ Cc, As ]

      | otherwise       = [ ]  -- just pass this file through to the linker

	-- ToDo: this is somewhat cryptic
    not_valid = throwDyn (OtherError ("invalid option combination"))
   ----------- -----  ----   ---   --   --  -  -  -

	-- this shouldn't happen.
   if start_phase /= Ln && start_phase `notElem` pipeline
	then throwDyn (OtherError ("can't find starting phase for "
				    ++ filename))
	else do

	-- if we can't find the phase we're supposed to stop before,
	-- something has gone wrong.
   case todo of
	StopBefore phase -> 
	   when (phase /= Ln 
		 && phase `notElem` pipeline
	   	 && not (phase == As && SplitAs `elem` pipeline)) $
	      throwDyn (OtherError 
		("flag " ++ stop_flag
		 ++ " is incompatible with source file `" ++ filename ++ "'"))
	_ -> return ()

   let
   ----------- -----  ----   ---   --   --  -  -  -
      annotatePipeline
	 :: [Phase]		-- raw pipeline
	 -> Phase		-- phase to stop before
     	 -> [(Phase, IntermediateFileType, String{-file extension-})]
      annotatePipeline []     _    = []
      annotatePipeline (Ln:_) _    = []
      annotatePipeline (phase:next_phase:ps) stop = 
     	  (phase, keep_this_output, phase_input_ext next_phase)
	     : annotatePipeline (next_phase:ps) stop
     	  where
     		keep_this_output
     		     | next_phase == stop = Persistent
     		     | otherwise =
     			case next_phase of
     			     Ln -> Persistent
     			     Mangle | keep_raw_s -> Persistent
     			     As     | keep_s     -> Persistent
     			     HCc    | keep_hc    -> Persistent
     			     _other              -> Temporary

	-- add information about output files to the pipeline
	-- the suffix on an output file is determined by the next phase
	-- in the pipeline, so we add linking to the end of the pipeline
	-- to force the output from the final phase to be a .o file.
      stop_phase = case todo of StopBefore phase -> phase
				DoMkDependHS	 -> Ln
				DoLink           -> Ln
      annotated_pipeline = annotatePipeline (pipeline ++ [ Ln ]) stop_phase

      phase_ne p (p1,_,_) = (p1 /= p)
   ----------- -----  ----   ---   --   --  -  -  -

   return $
     dropWhile (phase_ne start_phase) . 
	foldr (\p ps -> if phase_ne stop_phase p then p:ps else [])  []
		$ annotated_pipeline



run_pipeline
  :: [ (Phase, IntermediateFileType, String) ] -- phases to run
  -> String			-- input file
  -> Bool			-- doing linking afterward?
  -> Bool			-- take into account -o when generating output?
  -> String			-- original basename (eg. Main)
  -> String			-- original suffix   (eg. hs)
  -> IO String			-- return final filename

run_pipeline [] input_fn _ _ _ _ = return input_fn
run_pipeline ((phase, keep, o_suffix):phases) 
	input_fn do_linking use_ofile orig_basename orig_suffix
  = do

     output_fn <- outputFileName (null phases) keep o_suffix

     carry_on <- run_phase phase orig_basename orig_suffix input_fn output_fn
	-- sometimes we bail out early, eg. when the compiler's recompilation
	-- checker has determined that recompilation isn't necessary.
     if not carry_on 
	then do let (_,keep,final_suffix) = last phases
	        ofile <- outputFileName True keep final_suffix
		return ofile
	else do -- carry on ...

	-- sadly, ghc -E is supposed to write the file to stdout.  We
	-- generate <file>.cpp, so we also have to cat the file here.
     when (null phases && phase == Cpp) $
	run_something "Dump pre-processed file to stdout"
		      ("cat " ++ output_fn)

     run_pipeline phases output_fn do_linking use_ofile orig_basename orig_suffix

  where
     outputFileName last_phase keep suffix
  	= do o_file <- readIORef output_file
   	     if last_phase && not do_linking && use_ofile && isJust o_file
   	       then case o_file of 
   		       Just s  -> return s
   		       Nothing -> error "outputFileName"
   	       else if keep == Persistent
   			   then do f <- odir_ify (orig_basename ++ '.':suffix)
   				   osuf_ify f
   			   else newTempName suffix

-------------------------------------------------------------------------------
-- mkdependHS

	-- flags
GLOBAL_VAR(dep_makefile, 	"Makefile", String);
GLOBAL_VAR(dep_include_prelude, False, Bool);
GLOBAL_VAR(dep_ignore_dirs,	[], [String]);
GLOBAL_VAR(dep_suffixes,	[], [String]);
GLOBAL_VAR(dep_warnings,	True, Bool);

	-- global vars
GLOBAL_VAR(dep_makefile_hdl,   	error "dep_makefile_hdl", Maybe Handle);
GLOBAL_VAR(dep_tmp_file,       	error "dep_tmp_file", String);
GLOBAL_VAR(dep_tmp_hdl,        	error "dep_tmp_hdl", Handle);
GLOBAL_VAR(dep_dir_contents,   	error "dep_dir_contents", [(String,[String])]);

depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"

-- for compatibility with the old mkDependHS, we accept options of the form
-- -optdep-f -optdep.depend, etc.
dep_opts = [
   (  "s", 			SepArg (add dep_suffixes) ),
   (  "f", 			SepArg (writeIORef dep_makefile) ),
   (  "w", 			NoArg (writeIORef dep_warnings False) ),
   (  "-include-prelude",  	NoArg (writeIORef dep_include_prelude True) ),
   (  "X", 			Prefix (addToDirList dep_ignore_dirs) ),
   (  "-exclude-directory=",	Prefix (addToDirList dep_ignore_dirs) )
 ]

beginMkDependHS :: IO ()
beginMkDependHS = do

  	-- slurp in the mkdependHS-style options
  flags <- getOpts opt_dep
  _ <- processArgs dep_opts flags []

     	-- open a new temp file in which to stuff the dependency info
     	-- as we go along.
  dep_file <- newTempName "dep"
  writeIORef dep_tmp_file dep_file
  tmp_hdl <- openFile dep_file WriteMode
  writeIORef dep_tmp_hdl tmp_hdl

  	-- open the makefile
  makefile <- readIORef dep_makefile
  exists <- doesFileExist makefile
  if not exists
	then do 
	   writeIORef dep_makefile_hdl Nothing
	   return ()

	else do
  	   makefile_hdl <- openFile makefile ReadMode
  	   writeIORef dep_makefile_hdl (Just makefile_hdl)

		-- slurp through until we get the magic start string,
		-- copying the contents into dep_makefile
  	   let slurp = do
		l <- hGetLine makefile_hdl
		if (l == depStartMarker)
			then return ()
			else do hPutStrLn tmp_hdl l; slurp
	 
		-- slurp through until we get the magic end marker,
		-- throwing away the contents
  	   let chuck = do
		l <- hGetLine makefile_hdl
		if (l == depEndMarker)
			then return ()
			else chuck
	 
	   catchJust ioErrors slurp 
		(\e -> if isEOFError e then return () else ioError e)
	   catchJust ioErrors chuck
		(\e -> if isEOFError e then return () else ioError e)


	-- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depStartMarker

  	-- cache the contents of all the import directories, for future
	-- reference.
  import_dirs <- readIORef import_paths
  pkg_import_dirs <- getPackageImportPath
  import_dir_contents <- mapM getDirectoryContents import_dirs
  pkg_import_dir_contents <- mapM getDirectoryContents pkg_import_dirs
  writeIORef dep_dir_contents 
	(zip import_dirs import_dir_contents ++
  	 zip pkg_import_dirs pkg_import_dir_contents)

	-- ignore packages unless --include-prelude is on
  include_prelude <- readIORef dep_include_prelude
  when (not include_prelude) $
    mapM_ (add dep_ignore_dirs) pkg_import_dirs

  return ()


endMkDependHS :: IO ()
endMkDependHS = do
  makefile     <- readIORef dep_makefile
  makefile_hdl <- readIORef dep_makefile_hdl
  tmp_file     <- readIORef dep_tmp_file
  tmp_hdl      <- readIORef dep_tmp_hdl

	-- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depEndMarker

  case makefile_hdl of
     Nothing  -> return ()
     Just hdl -> do

	  -- slurp the rest of the orignal makefile and copy it into the output
  	let slurp = do
		l <- hGetLine hdl
		hPutStrLn tmp_hdl l
		slurp
	 
  	catchJust ioErrors slurp 
		(\e -> if isEOFError e then return () else ioError e)

	hClose hdl

  hClose tmp_hdl  -- make sure it's flushed

	-- create a backup of the original makefile
  when (isJust makefile_hdl) $
     run_something ("Backing up " ++ makefile)
	(unwords [ "cp", makefile, makefile++".bak" ])

  	-- copy the new makefile in place
  run_something "Installing new makefile"
	(unwords [ "cp", tmp_file, makefile ])


findDependency :: String -> Import -> IO (Maybe (String, Bool))
findDependency mod imp = do
   dir_contents <- readIORef dep_dir_contents
   ignore_dirs  <- readIORef dep_ignore_dirs
   hisuf <- readIORef hi_suf

   let
     (imp_mod, is_source) = 
	case imp of
	   Normal str -> (str, False)
	   Source str -> (str, True )	

     imp_hi = imp_mod ++ '.':hisuf
     imp_hiboot = imp_mod ++ ".hi-boot"
     imp_hiboot_v = imp_mod ++ ".hi-boot-" ++ cHscIfaceFileVersion
     imp_hs = imp_mod ++ ".hs"
     imp_lhs = imp_mod ++ ".lhs"

     deps | is_source = [ imp_hiboot_v, imp_hiboot, imp_hs, imp_lhs ]
     	  | otherwise = [ imp_hi, imp_hs, imp_lhs ]

     search [] = throwDyn (OtherError ("can't find one of the following: " ++
				      unwords (map (\d -> '`': d ++ "'") deps) ++
				      " (imported from `" ++ mod ++ "')"))
     search ((dir, contents) : dirs)
	   | null present = search dirs
	   | otherwise = 
		if dir `elem` ignore_dirs 
			then return Nothing
			else if is_source
				then if dep /= imp_hiboot_v 
					then return (Just (dir++'/':imp_hiboot, False))	
					else return (Just (dir++'/':dep, False))	
				else return (Just (dir++'/':imp_hi, not is_source))
	   where
		present = filter (`elem` contents) deps
		dep     = head present
 
   -- in
   search dir_contents


-----------------------------------------------------------------------------
-- MkDependHS phase

run_phase MkDependHS basename suff input_fn _output_fn = do 
   src <- readFile input_fn
   let imports = getImports src

   deps <- mapM (findDependency basename) imports

   osuf_opt <- readIORef output_suf
   let osuf = case osuf_opt of
			Nothing -> "o"
			Just s  -> s

   extra_suffixes <- readIORef dep_suffixes
   let suffixes = osuf : map (++ ('_':osuf)) extra_suffixes
       ofiles = map (\suf -> basename ++ '.':suf) suffixes
   	   
   objs <- mapM odir_ify ofiles
   
   hdl <- readIORef dep_tmp_hdl

	-- std dependeny of the object(s) on the source file
   hPutStrLn hdl (unwords objs ++ " : " ++ basename ++ '.':suff)

   let genDep (dep, False {- not an hi file -}) = 
	  hPutStrLn hdl (unwords objs ++ " : " ++ dep)
       genDep (dep, True  {- is an hi file -}) = do
	  hisuf <- readIORef hi_suf
	  let dep_base = remove_suffix '.' dep
	      deps = (dep_base ++ hisuf)
		     : map (\suf -> dep_base ++ suf ++ '_':hisuf) extra_suffixes
		  -- length objs should be == length deps
	  sequence_ (zipWith (\o d -> hPutStrLn hdl (o ++ " : " ++ d)) objs deps)

   mapM genDep [ d | Just d <- deps ]

   return True

-- add the lines to dep_makefile:
	   -- always:
		   -- this.o : this.hs

  	   -- if the dependency is on something other than a .hi file:
   		   -- this.o this.p_o ... : dep
   	   -- otherwise
   		   -- if the import is {-# SOURCE #-}
   			   -- this.o this.p_o ... : dep.hi-boot[-$vers]
   			   
   		   -- else
   			   -- this.o ...   : dep.hi
   			   -- this.p_o ... : dep.p_hi
   			   -- ...
   
   	   -- (where .o is $osuf, and the other suffixes come from
   	   -- the cmdline -s options).
   
-----------------------------------------------------------------------------
-- Hsc phase

run_phase Hsc	basename suff input_fn output_fn
  = do  hsc <- readIORef pgm_C
	
  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the import path, since this is
  -- what gcc does, and it's probably what you want.
	let current_dir = getdir basename
	
	paths <- readIORef include_paths
	writeIORef include_paths (current_dir : paths)
	
  -- build the hsc command line
	hsc_opts <- build_hsc_opts
	
	doing_hi <- readIORef produceHi
	tmp_hi_file <- if doing_hi 	
			  then newTempName "hi"
			  else return ""
	
  -- tmp files for foreign export stub code
	tmp_stub_h <- newTempName "stub_h"
	tmp_stub_c <- newTempName "stub_c"
	
  -- figure out where to put the .hi file
	ohi    <- readIORef output_hi
	hisuf  <- readIORef hi_suf
	let hi_flags = case ohi of
			   Nothing -> [ "-hidir="++current_dir, "-hisuf="++hisuf ]
			   Just fn -> [ "-hifile="++fn ]

  -- figure out if the source has changed, for recompilation avoidance.
  -- only do this if we're eventually going to generate a .o file.
  -- (ToDo: do when generating .hc files too?)
  --
  -- Setting source_unchanged to "-fsource_unchanged" means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to "" tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
	do_recomp <- readIORef recomp
	todo <- readIORef v_todo
        o_file <- odir_ify (basename ++ '.':phase_input_ext Ln)
	source_unchanged <- 
          if not (do_recomp && ( todo == DoLink || todo == StopBefore Ln ))
	     then return ""
	     else do t1 <- getModificationTime (basename ++ '.':suff)
		     o_file_exists <- doesFileExist o_file
		     if not o_file_exists
		        then return ""	-- Need to recompile
			else do t2 <- getModificationTime o_file
			        if t2 > t1
				  then return "-fsource-unchanged"
				  else return ""

  -- run the compiler!
	run_something "Haskell Compiler" 
		 (unwords (hsc : input_fn : (
		    hsc_opts
		    ++ hi_flags
		    ++ [ 
			  source_unchanged,
			  "-ofile="++output_fn, 
			  "-F="++tmp_stub_c, 
			  "-FH="++tmp_stub_h 
		       ]
		    ++ stat_opts
		 )))

  -- check whether compilation was performed, bail out if not
	b <- doesFileExist output_fn
	if not b && not (null source_unchanged) -- sanity
		then do run_something "Touching object file"
			    ("touch " ++ o_file)
			return False
		else do -- carry on...

  -- Deal with stubs
	let stub_h = basename ++ "_stub.h"
	let stub_c = basename ++ "_stub.c"
	
		-- copy .h_stub file into current dir if present
	b <- doesFileExist tmp_stub_h
	when b (do
	      	run_something "Copy stub .h file"
				("cp " ++ tmp_stub_h ++ ' ':stub_h)
	
			-- #include <..._stub.h> in .hc file
		addCmdlineHCInclude tmp_stub_h	-- hack

			-- copy the _stub.c file into the current dir
		run_something "Copy stub .c file" 
		    (unwords [ 
			"rm -f", stub_c, "&&",
			"echo \'#include \""++stub_h++"\"\' >"++stub_c, " &&",
			"cat", tmp_stub_c, ">> ", stub_c
			])

			-- compile the _stub.c file w/ gcc
		pipeline <- genPipeline (StopBefore Ln) "" stub_c
		run_pipeline pipeline stub_c False{-no linking-} 
				False{-no -o option-}
				(basename++"_stub") "c"

		add ld_inputs (basename++"_stub.o")
	 )
	return True

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

run_phase cc_phase _basename _suff input_fn output_fn
   | cc_phase == Cc || cc_phase == HCc
   = do	cc <- readIORef pgm_c
       	cc_opts <- (getOpts opt_c)
       	cmdline_include_dirs <- readIORef include_paths

        let hcc = cc_phase == HCc

		-- add package include paths even if we're just compiling
		-- .c files; this is the Value Add(TM) that using
		-- ghc instead of gcc gives you :)
        pkg_include_dirs <- getPackageIncludePath
	let include_paths = map (\p -> "-I"++p) (cmdline_include_dirs 
							++ pkg_include_dirs)

	c_includes <- getPackageCIncludes
	cmdline_includes <- readState cmdline_hc_includes -- -#include options

	let cc_injects | hcc = unlines (map mk_include 
					(c_includes ++ reverse cmdline_includes))
		       | otherwise = ""
	    mk_include h_file = 
		case h_file of 
		   '"':_{-"-} -> "#include "++h_file
		   '<':_      -> "#include "++h_file
		   _          -> "#include \""++h_file++"\""

	cc_help <- newTempName "c"
	h <- openFile cc_help WriteMode
	hPutStr h cc_injects
	hPutStrLn h ("#include \"" ++ input_fn ++ "\"\n")
	hClose h

	ccout <- newTempName "ccout"

	mangle <- readIORef do_asm_mangling
	(md_c_flags, md_regd_c_flags) <- machdepCCOpts

        verb <- is_verbose

	o2 <- readIORef opt_minus_o2_for_C
	let opt_flag | o2        = "-O2"
		     | otherwise = "-O"

	pkg_extra_cc_opts <- getPackageExtraCcOpts

	excessPrecision <- readState excess_precision

	run_something "C Compiler"
	 (unwords ([ cc, "-x", "c", cc_help, "-o", output_fn ]
		   ++ md_c_flags
		   ++ (if cc_phase == HCc && mangle
			 then md_regd_c_flags
			 else [])
		   ++ [ verb, "-S", "-Wimplicit", opt_flag ]
		   ++ [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]
		   ++ cc_opts
#ifdef mingw32_TARGET_OS
                   ++ [" -mno-cygwin"]
#endif
		   ++ (if excessPrecision then [] else [ "-ffloat-store" ])
		   ++ include_paths
		   ++ pkg_extra_cc_opts
--		   ++ [">", ccout]
		   ))
	return True

	-- ToDo: postprocess the output from gcc

-----------------------------------------------------------------------------
-- Mangle phase

run_phase Mangle _basename _suff input_fn output_fn
  = do mangler <- readIORef pgm_m
       mangler_opts <- getOpts opt_m
       machdep_opts <-
	 if (prefixMatch "i386" cTARGETPLATFORM)
	    then do n_regs <- readState stolen_x86_regs
		    return [ show n_regs ]
	    else return []
       run_something "Assembly Mangler"
	(unwords (mangler : 
		     mangler_opts
		  ++ [ input_fn, output_fn ]
		  ++ machdep_opts
		))
       return True

-----------------------------------------------------------------------------
-- Splitting phase

run_phase SplitMangle _basename _suff input_fn _output_fn
  = do  splitter <- readIORef pgm_s

	-- this is the prefix used for the split .s files
	tmp_pfx <- readIORef tmpdir
	x <- getProcessID
	let split_s_prefix = tmp_pfx ++ "/ghc" ++ show x
	writeIORef split_prefix split_s_prefix
	addFilesToClean (split_s_prefix ++ "__*") -- d:-)

	-- allocate a tmp file to put the no. of split .s files in (sigh)
	n_files <- newTempName "n_files"

	run_something "Split Assembly File"
	 (unwords [ splitter
		  , input_fn
		  , split_s_prefix
		  , n_files ]
	 )

	-- save the number of split files for future references
	s <- readFile n_files
	let n = read s :: Int
	writeIORef n_split_files n
	return True

-----------------------------------------------------------------------------
-- As phase

run_phase As _basename _suff input_fn output_fn
  = do 	as <- readIORef pgm_a
        as_opts <- getOpts opt_a

        cmdline_include_paths <- readIORef include_paths
        let cmdline_include_flags = map (\p -> "-I"++p) cmdline_include_paths
        run_something "Assembler"
	   (unwords (as : as_opts
		       ++ cmdline_include_flags
		       ++ [ "-c", input_fn, "-o",  output_fn ]
		    ))
	return True

run_phase SplitAs basename _suff _input_fn _output_fn
  = do  as <- readIORef pgm_a
        as_opts <- getOpts opt_a

	split_s_prefix <- readIORef split_prefix
	n <- readIORef n_split_files

	odir <- readIORef output_dir
	let real_odir = case odir of
				Nothing -> basename
				Just d  -> d

	let assemble_file n = do
		    let input_s  = split_s_prefix ++ "__" ++ show n ++ ".s"
		    let output_o = newdir real_odir 
					(basename ++ "__" ++ show n ++ ".o")
		    real_o <- osuf_ify output_o
		    run_something "Assembler" 
			    (unwords (as : as_opts
				      ++ [ "-c", "-o", real_o, input_s ]
			    ))
	
	mapM_ assemble_file [1..n]
	return True

-----------------------------------------------------------------------------
-- Linking

do_link :: [String] -> IO ()
do_link o_files = do
    ln <- readIORef pgm_l
    verb <- is_verbose
    o_file <- readIORef output_file
    let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

    pkg_lib_paths <- getPackageLibraryPath
    let pkg_lib_path_opts = map ("-L"++) pkg_lib_paths

    lib_paths <- readIORef library_paths
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_libs <- getPackageLibraries
    let pkg_lib_opts = map (\lib -> "-l"++lib) pkg_libs

    libs <- readIORef cmdline_libraries
    let lib_opts = map ("-l"++) (reverse libs)
	 -- reverse because they're added in reverse order from the cmd line

    pkg_extra_ld_opts <- getPackageExtraLdOpts

	-- probably _stub.o files
    extra_ld_inputs <- readIORef ld_inputs

	-- opts from -optl-<blah>
    extra_ld_opts <- getOpts opt_l

    run_something "Linker"
       (unwords 
	 ([ ln, verb, "-o", output_fn ]
	 ++ o_files
	 ++ extra_ld_inputs
	 ++ lib_path_opts
	 ++ lib_opts
	 ++ pkg_lib_path_opts
	 ++ pkg_lib_opts
	 ++ pkg_extra_ld_opts
	 ++ extra_ld_opts
	)
       )

-----------------------------------------------------------------------------
-- compatibility code

#if __GLASGOW_HASKELL__ <= 408
catchJust = catchIO
ioErrors  = justIoErrors
#endif

#ifdef mingw32_TARGET_OS
foreign import "_getpid" getProcessID :: IO Int 
#endif
