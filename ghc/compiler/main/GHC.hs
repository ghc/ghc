-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005
--
-- The GHC API
--
-- -----------------------------------------------------------------------------

module GHC (
	-- * Initialisation
	Session,
	defaultErrorHandler,
	defaultCleanupHandler,
	init,
	newSession,

	-- * Flags and settings
	DynFlags(..), DynFlag(..), GhcMode(..), HscTarget(..), dopt,
	parseDynamicFlags,
	getSessionDynFlags,
	setSessionDynFlags,
	setMsgHandler,

	-- * Targets
	Target(..),
	setTargets,
	getTargets,
	addTarget,
	guessTarget,
	
	-- * Loading/compiling the program
	depanal,
	load, SuccessFlag(..),		-- also does depanal
	workingDirectoryChanged,

	-- * Inspecting the module structure of the program
	ModuleGraph, ModSummary(..),
	getModuleGraph,
	topSortModuleGraph,

	-- * Interactive evaluation
	getBindings, getPrintUnqual,
#ifdef GHCI
	setContext, getContext,	
	moduleIsInterpreted,
	getInfo, GetInfoResult,
	exprType,
	typeKind,
	lookupName,
	RunResult(..),
	runStmt,
	browseModule,
	showModule,
	compileExpr, HValue,
#endif

	-- * Abstract syntax elements
	Module, mkModule, pprModule,
	Type, dropForAlls,
	Kind,
	Name, Id, TyCon, Class, DataCon,
	TyThing(..), 
	idType,

	-- used by DriverMkDepend:
	sessionHscEnv,
	cyclicModuleErr,
  ) where

{-
 ToDo:

  * return error messages rather than printing them.
  * inline bits of HscMain here to simplify layering: hscGetInfo,
    hscTcExpr, hscStmt.
  * implement second argument to load.
  * we need to expose DynFlags, so should parseDynamicFlags really be
    part of this interface?
  * what StaticFlags should we expose, if any?
-}

#include "HsVersions.h"

#ifdef GHCI
import qualified Linker
import Linker		( HValue, extendLinkEnv )
import NameEnv		( lookupNameEnv )
import TcRnDriver	( mkExportEnv, getModuleContents )
import RdrName		( GlobalRdrEnv, plusGlobalRdrEnv )
import HscMain		( hscGetInfo, GetInfoResult, 
			  hscStmt, hscTcExpr, hscKcType )
import Type		( tidyType )
import VarEnv		( emptyTidyEnv )
import GHC.Exts		( unsafeCoerce# )
import IfaceSyn		( IfaceDecl )
#endif

import Type		( Kind, Type, dropForAlls )
import Id		( Id, idType )
import TyCon		( TyCon )
import Class		( Class )
import DataCon		( DataCon )
import Name		( Name )
import NameEnv		( nameEnvElts )
import DriverPipeline	( preprocess, compile, CompResult(..), link )
import DriverPhases	( isHaskellSrcFilename )
import GetImports	( getImports )
import Packages		( isHomePackage )
import Finder
import HscMain		( newHscEnv )
import HscTypes
import DynFlags
import StaticFlags
import SysTools		( initSysTools, cleanTempFiles )
import Module
import FiniteMap
import Panic
import Digraph		( SCC(..), stronglyConnComp, flattenSCC, flattenSCCs )
import ErrUtils		( showPass )
import qualified ErrUtils
import Util
import StringBuffer	( hGetStringBuffer )
import Outputable
import SysTools		( cleanTempFilesExcept )
import BasicTypes	( SuccessFlag(..), succeeded )
import Maybes		( orElse, expectJust, mapCatMaybes )

import Directory        ( getModificationTime, doesFileExist )
import Maybe		( isJust, fromJust )
import List		( partition, nub )
import Monad		( unless, when, foldM )
import System		( exitWith, ExitCode(..) )
import EXCEPTION as Exception hiding (handle)
import DATA_IOREF
import IO
import Prelude hiding (init)

-- -----------------------------------------------------------------------------
-- Exception handlers

-- | Install some default exception handlers and run the inner computation.
-- Unless you want to handle exceptions yourself, you should wrap this around
-- the top level of your program.  The default handlers output the error
-- message(s) to stderr and exit cleanly.
defaultErrorHandler :: IO a -> IO a
defaultErrorHandler inner = 
  -- top-level exception handler: any unrecognised exception is a compiler bug.
  handle (\exception -> do
  	   hFlush stdout
	   case exception of
		-- an IO exception probably isn't our fault, so don't panic
		IOException _ ->  hPutStrLn stderr (show exception)
		AsyncException StackOverflow ->
			hPutStrLn stderr "stack overflow: use +RTS -K<size> to increase it"
		_other ->  hPutStr stderr (show (Panic (show exception)))
	   exitWith (ExitFailure 1)
         ) $

  -- all error messages are propagated as exceptions
  handleDyn (\dyn -> do
  		hFlush stdout
  		case dyn of
		     PhaseFailed _ code -> exitWith code
		     Interrupted -> exitWith (ExitFailure 1)
		     _ -> do hPutStrLn stderr (show (dyn :: GhcException))
			     exitWith (ExitFailure 1)
	    ) $
  inner

-- | Install a default cleanup handler to remove temporary files
-- deposited by a GHC run.  This is seperate from
-- 'defaultErrorHandler', because you might want to override the error
-- handling, but still get the ordinary cleanup behaviour.
defaultCleanupHandler :: DynFlags -> IO a -> IO a
defaultCleanupHandler dflags inner = 
   -- make sure we clean up after ourselves
   later (unless (dopt Opt_KeepTmpFiles dflags) $ 
	    cleanTempFiles dflags) 
	-- exceptions will be blocked while we clean the temporary files,
	-- so there shouldn't be any difficulty if we receive further
	-- signals.
   inner


-- | Initialises GHC.  This must be done /once/ only.  Takes the
-- command-line arguments.  All command-line arguments which aren't
-- understood by GHC will be returned.

init :: [String] -> IO [String]
init args = do
   -- catch ^C
   installSignalHandlers

   -- Grab the -B option if there is one
   let (minusB_args, argv1) = partition (prefixMatch "-B") args
   dflags0 <- initSysTools minusB_args defaultDynFlags
   writeIORef v_initDynFlags dflags0

   -- Parse the static flags
   argv2 <- parseStaticFlags argv1
   return argv2

GLOBAL_VAR(v_initDynFlags, error "initDynFlags", DynFlags)
	-- stores the DynFlags between the call to init and subsequent
	-- calls to newSession.

-- | Starts a new session.  A session consists of a set of loaded
-- modules, a set of options (DynFlags), and an interactive context.
-- ToDo: GhcMode should say "keep typechecked code" and/or "keep renamed
-- code".
newSession :: GhcMode -> IO Session
newSession mode = do
  dflags0 <- readIORef v_initDynFlags
  dflags <- initDynFlags dflags0
  env <- newHscEnv dflags{ ghcMode=mode }
  ref <- newIORef env
  return (Session ref)

-- tmp: this breaks the abstraction, but required because DriverMkDepend
-- needs to call the Finder.  ToDo: untangle this.
sessionHscEnv :: Session -> IO HscEnv
sessionHscEnv (Session ref) = readIORef ref

withSession :: Session -> (HscEnv -> IO a) -> IO a
withSession (Session ref) f = do h <- readIORef ref; f h

modifySession :: Session -> (HscEnv -> HscEnv) -> IO ()
modifySession (Session ref) f = do h <- readIORef ref; writeIORef ref $! f h

-- -----------------------------------------------------------------------------
-- Flags & settings

-- | Grabs the DynFlags from the Session
getSessionDynFlags :: Session -> IO DynFlags
getSessionDynFlags s = withSession s (return . hsc_dflags)

-- | Updates the DynFlags in a Session
setSessionDynFlags :: Session -> DynFlags -> IO ()
setSessionDynFlags s dflags = modifySession s (\h -> h{ hsc_dflags = dflags })

-- | Messages during compilation (eg. warnings and progress messages)
-- are reported using this callback.  By default, these messages are
-- printed to stderr.
setMsgHandler :: (String -> IO ()) -> IO ()
setMsgHandler = ErrUtils.setMsgHandler

-- -----------------------------------------------------------------------------
-- Targets

-- ToDo: think about relative vs. absolute file paths. And what
-- happens when the current directory changes.

-- | Sets the targets for this session.  Each target may be a module name
-- or a filename.  The targets correspond to the set of root modules for
-- the program/library.  Unloading the current program is achieved by
-- setting the current set of targets to be empty, followed by load.
setTargets :: Session -> [Target] -> IO ()
setTargets s targets = modifySession s (\h -> h{ hsc_targets = targets })

-- | returns the current set of targets
getTargets :: Session -> IO [Target]
getTargets s = withSession s (return . hsc_targets)

-- Add another target, or update an existing target with new content.
addTarget :: Session -> Target -> IO ()
addTarget s target
  = modifySession s (\h -> h{ hsc_targets = target : hsc_targets h })

-- Remove a target
-- removeTarget :: Session -> Module -> IO ()

-- Attempts to guess what Target a string refers to.  This function implements
-- the --make/GHCi command-line syntax for filenames: 
--
-- 	- if the string looks like a Haskell source filename, then interpret
--	  it as such
--	- if adding a .hs or .lhs suffix yields the name of an existing file,
--	  then use that
-- 	- otherwise interpret the string as a module name
--
guessTarget :: String -> IO Target
guessTarget file
   | isHaskellSrcFilename file
   = return (Target (TargetFile file) Nothing)
   | otherwise
   = do exists <- doesFileExist hs_file
	if exists then return (Target (TargetFile hs_file) Nothing) else do
	exists <- doesFileExist lhs_file
	if exists then return (Target (TargetFile lhs_file) Nothing) else do
	return (Target (TargetModule (mkModule file)) Nothing)
     where 
	 hs_file = file ++ ".hs"
	 lhs_file = file ++ ".lhs"

-- -----------------------------------------------------------------------------
-- Loading the program

-- | The result of load.
data LoadResult
  = LoadOk	Errors	-- ^ all specified targets were loaded successfully.
  | LoadFailed  Errors	-- ^ not all modules were loaded.

type Errors = [String]

{-
data ErrMsg = ErrMsg { 
	errMsgSeverity  :: Severity,  -- warning, error, etc.
	errMsgSpans     :: [SrcSpan],
	errMsgShortDoc  :: Doc,
	errMsgExtraInfo :: Doc
	}
-}

-- Perform a dependency analysis starting from the current targets
-- and update the session with the new module graph.
depanal :: Session -> [Module] -> IO ()
depanal (Session ref) excluded_mods = do
  hsc_env <- readIORef ref
  let
	 dflags  = hsc_dflags hsc_env
	 gmode   = ghcMode (hsc_dflags hsc_env)
	 targets = hsc_targets hsc_env
	 old_graph = hsc_mod_graph hsc_env
	
  showPass dflags "Chasing dependencies"
  when (verbosity dflags >= 1 && gmode == BatchCompile) $
	       hPutStrLn stderr (showSDoc (hcat [
		     text "Chasing modules from: ",
	     		hcat (punctuate comma (map pprTarget targets))]))

  graph <- downsweep hsc_env old_graph excluded_mods
  writeIORef ref hsc_env{ hsc_mod_graph=graph }


-- | Try to load the program.  If a Module is supplied, then just
-- attempt to load up to this target.  If no Module is supplied,
-- then try to load all targets.
load :: Session -> Maybe Module -> IO SuccessFlag
load s@(Session ref) maybe_mod{-ToDo-} 
   = do 
	-- dependency analysis first
	depanal s []

	hsc_env <- readIORef ref

        let hpt1      = hsc_HPT hsc_env
        let dflags    = hsc_dflags hsc_env
	let mod_graph = hsc_mod_graph hsc_env

        let ghci_mode = ghcMode (hsc_dflags hsc_env) -- this never changes
        let verb      = verbosity dflags

	-- The "bad" boot modules are the ones for which we have
	-- B.hs-boot in the module graph, but no B.hs
	-- The downsweep should have ensured this does not happen
	-- (see msDeps)
        let all_home_mods = [ms_mod s | s <- mod_graph, not (isBootSummary s)]
	    bad_boot_mods = [s 	      | s <- mod_graph, isBootSummary s,
					not (ms_mod s `elem` all_home_mods)]
	ASSERT( null bad_boot_mods ) return ()

        -- Topologically sort the module graph
        -- mg2 should be cycle free; but it includes hi-boot ModSummary nodes
        let mg2 :: [SCC ModSummary]
	    mg2 = topSortModuleGraph False mod_graph

        -- mg2_with_srcimps drops the hi-boot nodes, returning a 
	-- graph with cycles.  Among other things, it is used for
        -- backing out partially complete cycles following a failed
        -- upsweep, and for removing from hpt all the modules
        -- not in strict downwards closure, during calls to compile.
        let mg2_with_srcimps :: [SCC ModSummary]
	    mg2_with_srcimps = topSortModuleGraph True mod_graph

	-- Sort out which linkables we wish to keep in the unlinked image.
	-- See getValidLinkables below for details.
	(valid_old_linkables, new_linkables)
	    <- getValidLinkables ghci_mode (hptLinkables hpt1)
		  all_home_mods mg2_with_srcimps

	-- putStrLn (showSDoc (vcat [ppr valid_old_linkables, ppr new_linkables]))

	-- The new_linkables are .o files we found on the disk, presumably
	-- as a result of a GHC run "on the side".  So we'd better forget
	-- everything we know abouut those modules!
	let old_hpt = delModuleEnvList hpt1 (map linkableModule new_linkables)

	-- When (verb >= 2) $
        --    putStrLn (showSDoc (text "Valid linkables:" 
        -- 			 <+> ppr valid_linkables))

        -- Figure out a stable set of modules which can be retained
        -- the top level envs, to avoid upsweeping them.  Goes to a
        -- bit of trouble to avoid upsweeping module cycles.
        --
        -- Construct a set S of stable modules like this:
        -- Travel upwards, over the sccified graph.  For each scc
        -- of modules ms, add ms to S only if:
        -- 1.  All home imports of ms are either in ms or S
        -- 2.  A valid old linkable exists for each module in ms

	-- mg2_with_srcimps has no hi-boot nodes, 
	-- and hence neither does stable_mods 
        stable_summaries <- preUpsweep valid_old_linkables
		 		       all_home_mods [] mg2_with_srcimps
        let stable_mods      = map ms_mod stable_summaries
	    stable_linkables = filter (\m -> linkableModule m `elem` stable_mods) 
				      valid_old_linkables

	    stable_hpt = filterModuleEnv is_stable_hm hpt1
	    is_stable_hm hm_info = mi_module (hm_iface hm_info) `elem` stable_mods

            upsweep_these
               = filter (\scc -> any (`notElem` stable_mods) 
                                     (map ms_mod (flattenSCC scc)))
                        mg2

        when (verb >= 2) $
           hPutStrLn stderr (showSDoc (text "Stable modules:" 
                               <+> sep (map (text.moduleUserString) stable_mods)))

	-- Unload any modules which are going to be re-linked this time around.
	unload hsc_env stable_linkables

	-- We can now glom together our linkable sets
	let valid_linkables = valid_old_linkables ++ new_linkables

        -- We could at this point detect cycles which aren't broken by
        -- a source-import, and complain immediately, but it seems better
        -- to let upsweep_mods do this, so at least some useful work gets
        -- done before the upsweep is abandoned.
        --hPutStrLn stderr "after tsort:\n"
        --hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

        -- Now do the upsweep, calling compile for each module in
        -- turn.  Final result is version 3 of everything.

	-- clean up between compilations
	let cleanup = cleanTempFilesExcept dflags
			  (ppFilesFromSummaries (flattenSCCs mg2))

        (upsweep_ok, hsc_env3, modsUpswept)
           <- upsweep_mods (hsc_env { hsc_HPT = stable_hpt })
			   (old_hpt, valid_linkables)
                           cleanup upsweep_these

        -- At this point, modsUpswept and newLis should have the same
        -- length, so there is one new (or old) linkable for each 
        -- mod which was processed (passed to compile).

	-- Make modsDone be the summaries for each home module now
	-- available; this should equal the domain of hpt3.
	-- (NOT STRICTLY TRUE if an interactive session was started
	--  with some object on disk ???)
        -- Get in in a roughly top .. bottom order (hence reverse).

        let modsDone = reverse modsUpswept ++ stable_summaries

        -- Try and do linking in some form, depending on whether the
        -- upsweep was completely or only partially successful.

        if succeeded upsweep_ok

         then 
           -- Easy; just relink it all.
           do when (verb >= 2) $ 
		 hPutStrLn stderr "Upsweep completely successful."

	      -- Clean up after ourselves
	      cleanTempFilesExcept dflags (ppFilesFromSummaries modsDone)

	      -- Issue a warning for the confusing case where the user
	      -- said '-o foo' but we're not going to do any linking.
	      -- We attempt linking if either (a) one of the modules is
	      -- called Main, or (b) the user said -no-hs-main, indicating
	      -- that main() is going to come from somewhere else.
	      --
	      let ofile = outputFile dflags
	      let no_hs_main = dopt Opt_NoHsMain dflags
	      let mb_main_mod = mainModIs dflags
	      let 
	 	main_mod = mb_main_mod `orElse` "Main"
		a_root_is_Main 
               	    = any ((==main_mod).moduleUserString.ms_mod) 
                    	  mod_graph
		do_linking = a_root_is_Main || no_hs_main

	      when (ghci_mode == BatchCompile && isJust ofile && not do_linking
		     && verb > 0) $
	         hPutStrLn stderr ("Warning: output was redirected with -o, " ++
				   "but no output will be generated\n" ++
				   "because there is no " ++ main_mod ++ " module.")

	      -- link everything together
              linkresult <- link ghci_mode dflags do_linking (hsc_HPT hsc_env3)

	      let hsc_env4 = hsc_env3{ hsc_mod_graph = modsDone }
	      loadFinish Succeeded linkresult ref hsc_env4

         else 
           -- Tricky.  We need to back out the effects of compiling any
           -- half-done cycles, both so as to clean up the top level envs
           -- and to avoid telling the interactive linker to link them.
           do when (verb >= 2) $
		hPutStrLn stderr "Upsweep partially successful."

              let modsDone_names
                     = map ms_mod modsDone
              let mods_to_zap_names 
                     = findPartiallyCompletedCycles modsDone_names 
			  mg2_with_srcimps
              let mods_to_keep
                     = filter ((`notElem` mods_to_zap_names).ms_mod) 
			  modsDone

              let hpt4 = retainInTopLevelEnvs (map ms_mod mods_to_keep) 
					      (hsc_HPT hsc_env3)

	      -- Clean up after ourselves
	      cleanTempFilesExcept dflags (ppFilesFromSummaries mods_to_keep)

	      -- Link everything together
              linkresult <- link ghci_mode dflags False hpt4

	      let hsc_env4 = hsc_env3{ hsc_mod_graph = mods_to_keep,
				       hsc_HPT = hpt4 }
	      loadFinish Failed linkresult ref hsc_env4

-- Finish up after a load.

-- If the link failed, unload everything and return.
loadFinish all_ok Failed ref hsc_env
  = do unload hsc_env []
       writeIORef ref $! discardProg hsc_env
       return Failed

-- Empty the interactive context and set the module context to the topmost
-- newly loaded module, or the Prelude if none were loaded.
loadFinish all_ok Succeeded ref hsc_env
  = do writeIORef ref $! hsc_env{ hsc_IC = emptyInteractiveContext }
       return all_ok

-- Forget the current program, but retain the persistent info in HscEnv
discardProg :: HscEnv -> HscEnv
discardProg hsc_env
  = hsc_env { hsc_mod_graph = emptyMG, 
	      hsc_IC = emptyInteractiveContext,
	      hsc_HPT = emptyHomePackageTable }

-- used to fish out the preprocess output files for the purposes of
-- cleaning up.  The preprocessed file *might* be the same as the
-- source file, but that doesn't do any harm.
ppFilesFromSummaries summaries = [ fn | Just fn <- map ms_hspp_file summaries ]

-----------------------------------------------------------------------------
-- Unloading

unload :: HscEnv -> [Linkable] -> IO ()
unload hsc_env stable_linkables	-- Unload everthing *except* 'stable_linkables'
  = case ghcMode (hsc_dflags hsc_env) of
	BatchCompile -> return ()
#ifdef GHCI
	Interactive -> Linker.unload (hsc_dflags hsc_env) stable_linkables
#else
	Interactive -> panic "unload: no interpreter"
#endif
	other -> panic "unload: strange mode"
    
-----------------------------------------------------------------------------
-- getValidLinkables

-- For each module (or SCC of modules), we take:
--
--	- an on-disk linkable, if this is the first time around and one
--	  is available.
--
--	- the old linkable, otherwise (and if one is available).
--
-- and we throw away the linkable if it is older than the source file.
-- In interactive mode, we also ignore the on-disk linkables unless
-- all of the dependents of this SCC also have on-disk linkables (we
-- can't have dynamically loaded objects that depend on interpreted
-- modules in GHCi).
--
-- If a module has a valid linkable, then it may be STABLE (see below),
-- and it is classified as SOURCE UNCHANGED for the purposes of calling
-- compile.
--
-- ToDo: this pass could be merged with the preUpsweep.
-- 
-- ****************
-- CAREFUL!  This pass operates on the cyclic version of
-- the module graph (topSortModuleGraph True), whereas the upsweep operates on
-- the non-cyclic (topSortModuleGraph False) version of the graph.
-- ****************

getValidLinkables
	:: GhcMode
	-> [Linkable]		-- old linkables
	-> [Module]		-- all home modules
	-> [SCC ModSummary]	-- all modules in the program, dependency order
	-> IO ( [Linkable],	-- still-valid linkables 
		[Linkable] 	-- new linkables we just found on the disk
				-- presumably generated by separate run of ghc
	      )

getValidLinkables mode old_linkables all_home_mods module_graph
  = do	{ 	-- Process the SCCs in bottom-to-top order
		-- (foldM works left-to-right)
	  ls <- foldM (getValidLinkablesSCC mode old_linkables all_home_mods) 
	  	      [] module_graph
	; return (partition_it ls [] []) }
 where
  partition_it []         valid new = (valid,new)
  partition_it ((l,b):ls) valid new 
	| b         = partition_it ls valid (l:new)
	| otherwise = partition_it ls (l:valid) new


getValidLinkablesSCC
	:: GhcMode
	-> [Linkable]		-- old linkables
	-> [Module]		-- all home modules
	-> [(Linkable,Bool)]
	-> SCC ModSummary
	-> IO [(Linkable,Bool)]

getValidLinkablesSCC mode old_linkables all_home_mods new_linkables scc0
   = let 
	  scc             = flattenSCC scc0
          scc_names       = map ms_mod scc
	  home_module m   = m `elem` all_home_mods && m `notElem` scc_names
          scc_allhomeimps = nub (filter home_module (concatMap ms_imps scc))
		-- NB. ms_imps, not ms_allimps above.  We don't want to
		-- force a module's SOURCE imports to be already compiled for
		-- its object linkable to be valid.

		-- The new_linkables is only the *valid* linkables below here
	  has_object m = case findModuleLinkable_maybe (map fst new_linkables) m of
			    Nothing -> False
			    Just l  -> isObjectLinkable l

          objects_allowed = mode == BatchCompile || all has_object scc_allhomeimps
     in do

     new_linkables'
	<- foldM (getValidLinkable old_linkables objects_allowed) [] scc

	-- since an scc can contain only all objects or no objects at all,
	-- we have to check whether we got all objects or not, and re-do
	-- the linkable check if not.
     new_linkables' <- 
        if objects_allowed
	     && not (all isObjectLinkable (map fst new_linkables'))
	  then foldM (getValidLinkable old_linkables False) [] scc
	  else return new_linkables'

     return (new_linkables ++ new_linkables')


getValidLinkable :: [Linkable] -> Bool -> [(Linkable,Bool)] -> ModSummary 
	-> IO [(Linkable,Bool)]
	-- True <=> linkable is new; i.e. freshly discovered on the disk
	--				  presumably generated 'on the side'
	--				  by a separate GHC run
getValidLinkable old_linkables objects_allowed new_linkables summary 
	-- 'objects_allowed' says whether we permit this module to
	-- have a .o-file linkable.  We only permit it if all the
	-- modules it depends on also have .o files; a .o file can't
	-- link to a bytecode module
   = do let mod_name = ms_mod summary

	maybe_disk_linkable
          <- if (not objects_allowed)
		then return Nothing

		else findLinkable mod_name (ms_location summary)

	let old_linkable = findModuleLinkable_maybe old_linkables mod_name

	    new_linkables' = 
	     case (old_linkable, maybe_disk_linkable) of
		(Nothing, Nothing)			-> []

		-- new object linkable just appeared
		(Nothing, Just l)			-> up_to_date l True

		(Just l,  Nothing)
		  | isObjectLinkable l			-> []
		    -- object linkable disappeared!  In case we need to
		    -- relink the module, disregard the old linkable and
		    -- just interpret the module from now on.
		  | otherwise				-> up_to_date l False
		    -- old byte code linkable

		(Just l, Just l') 
		  | not (isObjectLinkable l)		-> up_to_date l  False
		    -- if the previous linkable was interpreted, then we
		    -- ignore a newly compiled version, because the version
		    -- numbers in the interface file will be out-of-sync with
		    -- our internal ones.
		  | linkableTime l' >  linkableTime l   -> up_to_date l' True
		  | linkableTime l' == linkableTime l   -> up_to_date l  False
		  | otherwise			        -> []
		    -- on-disk linkable has been replaced by an older one!
		    -- again, disregard the previous one.

	    up_to_date l b
		| linkableTime l < ms_hs_date summary = []
		| otherwise = [(l,b)]
		-- why '<' rather than '<=' above?  If the filesystem stores
		-- times to the nearset second, we may occasionally find that
		-- the object & source have the same modification time, 
		-- especially if the source was automatically generated
		-- and compiled.  Using >= is slightly unsafe, but it matches
		-- make's behaviour.

	return (new_linkables' ++ new_linkables)


hptLinkables :: HomePackageTable -> [Linkable]
-- Get all the linkables from the home package table, one for each module
-- Once the HPT is up to date, these are the ones we should link
hptLinkables hpt = map hm_linkable (moduleEnvElts hpt)


-----------------------------------------------------------------------------
-- Do a pre-upsweep without use of "compile", to establish a 
-- (downward-closed) set of stable modules for which we won't call compile.

-- a stable module:
--	* has a valid linkable (see getValidLinkables above)
--	* depends only on stable modules
--	* has an interface in the HPT (interactive mode only)

preUpsweep :: [Linkable]	-- new valid linkables
           -> [Module]		-- names of all mods encountered in downsweep
           -> [ModSummary]	-- accumulating stable modules
           -> [SCC ModSummary]  -- scc-ified mod graph, including src imps
           -> IO [ModSummary]	-- stable modules

preUpsweep valid_lis all_home_mods stable []  = return stable
preUpsweep valid_lis all_home_mods stable (scc0:sccs)
   = do let scc = flattenSCC scc0
            scc_allhomeimps :: [Module]
            scc_allhomeimps 
               = nub (filter (`elem` all_home_mods) (concatMap ms_allimps scc))
            all_imports_in_scc_or_stable
               = all in_stable_or_scc scc_allhomeimps
	    scc_mods     = map ms_mod scc
            stable_names = scc_mods ++ map ms_mod stable
            in_stable_or_scc m = m `elem` stable_names

	    -- now we check for valid linkables: each module in the SCC must 
	    -- have a valid linkable (see getValidLinkables above).
	    has_valid_linkable scc_mod
   	      = isJust (findModuleLinkable_maybe valid_lis scc_mod)

	    scc_is_stable = all_imports_in_scc_or_stable
			  && all has_valid_linkable scc_mods

        if scc_is_stable
         then preUpsweep valid_lis all_home_mods (scc ++ stable) sccs
         else preUpsweep valid_lis all_home_mods stable 	 sccs

ms_allimps :: ModSummary -> [Module]
ms_allimps ms = ms_srcimps ms ++ ms_imps ms

-- Return (names of) all those in modsDone who are part of a cycle
-- as defined by theGraph.
findPartiallyCompletedCycles :: [Module] -> [SCC ModSummary] -> [Module]
findPartiallyCompletedCycles modsDone theGraph
   = chew theGraph
     where
        chew [] = []
        chew ((AcyclicSCC v):rest) = chew rest    -- acyclic?  not interesting.
        chew ((CyclicSCC vs):rest)
           = let names_in_this_cycle = nub (map ms_mod vs)
                 mods_in_this_cycle  
                    = nub ([done | done <- modsDone, 
                                   done `elem` names_in_this_cycle])
                 chewed_rest = chew rest
             in 
             if   notNull mods_in_this_cycle
                  && length mods_in_this_cycle < length names_in_this_cycle
             then mods_in_this_cycle ++ chewed_rest
             else chewed_rest


-- Compile multiple modules, stopping as soon as an error appears.
-- There better had not be any cyclic groups here -- we check for them.
upsweep_mods :: HscEnv				-- Includes initially-empty HPT
             -> (HomePackageTable, [Linkable])	-- HPT and valid linkables from last time round
	     -> IO ()				-- How to clean up unwanted tmp files
             -> [SCC ModSummary]		-- Mods to do (the worklist)
             -> IO (SuccessFlag,
                    HscEnv,		-- With an updated HPT
                    [ModSummary])	-- Mods which succeeded

upsweep_mods hsc_env oldUI cleanup
     []
   = return (Succeeded, hsc_env, [])

upsweep_mods hsc_env oldUI cleanup
     (CyclicSCC ms:_)
   = do hPutStrLn stderr (showSDoc (cyclicModuleErr ms))
        return (Failed, hsc_env, [])

upsweep_mods hsc_env oldUI@(old_hpt, old_linkables) cleanup
     (AcyclicSCC mod:mods)
   = do -- putStrLn ("UPSWEEP_MOD: hpt = " ++ 
	--	     show (map (moduleUserString.moduleName.mi_module.hm_iface) 
	--		       (moduleEnvElts (hsc_HPT hsc_env)))

        mb_mod_info <- upsweep_mod hsc_env oldUI mod 

	cleanup		-- Remove unwanted tmp files between compilations

        case mb_mod_info of
	    Nothing -> return (Failed, hsc_env, [])
	    Just mod_info -> do 
		{ let this_mod = ms_mod mod

			-- Add new info to hsc_env
		      hpt1     = extendModuleEnv (hsc_HPT hsc_env) this_mod mod_info
		      hsc_env1 = hsc_env { hsc_HPT = hpt1 }
			-- Space-saving: delete the old HPT entry and
			-- linkable for mod BUT if mod is a hs-boot
			-- node, don't delete it For the linkable this
			-- is dead right: the linkable relates only to
			-- the main Haskell source file.  For the
			-- interface, the HPT entry is probaby for the
			-- main Haskell source file.  Deleting it
			-- would force .. (what?? --SDM)
		      oldUI1 | isBootSummary mod = oldUI
			     | otherwise
			     = (delModuleEnv old_hpt this_mod, 
				  delModuleLinkable old_linkables this_mod)

		; (restOK, hsc_env2, modOKs) <- upsweep_mods hsc_env1 oldUI1 cleanup mods
		; return (restOK, hsc_env2, mod:modOKs) }


-- Compile a single module.  Always produce a Linkable for it if 
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: HscEnv
            -> (HomePackageTable, UnlinkedImage)
            -> ModSummary
            -> IO (Maybe HomeModInfo)	-- Nothing => Failed

upsweep_mod hsc_env (old_hpt, old_linkables) summary
   = do 
        let this_mod = ms_mod summary

	-- The old interface is ok if it's in the old HPT 
	--	a) we're compiling a source file, and the old HPT
	--	entry is for a source file
	--	b) we're compiling a hs-boot file
	-- Case (b) allows an hs-boot file to get the interface of its
	-- real source file on the second iteration of the compilation
	-- manager, but that does no harm.  Otherwise the hs-boot file
	-- will always be recompiled

            mb_old_iface 
		= case lookupModuleEnv old_hpt this_mod of
		     Nothing	 			  -> Nothing
		     Just hm_info | isBootSummary summary -> Just iface
				  | not (mi_boot iface)   -> Just iface
				  | otherwise		  -> Nothing
				   where 
				     iface = hm_iface hm_info

            maybe_old_linkable = findModuleLinkable_maybe old_linkables this_mod
            source_unchanged   = isJust maybe_old_linkable

            old_linkable = expectJust "upsweep_mod:old_linkable" maybe_old_linkable

	    have_object 
	       | Just l <- maybe_old_linkable, isObjectLinkable l = True
	       | otherwise = False

        compresult <- compile hsc_env summary source_unchanged have_object mb_old_iface

        case compresult of

           -- Compilation "succeeded", and may or may not have returned a new
           -- linkable (depending on whether compilation was actually performed
	   -- or not).
           CompOK new_details new_iface maybe_new_linkable
              -> do let 
			new_linkable = maybe_new_linkable `orElse` old_linkable
			new_info = HomeModInfo { hm_iface = new_iface,
						 hm_details = new_details,
						 hm_linkable = new_linkable }
                    return (Just new_info)

           -- Compilation failed.  Compile may still have updated the PCS, tho.
           CompErrs -> return Nothing

-- Filter modules in the HPT
retainInTopLevelEnvs :: [Module] -> HomePackageTable -> HomePackageTable
retainInTopLevelEnvs keep_these hpt
   = mkModuleEnv [ (mod, fromJust mb_mod_info)
		 | mod <- keep_these
		 , let mb_mod_info = lookupModuleEnv hpt mod
		 , isJust mb_mod_info ]

-- ---------------------------------------------------------------------------
-- Topological sort of the module graph

topSortModuleGraph
	  :: Bool 		-- Drop hi-boot nodes? (see below)
	  -> [ModSummary]
	  -> [SCC ModSummary]
-- Calculate SCCs of the module graph, possibly dropping the hi-boot nodes
--
-- Drop hi-boot nodes (first boolean arg)? 
--
--   False:	treat the hi-boot summaries as nodes of the graph,
--		so the graph must be acyclic
--
--   True:	eliminate the hi-boot nodes, and instead pretend
--		the a source-import of Foo is an import of Foo
--		The resulting graph has no hi-boot nodes, but can by cyclic

topSortModuleGraph drop_hs_boot_nodes summaries
   = stronglyConnComp nodes
   where
	-- Drop hs-boot nodes by using HsSrcFile as the key
	hs_boot_key | drop_hs_boot_nodes = HsSrcFile
		    | otherwise		 = HsBootFile	

	-- We use integers as the keys for the SCC algorithm
	nodes :: [(ModSummary, Int, [Int])]	
	nodes = [(s, fromJust (lookup_key (ms_hsc_src s) (ms_mod s)), 
		     out_edge_keys hs_boot_key (ms_srcimps s) ++
		     out_edge_keys HsSrcFile   (ms_imps s)    )
		| s <- summaries
		, not (isBootSummary s && drop_hs_boot_nodes) ]
		-- Drop the hi-boot ones if told to do so

	key_map :: NodeMap Int
	key_map = listToFM ([(ms_mod s, ms_hsc_src s) | s <- summaries]
			   `zip` [1..])

	lookup_key :: HscSource -> Module -> Maybe Int
	lookup_key hs_src mod = lookupFM key_map (mod, hs_src)

	out_edge_keys :: HscSource -> [Module] -> [Int]
        out_edge_keys hi_boot ms = mapCatMaybes (lookup_key hi_boot) ms
		-- If we want keep_hi_boot_nodes, then we do lookup_key with
		-- the IsBootInterface parameter True; else False


type NodeKey   = (Module, HscSource)	  -- The nodes of the graph are 
type NodeMap a = FiniteMap NodeKey a	  -- keyed by (mod, src_file_type) pairs

msKey :: ModSummary -> NodeKey
msKey (ModSummary { ms_mod = mod, ms_hsc_src = boot }) = (mod,boot)

emptyNodeMap :: NodeMap a
emptyNodeMap = emptyFM

mkNodeMap :: [ModSummary] -> NodeMap ModSummary
mkNodeMap summaries = listToFM [ (msKey s, s) | s <- summaries]
	
nodeMapElts :: NodeMap a -> [a]
nodeMapElts = eltsFM

-- -----------------------------------------------------------------
-- The unlinked image
-- 
-- The compilation manager keeps a list of compiled, but as-yet unlinked
-- binaries (byte code or object code).  Even when it links bytecode
-- it keeps the unlinked version so it can re-link it later without
-- recompiling.

type UnlinkedImage = [Linkable]	-- the unlinked images (should be a set, really)

findModuleLinkable_maybe :: [Linkable] -> Module -> Maybe Linkable
findModuleLinkable_maybe lis mod
   = case [LM time nm us | LM time nm us <- lis, nm == mod] of
        []   -> Nothing
        [li] -> Just li
        many -> pprPanic "findModuleLinkable" (ppr mod)

delModuleLinkable :: [Linkable] -> Module -> [Linkable]
delModuleLinkable ls mod = [ l | l@(LM _ nm _) <- ls, nm /= mod ]

-----------------------------------------------------------------------------
-- Downsweep (dependency analysis)

-- Chase downwards from the specified root set, returning summaries
-- for all home modules encountered.  Only follow source-import
-- links.

-- We pass in the previous collection of summaries, which is used as a
-- cache to avoid recalculating a module summary if the source is
-- unchanged.
--
-- The returned list of [ModSummary] nodes has one node for each home-package
-- module, plus one for any hs-boot files.  The imports of these nodes 
-- are all there, including the imports of non-home-package modules.

downsweep :: HscEnv
	  -> [ModSummary]	-- Old summaries
	  -> [Module]		-- Ignore dependencies on these; treat them as
				-- if they were package modules
	  -> IO [ModSummary]
downsweep hsc_env old_summaries excl_mods
   = do rootSummaries <- mapM getRootSummary roots
	checkDuplicates rootSummaries
        loop (concatMap msDeps rootSummaries) 
	     (mkNodeMap rootSummaries)
     where
	dflags = hsc_dflags hsc_env
	roots = hsc_targets hsc_env

	old_summary_map :: NodeMap ModSummary
	old_summary_map = mkNodeMap old_summaries

	getRootSummary :: Target -> IO ModSummary
	getRootSummary (Target (TargetFile file) maybe_buf)
	   = do exists <- doesFileExist file
		if exists then summariseFile hsc_env file else do
		throwDyn (CmdLineError ("can't find file: " ++ file))	
	getRootSummary (Target (TargetModule modl) maybe_buf)
 	   = do maybe_summary <- summarise hsc_env emptyNodeMap Nothing False 
					   modl excl_mods
		case maybe_summary of
		   Nothing -> packageModErr modl
		   Just s  -> return s

	-- In a root module, the filename is allowed to diverge from the module
	-- name, so we have to check that there aren't multiple root files
	-- defining the same module (otherwise the duplicates will be silently
 	-- ignored, leading to confusing behaviour).
	checkDuplicates :: [ModSummary] -> IO ()
	checkDuplicates summaries = mapM_ check summaries
  	  where check summ = 
		  case dups of
			[]     -> return ()
			[_one] -> return ()
			many   -> multiRootsErr modl many
		   where modl = ms_mod summ
			 dups = 
			   [ fromJust (ml_hs_file (ms_location summ'))
			   | summ' <- summaries, ms_mod summ' == modl ]

	loop :: [(FilePath,Module,IsBootInterface)]
			-- Work list: process these modules
	     -> NodeMap ModSummary
		 	-- Visited set
	     -> IO [ModSummary]
			-- The result includes the worklist, except
			-- for those mentioned in the visited set
	loop [] done 	  = return (nodeMapElts done)
	loop ((cur_path, wanted_mod, is_boot) : ss) done 
	  | key `elemFM` done = loop ss done
	  | otherwise	      = do { mb_s <- summarise hsc_env old_summary_map 
						 (Just cur_path) is_boot 
						 wanted_mod excl_mods
				   ; case mb_s of
					Nothing -> loop ss done
					Just s  -> loop (msDeps s ++ ss) 
							(addToFM done key s) }
	  where
	    key = (wanted_mod, if is_boot then HsBootFile else HsSrcFile)

msDeps :: ModSummary -> [(FilePath, 		-- Importing module
			  Module,	 	-- Imported module
			  IsBootInterface)]	 -- {-# SOURCE #-} import or not
-- (msDeps s) returns the dependencies of the ModSummary s.
-- A wrinkle is that for a {-# SOURCE #-} import we return
--	*both* the hs-boot file
--	*and* the source file
-- as "dependencies".  That ensures that the list of all relevant
-- modules always contains B.hs if it contains B.hs-boot.
-- Remember, this pass isn't doing the topological sort.  It's
-- just gathering the list of all relevant ModSummaries
msDeps s =  concat [ [(f, m, True), (f,m,False)] | m <- ms_srcimps s] 
	 ++ [(f,m,False) | m <- ms_imps    s] 
	where
	  f = msHsFilePath s	-- Keep the importing module for error reporting


-----------------------------------------------------------------------------
-- Summarising modules

-- We have two types of summarisation:
--
--    * Summarise a file.  This is used for the root module(s) passed to
--	cmLoadModules.  The file is read, and used to determine the root
--	module name.  The module name may differ from the filename.
--
--    * Summarise a module.  We are given a module name, and must provide
--	a summary.  The finder is used to locate the file in which the module
--	resides.

summariseFile :: HscEnv -> FilePath -> IO ModSummary
-- Used for Haskell source only, I think
-- We know the file name, and we know it exists,
-- but we don't necessarily know the module name (might differ)
summariseFile hsc_env file
   = do let dflags = hsc_dflags hsc_env

	(dflags', hspp_fn) <- preprocess dflags file
		-- The dflags' contains the OPTIONS pragmas

	-- Read the file into a buffer.  We're going to cache
	-- this buffer in the ModLocation (ml_hspp_buf) so that it
	-- doesn't have to be slurped again when hscMain parses the
	-- file later.
	buf <- hGetStringBuffer hspp_fn
        (srcimps,the_imps,mod) <- getImports dflags' buf hspp_fn

	-- Make a ModLocation for this file
	location <- mkHomeModLocation dflags mod file

	-- Tell the Finder cache where it is, so that subsequent calls
	-- to findModule will find it, even if it's not on any search path
	addHomeModuleToFinder hsc_env mod location

        src_timestamp <- getModificationTime file
        return (ModSummary { ms_mod = mod, ms_hsc_src = HsSrcFile,
			     ms_location = location,
                             ms_hspp_file = Just hspp_fn,
			     ms_hspp_buf  = Just buf,
                             ms_srcimps = srcimps, ms_imps = the_imps,
			     ms_hs_date = src_timestamp })

-- Summarise a module, and pick up source and timestamp.
summarise :: HscEnv
	  -> NodeMap ModSummary	-- Map of old summaries
	  -> Maybe FilePath	-- Importing module (for error messages)
	  -> IsBootInterface	-- True <=> a {-# SOURCE #-} import
	  -> Module 		-- Imported module to be summarised
	  -> [Module]		-- Modules to exclude
	  -> IO (Maybe ModSummary)	-- Its new summary

summarise hsc_env old_summary_map cur_mod is_boot wanted_mod excl_mods
  | wanted_mod `elem` excl_mods
  = return Nothing

  | Just old_summary <- lookupFM old_summary_map (wanted_mod, hsc_src)
  = do	{ 	-- Find its new timestamp; all the 
		-- ModSummaries in the old map have valid ml_hs_files
	   let location = ms_location old_summary
	       src_fn = fromJust (ml_hs_file location)

	;  src_timestamp <- getModificationTime src_fn

		-- return the cached summary if the source didn't change
	; if ms_hs_date old_summary == src_timestamp 
	  then return (Just old_summary)
	  else new_summary location
	}

  | otherwise
  = do	{ found <- findModule hsc_env wanted_mod True {-explicit-}
	; case found of
	     Found location pkg 
		| not (isHomePackage pkg)      -> return Nothing
			-- Drop external-pkg
		| isJust (ml_hs_file location) -> new_summary location
			-- Home package
	     err -> noModError dflags cur_mod wanted_mod err
			-- Not found
	}
  where
    dflags = hsc_dflags hsc_env

    hsc_src = if is_boot then HsBootFile else HsSrcFile

    new_summary location
      = do { 	-- Adjust location to point to the hs-boot source file, 
		-- hi file, object file, when is_boot says so
	  let location' | is_boot   = addBootSuffixLocn location
			| otherwise = location
	      src_fn = fromJust (ml_hs_file location')

		-- Check that it exists
		-- It might have been deleted since the Finder last found it
	; exists <- doesFileExist src_fn
	; if exists then return () else noHsFileErr cur_mod src_fn

	-- Preprocess the source file and get its imports
	-- The dflags' contains the OPTIONS pragmas
	; (dflags', hspp_fn) <- preprocess dflags src_fn
	; buf <- hGetStringBuffer hspp_fn
        ; (srcimps, the_imps, mod_name) <- getImports dflags' buf hspp_fn

	; when (mod_name /= wanted_mod) $
		throwDyn (ProgramError 
		   (showSDoc (text src_fn
			      <>  text ": file name does not match module name"
			      <+> quotes (ppr mod_name))))

		-- Find its timestamp, and return the summary
        ; src_timestamp <- getModificationTime src_fn
	; return (Just ( ModSummary { ms_mod       = wanted_mod, 
				      ms_hsc_src   = hsc_src,
				      ms_location  = location',
				      ms_hspp_file = Just hspp_fn,
				      ms_hspp_buf  = Just buf,
				      ms_srcimps   = srcimps,
				      ms_imps      = the_imps,
				      ms_hs_date   = src_timestamp }))
    	}


-----------------------------------------------------------------------------
-- 			Error messages
-----------------------------------------------------------------------------

noModError :: DynFlags -> Maybe FilePath -> Module -> FindResult -> IO ab
-- ToDo: we don't have a proper line number for this error
noModError dflags cur_mod wanted_mod err
  = throwDyn $ ProgramError $ showSDoc $
    vcat [cantFindError dflags wanted_mod err,
	  nest 2 (parens (pp_where cur_mod))]
				
noHsFileErr cur_mod path
  = throwDyn $ CmdLineError $ showSDoc $
    vcat [text "Can't find" <+> text path,
	  nest 2 (parens (pp_where cur_mod))]
 
pp_where Nothing  = text "one of the roots of the dependency analysis"
pp_where (Just p) = text "imported from" <+> text p

packageModErr mod
  = throwDyn (CmdLineError (showSDoc (text "module" <+>
				   quotes (ppr mod) <+>
				   text "is a package module")))

multiRootsErr mod files
  = throwDyn (ProgramError (showSDoc (
	text "module" <+> quotes (ppr mod) <+> 
	text "is defined in multiple files:" <+>
	sep (map text files))))

cyclicModuleErr :: [ModSummary] -> SDoc
cyclicModuleErr ms
  = hang (ptext SLIT("Module imports form a cycle for modules:"))
       2 (vcat (map show_one ms))
  where
    show_one ms = sep [ show_mod (ms_hsc_src ms) (ms_mod ms),
			nest 2 $ ptext SLIT("imports:") <+> 
				   (pp_imps HsBootFile (ms_srcimps ms)
				   $$ pp_imps HsSrcFile  (ms_imps ms))]
    show_mod hsc_src mod = ppr mod <> text (hscSourceString hsc_src)
    pp_imps src mods = fsep (map (show_mod src) mods)


-- | Inform GHC that the working directory has changed.  GHC will flush
-- its cache of module locations, since it may no longer be valid.
-- Note: if you change the working directory, you should also unload
-- the current program (set targets to empty, followed by load).
workingDirectoryChanged :: Session -> IO ()
workingDirectoryChanged s = withSession s $ \hsc_env ->
  flushFinderCache (hsc_FC hsc_env)

-- -----------------------------------------------------------------------------
-- inspecting the session

-- | Get the module dependency graph.  After a 'load', this will contain
-- only the modules that were successfully loaded.
getModuleGraph :: Session -> IO ModuleGraph -- ToDo: DiGraph ModSummary
getModuleGraph s = withSession s (return . hsc_mod_graph)

getBindings :: Session -> IO [TyThing]
getBindings s = withSession s (return . nameEnvElts . ic_type_env . hsc_IC)

getPrintUnqual :: Session -> IO PrintUnqualified
getPrintUnqual s = withSession s (return . icPrintUnqual . hsc_IC)

#if 0
getModuleInfo :: Session -> Module -> IO ModuleInfo

data ObjectCode
  = ByteCode
  | BinaryCode FilePath

data ModuleInfo = ModuleInfo {
  lm_modulename :: Module,
  lm_summary    :: ModSummary,
  lm_interface  :: ModIface,
  lm_tc_code    :: Maybe TypecheckedCode,
  lm_rn_code    :: Maybe RenamedCode,
  lm_obj        :: Maybe ObjectCode
  }

type TypecheckedCode = HsTypecheckedGroup
type RenamedCode     = [HsGroup Name]

-- ToDo: typechecks abstract syntax or renamed abstract syntax.  Issues:
--   - typechecked syntax includes extra dictionary translation and
--     AbsBinds which need to be translated back into something closer to
--     the original source.
--   - renamed syntax currently doesn't exist in a single blob, since
--     renaming and typechecking are interleaved at splice points.  We'd
--     need a restriction that there are no splices in the source module.

-- ToDo:
--   - Data and Typeable instances for HsSyn.

-- ToDo:
--   - things that aren't in the output of the renamer:
--     - the export list
--     - the imports

-- ToDo:
--   - things that aren't in the output of the typechecker right now:
--     - the export list
--     - the imports
--     - type signatures
--     - type/data/newtype declarations
--     - class declarations
--     - instances
--   - extra things in the typechecker's output:
--     - default methods are turned into top-level decls.
--     - dictionary bindings

-- ToDo: check for small transformations that happen to the syntax in
-- the typechecker (eg. -e ==> negate e, perhaps for fromIntegral)

-- ToDo: maybe use TH syntax instead of IfaceSyn?  There's already a way
-- to get from TyCons, Ids etc. to TH syntax (reify).

-- :browse will use either lm_toplev or inspect lm_interface, depending
-- on whether the module is interpreted or not.

-- various abstract syntax types (perhaps IfaceBlah)
data Type = ...
data Kind = ...

-- This is for reconstructing refactored source code
-- Calls the lexer repeatedly.
-- ToDo: add comment tokens to token stream
getTokenStream :: Session -> Module -> IO [Located Token]
#endif

-- -----------------------------------------------------------------------------
-- Interactive evaluation

#ifdef GHCI

-- | Set the interactive evaluation context.
--
-- Setting the context doesn't throw away any bindings; the bindings
-- we've built up in the InteractiveContext simply move to the new
-- module.  They always shadow anything in scope in the current context.
setContext :: Session
	   -> [Module]	-- entire top level scope of these modules
	   -> [Module]	-- exports only of these modules
	   -> IO ()
setContext (Session ref) toplevs exports = do 
  hsc_env <- readIORef ref
  let old_ic  = hsc_IC     hsc_env
      hpt     = hsc_HPT    hsc_env
      dflags  = hsc_dflags hsc_env

  mapM_ (checkModuleExists hsc_env hpt) exports
  export_env  <- mkExportEnv hsc_env exports
  toplev_envs <- mapM (mkTopLevEnv hpt) toplevs
  let all_env = foldr plusGlobalRdrEnv export_env toplev_envs
  writeIORef ref hsc_env{ hsc_IC = old_ic { ic_toplev_scope = toplevs,
					    ic_exports      = exports,
					    ic_rn_gbl_env   = all_env } }

checkModuleExists :: HscEnv -> HomePackageTable -> Module -> IO ()
checkModuleExists hsc_env hpt mod = 
  case lookupModuleEnv hpt mod of
    Just mod_info -> return ()
    _not_a_home_module -> do
	  res <- findPackageModule hsc_env mod True
	  case res of
	    Found _ _ -> return  ()
	    err -> let msg = cantFindError (hsc_dflags hsc_env) mod err in
		   throwDyn (CmdLineError (showSDoc msg))

mkTopLevEnv :: HomePackageTable -> Module -> IO GlobalRdrEnv
mkTopLevEnv hpt modl
 = case lookupModuleEnv hpt modl of
      Nothing -> 	
	 throwDyn (ProgramError ("mkTopLevEnv: not a home module " 
			++ showSDoc (pprModule modl)))
      Just details ->
	 case mi_globals (hm_iface details) of
		Nothing  -> 
		   throwDyn (ProgramError ("mkTopLevEnv: not interpreted " 
						++ showSDoc (pprModule modl)))
		Just env -> return env

-- | Get the interactive evaluation context, consisting of a pair of the
-- set of modules from which we take the full top-level scope, and the set
-- of modules from which we take just the exports respectively.
getContext :: Session -> IO ([Module],[Module])
getContext s = withSession s (\HscEnv{ hsc_IC=ic } ->
				return (ic_toplev_scope ic, ic_exports ic))

-- | Returns 'True' if the specified module is interpreted, and hence has
-- its full top-level scope available.
moduleIsInterpreted :: Session -> Module -> IO Bool
moduleIsInterpreted s modl = withSession s $ \h ->
 case lookupModuleEnv (hsc_HPT h) modl of
      Just details       -> return (isJust (mi_globals (hm_iface details)))
      _not_a_home_module -> return False

-- | Looks up an identifier in the current interactive context (for :info)
getInfo :: Session -> String -> IO [GetInfoResult]
getInfo s id = withSession s $ \hsc_env -> hscGetInfo hsc_env id

-- -----------------------------------------------------------------------------
-- Getting the type of an expression

-- | Get the type of an expression
exprType :: Session -> String -> IO (Maybe Type)
exprType s expr = withSession s $ \hsc_env -> do
   maybe_stuff <- hscTcExpr hsc_env expr
   case maybe_stuff of
	Nothing -> return Nothing
	Just ty -> return (Just tidy_ty)
 	     where 
		tidy_ty = tidyType emptyTidyEnv ty
		dflags  = hsc_dflags hsc_env

-- -----------------------------------------------------------------------------
-- Getting the kind of a type

-- | Get the kind of a  type
typeKind  :: Session -> String -> IO (Maybe Kind)
typeKind s str = withSession s $ \hsc_env -> do
   maybe_stuff <- hscKcType hsc_env str
   case maybe_stuff of
	Nothing -> return Nothing
	Just kind -> return (Just kind)

-----------------------------------------------------------------------------
-- lookupName: returns the TyThing for a Name in the interactive context.
-- ToDo: should look it up in the full environment

lookupName :: Session -> Name -> IO (Maybe TyThing)
lookupName s name = withSession s $ \hsc_env -> do
  return $! lookupNameEnv (ic_type_env (hsc_IC hsc_env)) name

-----------------------------------------------------------------------------
-- cmCompileExpr: compile an expression and deliver an HValue

compileExpr :: Session -> String -> IO (Maybe HValue)
compileExpr s expr = withSession s $ \hsc_env -> do
  maybe_stuff <- hscStmt hsc_env ("let __cmCompileExpr = "++expr)
  case maybe_stuff of
	Nothing -> return Nothing
	Just (new_ic, names, hval) -> do
			-- Run it!
		hvals <- (unsafeCoerce# hval) :: IO [HValue]

		case (names,hvals) of
		  ([n],[hv]) -> return (Just hv)
		  _ 	     -> panic "compileExpr"

-- -----------------------------------------------------------------------------
-- running a statement interactively

data RunResult
  = RunOk [Name] 		-- ^ names bound by this evaluation
  | RunFailed	 		-- ^ statement failed compilation
  | RunException Exception	-- ^ statement raised an exception

-- | Run a statement in the current interactive context.  Statemenet
-- may bind multple values.
runStmt :: Session -> String -> IO RunResult
runStmt (Session ref) expr
   = do 
	hsc_env <- readIORef ref

	-- Turn off -fwarn-unused-bindings when running a statement, to hide
	-- warnings about the implicit bindings we introduce.
	let dflags'  = dopt_unset (hsc_dflags hsc_env) Opt_WarnUnusedBinds
	    hsc_env' = hsc_env{ hsc_dflags = dflags' }

        maybe_stuff <- hscStmt hsc_env' expr

        case maybe_stuff of
	   Nothing -> return RunFailed
	   Just (new_hsc_env, names, hval) -> do

		let thing_to_run = unsafeCoerce# hval :: IO [HValue]
		either_hvals <- sandboxIO thing_to_run

		case either_hvals of
		    Left e -> do
			-- on error, keep the *old* interactive context,
			-- so that 'it' is not bound to something
			-- that doesn't exist.
		        return (RunException e)

		    Right hvals -> do
			-- Get the newly bound things, and bind them.  
			-- Don't need to delete any shadowed bindings;
			-- the new ones override the old ones. 
			extendLinkEnv (zip names hvals)
	     		
			writeIORef ref new_hsc_env
			return (RunOk names)


-- We run the statement in a "sandbox" to protect the rest of the
-- system from anything the expression might do.  For now, this
-- consists of just wrapping it in an exception handler, but see below
-- for another version.

sandboxIO :: IO a -> IO (Either Exception a)
sandboxIO thing = Exception.try thing

{-
-- This version of sandboxIO runs the expression in a completely new
-- RTS main thread.  It is disabled for now because ^C exceptions
-- won't be delivered to the new thread, instead they'll be delivered
-- to the (blocked) GHCi main thread.

-- SLPJ: when re-enabling this, reflect a wrong-stat error as an exception

sandboxIO :: IO a -> IO (Either Int (Either Exception a))
sandboxIO thing = do
  st_thing <- newStablePtr (Exception.try thing)
  alloca $ \ p_st_result -> do
    stat <- rts_evalStableIO st_thing p_st_result
    freeStablePtr st_thing
    if stat == 1
	then do st_result <- peek p_st_result
		result <- deRefStablePtr st_result
		freeStablePtr st_result
		return (Right result)
	else do
		return (Left (fromIntegral stat))

foreign import "rts_evalStableIO"  {- safe -}
  rts_evalStableIO :: StablePtr (IO a) -> Ptr (StablePtr a) -> IO CInt
  -- more informative than the C type!
-}

-- ---------------------------------------------------------------------------
-- cmBrowseModule: get all the TyThings defined in a module

browseModule :: Session -> Module -> Bool -> IO [IfaceDecl]
browseModule s modl exports_only = withSession s $ \hsc_env -> do
  mb_decls <- getModuleContents hsc_env modl exports_only
  case mb_decls of
	Nothing -> return []		-- An error of some kind
	Just ds -> return ds


-----------------------------------------------------------------------------
-- show a module and it's source/object filenames

showModule :: Session -> ModSummary -> IO String
showModule s mod_summary = withSession s $ \hsc_env -> do
  case lookupModuleEnv (hsc_HPT hsc_env) (ms_mod mod_summary) of
	Nothing	      -> panic "missing linkable"
	Just mod_info -> return (showModMsg obj_linkable mod_summary)
		      where
			 obj_linkable = isObjectLinkable (hm_linkable mod_info)

#endif /* GHCI */
