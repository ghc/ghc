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
	initPackages,
	getSessionDynFlags,
	setSessionDynFlags,
	setMsgHandler,

	-- * Targets
	Target(..), TargetId(..),
	setTargets,
	getTargets,
	addTarget,
	removeTarget,
	guessTarget,
	
	-- * Loading\/compiling the program
	depanal,
	load, LoadHowMuch(..), SuccessFlag(..),	-- also does depanal
	loadMsgs,
	workingDirectoryChanged,
	checkModule, CheckedModule(..),

	-- * Inspecting the module structure of the program
	ModuleGraph, ModSummary(..),
	getModuleGraph,
	isLoaded,
	topSortModuleGraph,

	-- * Inspecting modules
	ModuleInfo,
	getModuleInfo,
	modInfoTyThings,
	modInfoTopLevelScope,
	lookupName,

	-- * Interactive evaluation
	getBindings, getPrintUnqual,
#ifdef GHCI
	setContext, getContext,	
	getNamesInScope,
	moduleIsInterpreted,
	getInfo, GetInfoResult,
	exprType,
	typeKind,
	parseName,
	RunResult(..),
	runStmt,
	browseModule,
	showModule,
	compileExpr, HValue,
#endif

	-- * Abstract syntax elements

	-- ** Modules
	Module, mkModule, pprModule,

	-- ** Names
	Name,
	
	-- ** Identifiers
	Id, idType,
	isImplicitId, isDeadBinder,
	isSpecPragmaId,	isExportedId, isLocalId, isGlobalId,
	isRecordSelector,
	isPrimOpId, isFCallId,
	isDataConWorkId, idDataCon,
	isBottomingId, isDictonaryId,

	-- ** Type constructors
	TyCon, 
	isClassTyCon, isSynTyCon, isNewTyCon,

	-- ** Data constructors
	DataCon,

	-- ** Classes
	Class, 
	classSCTheta, classTvsFds,

	-- ** Types and Kinds
	Type, dropForAlls,
	Kind,

	-- ** Entities
	TyThing(..), 

	-- * Exceptions
	GhcException(..), showGhcException,

	-- * Miscellaneous
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
import TcRnDriver	( mkExportEnv, getModuleContents, tcRnLookupRdrName )
import RdrName		( plusGlobalRdrEnv )
import HscMain		( hscGetInfo, GetInfoResult, hscParseIdentifier,
			  hscStmt, hscTcExpr, hscKcType )
import Type		( tidyType )
import VarEnv		( emptyTidyEnv )
import GHC.Exts		( unsafeCoerce# )
import IfaceSyn		( IfaceDecl )
#endif

import Packages		( initPackages )
import NameSet		( NameSet, nameSetToList )
import RdrName		( GlobalRdrEnv )
import HsSyn		( HsModule, LHsBinds )
import Type		( Kind, Type, dropForAlls )
import Id		( Id, idType, isImplicitId, isDeadBinder,
                          isSpecPragmaId, isExportedId, isLocalId, isGlobalId,
                          isRecordSelector,
                          isPrimOpId, isFCallId,
                          isDataConWorkId, idDataCon,
                          isBottomingId )
import TyCon		( TyCon, isClassTyCon, isSynTyCon, isNewTyCon )
import Class		( Class, classSCTheta, classTvsFds )
import DataCon		( DataCon )
import Name		( Name, getName, nameModule_maybe )
import RdrName		( RdrName, gre_name, globalRdrEnvElts )
import NameEnv		( nameEnvElts )
import SrcLoc		( Located(..) )
import DriverPipeline
import DriverPhases	( Phase(..), isHaskellSrcFilename, startPhase )
import GetImports	( getImports )
import Packages		( isHomePackage )
import Finder
import HscMain		( newHscEnv, hscFileCheck, HscResult(..) )
import HscTypes
import DynFlags
import StaticFlags
import SysTools		( initSysTools, cleanTempFiles )
import Module
import FiniteMap
import Panic
import Digraph
import ErrUtils		( showPass, Messages, putMsg, debugTraceMsg )
import qualified ErrUtils
import Util
import StringBuffer	( StringBuffer, hGetStringBuffer )
import Outputable
import SysTools		( cleanTempFilesExcept )
import BasicTypes	( SuccessFlag(..), succeeded, failed )
import Maybes		( orElse, expectJust, mapCatMaybes )
import TcType           ( tcSplitSigmaTy, isDictTy )

import Directory        ( getModificationTime, doesFileExist )
import Maybe		( isJust, isNothing, fromJust, fromMaybe, catMaybes )
import Maybes		( expectJust )
import List		( partition, nub )
import qualified List
import Monad		( unless, when, foldM )
import System		( exitWith, ExitCode(..) )
import Time		( ClockTime )
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
		IOException _ ->  putMsg (show exception)
		AsyncException StackOverflow ->
			putMsg "stack overflow: use +RTS -K<size> to increase it"
		_other ->  putMsg (show (Panic (show exception)))
	   exitWith (ExitFailure 1)
         ) $

  -- all error messages are propagated as exceptions
  handleDyn (\dyn -> do
  		hFlush stdout
  		case dyn of
		     PhaseFailed _ code -> exitWith code
		     Interrupted -> exitWith (ExitFailure 1)
		     _ -> do putMsg (show (dyn :: GhcException))
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
-- ToDo: GhcMode should say "keep typechecked code" and\/or "keep renamed
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
-- the program\/library.  Unloading the current program is achieved by
-- setting the current set of targets to be empty, followed by load.
setTargets :: Session -> [Target] -> IO ()
setTargets s targets = modifySession s (\h -> h{ hsc_targets = targets })

-- | returns the current set of targets
getTargets :: Session -> IO [Target]
getTargets s = withSession s (return . hsc_targets)

-- | Add another target
addTarget :: Session -> Target -> IO ()
addTarget s target
  = modifySession s (\h -> h{ hsc_targets = target : hsc_targets h })

-- | Remove a target
removeTarget :: Session -> TargetId -> IO ()
removeTarget s target_id
  = modifySession s (\h -> h{ hsc_targets = filter (hsc_targets h) })
  where
   filter targets = [ t | t@(Target id _) <- targets, id /= target_id ]

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
  when (gmode == BatchCompile) $
	debugTraceMsg dflags 1 (showSDoc (hcat [
		     text "Chasing modules from: ",
	     		hcat (punctuate comma (map pprTarget targets))]))

  graph <- downsweep hsc_env old_graph excluded_mods
  writeIORef ref hsc_env{ hsc_mod_graph=graph }

{-
-- | The result of load.
data LoadResult
  = LoadOk	Errors	-- ^ all specified targets were loaded successfully.
  | LoadFailed  Errors	-- ^ not all modules were loaded.

type Errors = [String]

data ErrMsg = ErrMsg { 
	errMsgSeverity  :: Severity,  -- warning, error, etc.
	errMsgSpans     :: [SrcSpan],
	errMsgShortDoc  :: Doc,
	errMsgExtraInfo :: Doc
	}
-}

data LoadHowMuch
   = LoadAllTargets
   | LoadUpTo Module
   | LoadDependenciesOf Module

-- | Try to load the program.  If a Module is supplied, then just
-- attempt to load up to this target.  If no Module is supplied,
-- then try to load all targets.
load :: Session -> LoadHowMuch -> IO SuccessFlag
load session how_much = 
   loadMsgs session how_much ErrUtils.printErrorsAndWarnings

-- | Version of 'load' that takes a callback function to be invoked
-- on compiler errors and warnings as they occur during compilation.
loadMsgs :: Session -> LoadHowMuch -> (Messages-> IO ()) -> IO SuccessFlag
loadMsgs s@(Session ref) how_much msg_act
   = do 
	-- Dependency analysis first.  Note that this fixes the module graph:
	-- even if we don't get a fully successful upsweep, the full module
	-- graph is still retained in the Session.  We can tell which modules
	-- were successfully loaded by inspecting the Session's HPT.
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

        -- mg2_with_srcimps drops the hi-boot nodes, returning a 
	-- graph with cycles.  Among other things, it is used for
        -- backing out partially complete cycles following a failed
        -- upsweep, and for removing from hpt all the modules
        -- not in strict downwards closure, during calls to compile.
        let mg2_with_srcimps :: [SCC ModSummary]
	    mg2_with_srcimps = topSortModuleGraph True mod_graph Nothing

	    -- check the stability property for each module.
	    stable_mods@(stable_obj,stable_bco)
		| BatchCompile <- ghci_mode = ([],[])
	        | otherwise = checkStability hpt1 mg2_with_srcimps all_home_mods

	    -- prune bits of the HPT which are definitely redundant now,
	    -- to save space.
	    pruned_hpt = pruneHomePackageTable hpt1 
				(flattenSCCs mg2_with_srcimps)
				stable_mods

	evaluate pruned_hpt

	debugTraceMsg dflags 2 (showSDoc (text "Stable obj:" <+> ppr stable_obj $$
				text "Stable BCO:" <+> ppr stable_bco))

	-- Unload any modules which are going to be re-linked this time around.
	let stable_linkables = [ linkable
			       | m <- stable_obj++stable_bco,
				 Just hmi <- [lookupModuleEnv pruned_hpt m],
				 Just linkable <- [hm_linkable hmi] ]
	unload hsc_env stable_linkables

        -- We could at this point detect cycles which aren't broken by
        -- a source-import, and complain immediately, but it seems better
        -- to let upsweep_mods do this, so at least some useful work gets
        -- done before the upsweep is abandoned.
        --hPutStrLn stderr "after tsort:\n"
        --hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

        -- Now do the upsweep, calling compile for each module in
        -- turn.  Final result is version 3 of everything.

        -- Topologically sort the module graph, this time including hi-boot
	-- nodes, and possibly just including the portion of the graph
	-- reachable from the module specified in the 2nd argument to load.
	-- This graph should be cycle-free.
	-- If we're restricting the upsweep to a portion of the graph, we
	-- also want to retain everything that is still stable.
        let full_mg :: [SCC ModSummary]
	    full_mg    = topSortModuleGraph False mod_graph Nothing

	    maybe_top_mod = case how_much of
				LoadUpTo m           -> Just m
			  	LoadDependenciesOf m -> Just m
			  	_		     -> Nothing

	    partial_mg0 :: [SCC ModSummary]
	    partial_mg0 = topSortModuleGraph False mod_graph maybe_top_mod

	    -- LoadDependenciesOf m: we want the upsweep to stop just
	    -- short of the specified module (unless the specified module
	    -- is stable).
	    partial_mg
		| LoadDependenciesOf mod <- how_much
		= ASSERT( case last partial_mg0 of 
			    AcyclicSCC ms -> ms_mod ms == mod; _ -> False )
		  List.init partial_mg0
		| otherwise
		= partial_mg0
  
	    stable_mg = 
		[ AcyclicSCC ms
	        | AcyclicSCC ms <- full_mg,
		  ms_mod ms `elem` stable_obj++stable_bco,
		  ms_mod ms `notElem` [ ms_mod ms' | 
					AcyclicSCC ms' <- partial_mg ] ]

	    mg = stable_mg ++ partial_mg

	-- clean up between compilations
	let cleanup = cleanTempFilesExcept dflags
			  (ppFilesFromSummaries (flattenSCCs mg2_with_srcimps))

        (upsweep_ok, hsc_env1, modsUpswept)
           <- upsweep (hsc_env { hsc_HPT = emptyHomePackageTable })
			   pruned_hpt stable_mods cleanup msg_act mg

	-- Make modsDone be the summaries for each home module now
	-- available; this should equal the domain of hpt3.
        -- Get in in a roughly top .. bottom order (hence reverse).

        let modsDone = reverse modsUpswept

        -- Try and do linking in some form, depending on whether the
        -- upsweep was completely or only partially successful.

        if succeeded upsweep_ok

         then 
           -- Easy; just relink it all.
           do debugTraceMsg dflags 2 "Upsweep completely successful."

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

	      when (ghci_mode == BatchCompile && isJust ofile && not do_linking) $
	        debugTraceMsg dflags 1 ("Warning: output was redirected with -o, " ++
				   "but no output will be generated\n" ++
				   "because there is no " ++ main_mod ++ " module.")

	      -- link everything together
              linkresult <- link ghci_mode dflags do_linking (hsc_HPT hsc_env1)

	      loadFinish Succeeded linkresult ref hsc_env1

         else 
           -- Tricky.  We need to back out the effects of compiling any
           -- half-done cycles, both so as to clean up the top level envs
           -- and to avoid telling the interactive linker to link them.
           do debugTraceMsg dflags 2 "Upsweep partially successful."

              let modsDone_names
                     = map ms_mod modsDone
              let mods_to_zap_names 
                     = findPartiallyCompletedCycles modsDone_names 
			  mg2_with_srcimps
              let mods_to_keep
                     = filter ((`notElem` mods_to_zap_names).ms_mod) 
			  modsDone

              let hpt4 = retainInTopLevelEnvs (map ms_mod mods_to_keep) 
					      (hsc_HPT hsc_env1)

	      -- Clean up after ourselves
	      cleanTempFilesExcept dflags (ppFilesFromSummaries mods_to_keep)

	      -- there should be no Nothings where linkables should be, now
	      ASSERT(all (isJust.hm_linkable) 
			(moduleEnvElts (hsc_HPT hsc_env))) do
	
	      -- Link everything together
              linkresult <- link ghci_mode dflags False hpt4

	      let hsc_env4 = hsc_env1{ hsc_HPT = hpt4 }
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

-- -----------------------------------------------------------------------------
-- Check module

data CheckedModule = 
  CheckedModule { parsedSource      :: ParsedSource,
		-- ToDo: renamedSource
		  typecheckedSource :: Maybe TypecheckedSource,
		  checkedModuleInfo :: Maybe ModuleInfo
	        }

type ParsedSource  = Located (HsModule RdrName)
type TypecheckedSource = LHsBinds Id

-- | This is the way to get access to parsed and typechecked source code
-- for a module.  'checkModule' loads all the dependencies of the specified
-- module in the Session, and then attempts to typecheck the module.  If
-- successful, it returns the abstract syntax for the module.
checkModule :: Session -> Module -> (Messages -> IO ()) 
	-> IO (Maybe CheckedModule)
checkModule session@(Session ref) mod msg_act = do
	-- load up the dependencies first
   r <- loadMsgs session (LoadDependenciesOf mod) msg_act
   if (failed r) then return Nothing else do

	-- now parse & typecheck the module
   hsc_env <- readIORef ref   
   let mg  = hsc_mod_graph hsc_env
   case [ ms | ms <- mg, ms_mod ms == mod ] of
	[] -> return Nothing
	(ms:_) -> do 
	   r <- hscFileCheck hsc_env msg_act ms
	   case r of
		HscFail -> 
		   return Nothing
		HscChecked parsed Nothing ->
		   return (Just (CheckedModule parsed Nothing Nothing))
		HscChecked parsed (Just (tc_binds, rdr_env, details)) -> do
		   let minf = ModuleInfo {
				minf_details  = details,
				minf_rdr_env  = Just rdr_env
			      }
		   return (Just (CheckedModule {
					parsedSource = parsed,
					typecheckedSource = Just tc_binds,
					checkedModuleInfo = Just minf }))

-- ---------------------------------------------------------------------------
-- Unloading

unload :: HscEnv -> [Linkable] -> IO ()
unload hsc_env stable_linkables	-- Unload everthing *except* 'stable_linkables'
  = case ghcMode (hsc_dflags hsc_env) of
	BatchCompile  -> return ()
	JustTypecheck -> return ()
#ifdef GHCI
	Interactive -> Linker.unload (hsc_dflags hsc_env) stable_linkables
#else
	Interactive -> panic "unload: no interpreter"
#endif
	other -> panic "unload: strange mode"

-- -----------------------------------------------------------------------------
-- checkStability

{-
  Stability tells us which modules definitely do not need to be recompiled.
  There are two main reasons for having stability:
  
   - avoid doing a complete upsweep of the module graph in GHCi when
     modules near the bottom of the tree have not changed.

   - to tell GHCi when it can load object code: we can only load object code
     for a module when we also load object code fo  all of the imports of the
     module.  So we need to know that we will definitely not be recompiling
     any of these modules, and we can use the object code.

  NB. stability is of no importance to BatchCompile at all, only Interactive.
  (ToDo: what about JustTypecheck?)

  The stability check is as follows.  Both stableObject and
  stableBCO are used during the upsweep phase later.

  -------------------
  stable m = stableObject m || stableBCO m

  stableObject m = 
	all stableObject (imports m)
	&& old linkable does not exist, or is == on-disk .o
	&& date(on-disk .o) > date(.hs)

  stableBCO m =
	all stable (imports m)
	&& date(BCO) > date(.hs)
  -------------------    

  These properties embody the following ideas:

    - if a module is stable:
	- if it has been compiled in a previous pass (present in HPT)
	  then it does not need to be compiled or re-linked.
        - if it has not been compiled in a previous pass,
	  then we only need to read its .hi file from disk and
	  link it to produce a ModDetails.

    - if a modules is not stable, we will definitely be at least
      re-linking, and possibly re-compiling it during the upsweep.
      All non-stable modules can (and should) therefore be unlinked
      before the upsweep.

    - Note that objects are only considered stable if they only depend
      on other objects.  We can't link object code against byte code.
-}

checkStability
	:: HomePackageTable		-- HPT from last compilation
	-> [SCC ModSummary]		-- current module graph (cyclic)
	-> [Module]			-- all home modules
	-> ([Module],			-- stableObject
	    [Module])			-- stableBCO

checkStability hpt sccs all_home_mods = foldl checkSCC ([],[]) sccs
  where
   checkSCC (stable_obj, stable_bco) scc0
     | stableObjects = (scc_mods ++ stable_obj, stable_bco)
     | stableBCOs    = (stable_obj, scc_mods ++ stable_bco)
     | otherwise     = (stable_obj, stable_bco)
     where
	scc = flattenSCC scc0
	scc_mods = map ms_mod scc
	home_module m   = m `elem` all_home_mods && m `notElem` scc_mods

        scc_allimps = nub (filter home_module (concatMap ms_allimps scc))
	    -- all imports outside the current SCC, but in the home pkg
	
	stable_obj_imps = map (`elem` stable_obj) scc_allimps
	stable_bco_imps = map (`elem` stable_bco) scc_allimps

	stableObjects = 
	   and stable_obj_imps
	   && all object_ok scc

	stableBCOs = 
	   and (zipWith (||) stable_obj_imps stable_bco_imps)
	   && all bco_ok scc

	object_ok ms
	  | Just t <- ms_obj_date ms  =  t >= ms_hs_date ms 
					 && same_as_prev t
	  | otherwise = False
	  where
	     same_as_prev t = case lookupModuleEnv hpt (ms_mod ms) of
				Nothing  -> True
				Just hmi  | Just l <- hm_linkable hmi
				 -> isObjectLinkable l && t == linkableTime l
		-- why '>=' rather than '>' above?  If the filesystem stores
		-- times to the nearset second, we may occasionally find that
		-- the object & source have the same modification time, 
		-- especially if the source was automatically generated
		-- and compiled.  Using >= is slightly unsafe, but it matches
		-- make's behaviour.

	bco_ok ms
	  = case lookupModuleEnv hpt (ms_mod ms) of
	   	Nothing  -> False
		Just hmi  | Just l <- hm_linkable hmi ->
			not (isObjectLinkable l) && 
			linkableTime l >= ms_hs_date ms

ms_allimps :: ModSummary -> [Module]
ms_allimps ms = ms_srcimps ms ++ ms_imps ms

-- -----------------------------------------------------------------------------
-- Prune the HomePackageTable

-- Before doing an upsweep, we can throw away:
--
--   - For non-stable modules:
--	- all ModDetails, all linked code
--   - all unlinked code that is out of date with respect to
--     the source file
--
-- This is VERY IMPORTANT otherwise we'll end up requiring 2x the
-- space at the end of the upsweep, because the topmost ModDetails of the
-- old HPT holds on to the entire type environment from the previous
-- compilation.

pruneHomePackageTable
   :: HomePackageTable
   -> [ModSummary]
   -> ([Module],[Module])
   -> HomePackageTable

pruneHomePackageTable hpt summ (stable_obj, stable_bco)
  = mapModuleEnv prune hpt
  where prune hmi
	  | is_stable modl = hmi'
	  | otherwise      = hmi'{ hm_details = emptyModDetails }
	  where
	   modl = mi_module (hm_iface hmi)
	   hmi' | Just l <- hm_linkable hmi, linkableTime l < ms_hs_date ms
		= hmi{ hm_linkable = Nothing }
		| otherwise
		= hmi
		where ms = expectJust "prune" (lookupModuleEnv ms_map modl)

        ms_map = mkModuleEnv [(ms_mod ms, ms) | ms <- summ]

	is_stable m = m `elem` stable_obj || m `elem` stable_bco

-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------
-- The upsweep

-- This is where we compile each module in the module graph, in a pass
-- from the bottom to the top of the graph.

-- There better had not be any cyclic groups here -- we check for them.

upsweep
    :: HscEnv			-- Includes initially-empty HPT
    -> HomePackageTable		-- HPT from last time round (pruned)
    -> ([Module],[Module])	-- stable modules (see checkStability)
    -> IO ()			-- How to clean up unwanted tmp files
    -> (Messages -> IO ())	-- Compiler error message callback
    -> [SCC ModSummary]		-- Mods to do (the worklist)
    -> IO (SuccessFlag,
           HscEnv,		-- With an updated HPT
           [ModSummary])	-- Mods which succeeded

upsweep hsc_env old_hpt stable_mods cleanup msg_act mods
   = upsweep' hsc_env old_hpt stable_mods cleanup msg_act mods 1 (length mods)

upsweep hsc_env old_hpt stable_mods cleanup msg_act
     [] _ _
   = return (Succeeded, hsc_env, [])

upsweep hsc_env old_hpt stable_mods cleanup msg_act
     (CyclicSCC ms:_) _ _
   = do putMsg (showSDoc (cyclicModuleErr ms))
        return (Failed, hsc_env, [])

upsweep hsc_env old_hpt stable_mods cleanup msg_act
     (AcyclicSCC mod:mods) mod_index nmods
   = do -- putStrLn ("UPSWEEP_MOD: hpt = " ++ 
	--	     show (map (moduleUserString.moduleName.mi_module.hm_iface) 
	--		       (moduleEnvElts (hsc_HPT hsc_env)))

        mb_mod_info <- upsweep_mod hsc_env old_hpt stable_mods msg_act mod 
                       mod_index nmods

	cleanup		-- Remove unwanted tmp files between compilations

        case mb_mod_info of
	    Nothing -> return (Failed, hsc_env, [])
	    Just mod_info -> do 
		{ let this_mod = ms_mod mod

			-- Add new info to hsc_env
		      hpt1     = extendModuleEnv (hsc_HPT hsc_env) 
					this_mod mod_info
		      hsc_env1 = hsc_env { hsc_HPT = hpt1 }

			-- Space-saving: delete the old HPT entry
			-- for mod BUT if mod is a hs-boot
			-- node, don't delete it.  For the
			-- interface, the HPT entry is probaby for the
			-- main Haskell source file.  Deleting it
			-- would force .. (what?? --SDM)
		      old_hpt1 | isBootSummary mod = old_hpt
			       | otherwise = delModuleEnv old_hpt this_mod

		; (restOK, hsc_env2, modOKs) 
			<- upsweep hsc_env1 old_hpt1 stable_mods cleanup 
				msg_act mods (mod_index+1) nmods
		; return (restOK, hsc_env2, mod:modOKs)
		}


-- Compile a single module.  Always produce a Linkable for it if 
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: HscEnv
            -> HomePackageTable
	    -> ([Module],[Module])
	    -> (Messages -> IO ())
            -> ModSummary
            -> Int  -- index of module
            -> Int  -- total number of modules
            -> IO (Maybe HomeModInfo)	-- Nothing => Failed

upsweep_mod hsc_env old_hpt (stable_obj, stable_bco) msg_act summary mod_index nmods
   = do 
        let 
	    this_mod    = ms_mod summary
	    mb_obj_date = ms_obj_date summary
	    obj_fn	= ml_obj_file (ms_location summary)
	    hs_date     = ms_hs_date summary

	    compile_it :: Maybe Linkable -> IO (Maybe HomeModInfo)
	    compile_it  = upsweep_compile hsc_env old_hpt this_mod 
				msg_act summary mod_index nmods

	case ghcMode (hsc_dflags hsc_env) of
	    BatchCompile ->
		case () of
		   -- Batch-compilating is easy: just check whether we have
		   -- an up-to-date object file.  If we do, then the compiler
		   -- needs to do a recompilation check.
		   _ | Just obj_date <- mb_obj_date, obj_date >= hs_date -> do
		           linkable <- 
				findObjectLinkable this_mod obj_fn obj_date
			   compile_it (Just linkable)

		     | otherwise ->
		           compile_it Nothing

	    interactive ->
		case () of
		    _ | is_stable_obj, isJust old_hmi ->
			   return old_hmi
			-- object is stable, and we have an entry in the
			-- old HPT: nothing to do

		      | is_stable_obj, isNothing old_hmi -> do
		           linkable <-
				findObjectLinkable this_mod obj_fn 
					(expectJust "upseep1" mb_obj_date)
			   compile_it (Just linkable)
			-- object is stable, but we need to load the interface
			-- off disk to make a HMI.

		      | is_stable_bco -> 
			   ASSERT(isJust old_hmi) -- must be in the old_hpt
			   return old_hmi
			-- BCO is stable: nothing to do

		      | Just hmi <- old_hmi,
			Just l <- hm_linkable hmi, not (isObjectLinkable l),
			linkableTime l >= ms_hs_date summary ->
			   compile_it (Just l)
			-- we have an old BCO that is up to date with respect
			-- to the source: do a recompilation check as normal.

		      | otherwise ->
			  compile_it Nothing
			-- no existing code at all: we must recompile.
		   where
		    is_stable_obj = this_mod `elem` stable_obj
		    is_stable_bco = this_mod `elem` stable_bco

		    old_hmi = lookupModuleEnv old_hpt this_mod

-- Run hsc to compile a module
upsweep_compile hsc_env old_hpt this_mod msg_act summary
                mod_index nmods
                mb_old_linkable = do
  let
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

  compresult <- compile hsc_env msg_act summary mb_old_linkable mb_old_iface
                        mod_index nmods

  case compresult of
        -- Compilation failed.  Compile may still have updated the PCS, tho.
        CompErrs -> return Nothing

	-- Compilation "succeeded", and may or may not have returned a new
	-- linkable (depending on whether compilation was actually performed
	-- or not).
	CompOK new_details new_iface new_linkable
              -> do let new_info = HomeModInfo { hm_iface = new_iface,
						 hm_details = new_details,
						 hm_linkable = new_linkable }
                    return (Just new_info)


-- Filter modules in the HPT
retainInTopLevelEnvs :: [Module] -> HomePackageTable -> HomePackageTable
retainInTopLevelEnvs keep_these hpt
   = mkModuleEnv [ (mod, expectJust "retain" mb_mod_info)
		 | mod <- keep_these
		 , let mb_mod_info = lookupModuleEnv hpt mod
		 , isJust mb_mod_info ]

-- ---------------------------------------------------------------------------
-- Topological sort of the module graph

topSortModuleGraph
	  :: Bool 		-- Drop hi-boot nodes? (see below)
	  -> [ModSummary]
	  -> Maybe Module
	  -> [SCC ModSummary]
-- Calculate SCCs of the module graph, possibly dropping the hi-boot nodes
-- The resulting list of strongly-connected-components is in topologically
-- sorted order, starting with the module(s) at the bottom of the
-- dependency graph (ie compile them first) and ending with the ones at
-- the top.
--
-- Drop hi-boot nodes (first boolean arg)? 
--
--   False:	treat the hi-boot summaries as nodes of the graph,
--		so the graph must be acyclic
--
--   True:	eliminate the hi-boot nodes, and instead pretend
--		the a source-import of Foo is an import of Foo
--		The resulting graph has no hi-boot nodes, but can by cyclic

topSortModuleGraph drop_hs_boot_nodes summaries Nothing
  = stronglyConnComp (fst (moduleGraphNodes drop_hs_boot_nodes summaries))
topSortModuleGraph drop_hs_boot_nodes summaries (Just mod)
  = stronglyConnComp (map vertex_fn (reachable graph root))
  where 
	-- restrict the graph to just those modules reachable from
	-- the specified module.  We do this by building a graph with
	-- the full set of nodes, and determining the reachable set from
	-- the specified node.
	(nodes, lookup_key) = moduleGraphNodes drop_hs_boot_nodes summaries
	(graph, vertex_fn, key_fn) = graphFromEdges' nodes
	root 
	  | Just key <- lookup_key HsSrcFile mod, Just v <- key_fn key = v
	  | otherwise  = throwDyn (ProgramError "module does not exist")

moduleGraphNodes :: Bool -> [ModSummary]
  -> ([(ModSummary, Int, [Int])], HscSource -> Module -> Maybe Int)
moduleGraphNodes drop_hs_boot_nodes summaries = (nodes, lookup_key)
   where
	-- Drop hs-boot nodes by using HsSrcFile as the key
	hs_boot_key | drop_hs_boot_nodes = HsSrcFile
		    | otherwise		 = HsBootFile	

	-- We use integers as the keys for the SCC algorithm
	nodes :: [(ModSummary, Int, [Int])]	
	nodes = [(s, expectJust "topSort" (lookup_key (ms_hsc_src s) (ms_mod s)), 
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
	roots = hsc_targets hsc_env

	old_summary_map :: NodeMap ModSummary
	old_summary_map = mkNodeMap old_summaries

	getRootSummary :: Target -> IO ModSummary
	getRootSummary (Target (TargetFile file) maybe_buf)
	   = do exists <- doesFileExist file
		if exists then summariseFile hsc_env file maybe_buf else do
		throwDyn (CmdLineError ("can't find file: " ++ file))	
	getRootSummary (Target (TargetModule modl) maybe_buf)
 	   = do maybe_summary <- summarise hsc_env emptyNodeMap Nothing False 
					   modl maybe_buf excl_mods
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
			   [ expectJust "checkDup" (ml_hs_file (ms_location summ'))
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
						 wanted_mod Nothing excl_mods
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

summariseFile :: HscEnv -> FilePath
   -> Maybe (StringBuffer,ClockTime)
   -> IO ModSummary
-- Used for Haskell source only, I think
-- We know the file name, and we know it exists,
-- but we don't necessarily know the module name (might differ)
summariseFile hsc_env file maybe_buf
   = do let dflags = hsc_dflags hsc_env

	(dflags', hspp_fn, buf)
	    <- preprocessFile dflags file maybe_buf

        (srcimps,the_imps,mod) <- getImports dflags' buf hspp_fn

	-- Make a ModLocation for this file
	location <- mkHomeModLocation dflags mod file

	-- Tell the Finder cache where it is, so that subsequent calls
	-- to findModule will find it, even if it's not on any search path
	addHomeModuleToFinder hsc_env mod location

        src_timestamp <- case maybe_buf of
			   Just (_,t) -> return t
			   Nothing    -> getModificationTime file

	obj_timestamp <- modificationTimeIfExists (ml_obj_file location)

        return (ModSummary { ms_mod = mod, ms_hsc_src = HsSrcFile,
			     ms_location = location,
                             ms_hspp_file = Just hspp_fn,
			     ms_hspp_buf  = Just buf,
                             ms_srcimps = srcimps, ms_imps = the_imps,
			     ms_hs_date = src_timestamp,
			     ms_obj_date = obj_timestamp })

-- Summarise a module, and pick up source and timestamp.
summarise :: HscEnv
	  -> NodeMap ModSummary	-- Map of old summaries
	  -> Maybe FilePath	-- Importing module (for error messages)
	  -> IsBootInterface	-- True <=> a {-# SOURCE #-} import
	  -> Module 		-- Imported module to be summarised
	  -> Maybe (StringBuffer, ClockTime)
	  -> [Module]		-- Modules to exclude
	  -> IO (Maybe ModSummary)	-- Its new summary

summarise hsc_env old_summary_map cur_mod is_boot wanted_mod maybe_buf excl_mods
  | wanted_mod `elem` excl_mods
  = return Nothing

  | Just old_summary <- lookupFM old_summary_map (wanted_mod, hsc_src)
  = do	 	-- Find its new timestamp; all the 
		-- ModSummaries in the old map have valid ml_hs_files
	let location = ms_location old_summary
	    src_fn = expectJust "summarise" (ml_hs_file location)

		-- return the cached summary if the source didn't change
	src_timestamp <- case maybe_buf of
			   Just (_,t) -> return t
			   Nothing    -> getModificationTime src_fn

	if ms_hs_date old_summary == src_timestamp 
	   then do -- update the object-file timestamp
		  obj_timestamp <- getObjTimestamp location is_boot
		  return (Just old_summary{ ms_obj_date = obj_timestamp })
	   else
		-- source changed: re-summarise
		new_summary location src_fn maybe_buf src_timestamp

  | otherwise
  = do	found <- findModule hsc_env wanted_mod True {-explicit-}
	case found of
	     Found location pkg 
		| not (isHomePackage pkg) -> return Nothing
			-- Drop external-pkg
		| isJust (ml_hs_file location) -> just_found location
			-- Home package
	     err -> noModError dflags cur_mod wanted_mod err
			-- Not found
  where
    dflags = hsc_dflags hsc_env

    hsc_src = if is_boot then HsBootFile else HsSrcFile

    just_found location = do
	  	-- Adjust location to point to the hs-boot source file, 
		-- hi file, object file, when is_boot says so
	let location' | is_boot   = addBootSuffixLocn location
		      | otherwise = location
	    src_fn = expectJust "summarise2" (ml_hs_file location')

		-- Check that it exists
	  	-- It might have been deleted since the Finder last found it
	maybe_t <- modificationTimeIfExists src_fn
	case maybe_t of
	  Nothing -> noHsFileErr cur_mod src_fn
	  Just t  -> new_summary location' src_fn Nothing t


    new_summary location src_fn maybe_bug src_timestamp
      = do
	-- Preprocess the source file and get its imports
	-- The dflags' contains the OPTIONS pragmas
	(dflags', hspp_fn, buf) <- preprocessFile dflags src_fn maybe_buf
        (srcimps, the_imps, mod_name) <- getImports dflags' buf hspp_fn

	when (mod_name /= wanted_mod) $
		throwDyn (ProgramError 
		   (showSDoc (text src_fn
			      <>  text ": file name does not match module name"
			      <+> quotes (ppr mod_name))))

		-- Find the object timestamp, and return the summary
	obj_timestamp <- getObjTimestamp location is_boot

	return (Just ( ModSummary { ms_mod       = wanted_mod, 
				    ms_hsc_src   = hsc_src,
				    ms_location  = location,
				    ms_hspp_file = Just hspp_fn,
				    ms_hspp_buf  = Just buf,
				    ms_srcimps   = srcimps,
				    ms_imps      = the_imps,
				    ms_hs_date   = src_timestamp,
				    ms_obj_date  = obj_timestamp }))


getObjTimestamp location is_boot
  = if is_boot then return Nothing
	       else modificationTimeIfExists (ml_obj_file location)


preprocessFile :: DynFlags -> FilePath -> Maybe (StringBuffer,ClockTime)
  -> IO (DynFlags, FilePath, StringBuffer)
preprocessFile dflags src_fn Nothing
  = do
	(dflags', hspp_fn) <- preprocess dflags src_fn
	buf <- hGetStringBuffer hspp_fn
	return (dflags', hspp_fn, buf)

preprocessFile dflags src_fn (Just (buf, time))
  = do
	-- case we bypass the preprocessing stage?
	let 
	    local_opts = getOptionsFromStringBuffer buf
	--
	(dflags', errs) <- parseDynamicFlags dflags local_opts

	let
	    needs_preprocessing
		| Unlit _ <- startPhase src_fn  = True
		  -- note: local_opts is only required if there's no Unlit phase
		| dopt Opt_Cpp dflags'		= True
		| dopt Opt_Pp  dflags'		= True
		| otherwise			= False

	when needs_preprocessing $
	   ghcError (ProgramError "buffer needs preprocesing; interactive check disabled")

	return (dflags', src_fn, buf)


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

-- | Get the module dependency graph.
getModuleGraph :: Session -> IO ModuleGraph -- ToDo: DiGraph ModSummary
getModuleGraph s = withSession s (return . hsc_mod_graph)

isLoaded :: Session -> Module -> IO Bool
isLoaded s m = withSession s $ \hsc_env ->
  return $! isJust (lookupModuleEnv (hsc_HPT hsc_env) m)

getBindings :: Session -> IO [TyThing]
getBindings s = withSession s (return . nameEnvElts . ic_type_env . hsc_IC)

getPrintUnqual :: Session -> IO PrintUnqualified
getPrintUnqual s = withSession s (return . icPrintUnqual . hsc_IC)

#ifdef GHCI
-- | Parses a string as an identifier, and returns the list of 'Name's that
-- the identifier can refer to in the current interactive context.
parseName :: Session -> String -> IO [Name]
parseName s str = withSession s $ \hsc_env -> do
   maybe_rdr_name <- hscParseIdentifier (hsc_dflags hsc_env) str
   case maybe_rdr_name of
	Nothing -> return []
	Just (L _ rdr_name) -> do
	    mb_names <- tcRnLookupRdrName hsc_env rdr_name
	    case mb_names of
		Nothing -> return []
		Just ns -> return ns
		-- ToDo: should return error messages
#endif

-- | Returns the 'TyThing' for a 'Name'.  The 'Name' may refer to any
-- entity known to GHC, including 'Name's defined using 'runStmt'.
lookupName :: Session -> Name -> IO (Maybe TyThing)
lookupName s name = withSession s $ \hsc_env -> do
  case lookupTypeEnv (ic_type_env (hsc_IC hsc_env)) name of
	Just tt -> return (Just tt)
	Nothing -> do
	    eps <- readIORef (hsc_EPS hsc_env)
	    return $! lookupType (hsc_HPT hsc_env) (eps_PTE eps) name

-- | Container for information about a 'Module'.
data ModuleInfo = ModuleInfo {
	minf_details  :: ModDetails,
	minf_rdr_env  :: Maybe GlobalRdrEnv
  }
	-- ToDo: this should really contain the ModIface too
	-- We don't want HomeModInfo here, because a ModuleInfo applies
	-- to package modules too.

-- | Request information about a loaded 'Module'
getModuleInfo :: Session -> Module -> IO (Maybe ModuleInfo)
getModuleInfo s mdl = withSession s $ \hsc_env -> do
  case lookupModuleEnv (hsc_HPT hsc_env) mdl of
    Nothing  -> return Nothing
    Just hmi -> 
	return (Just (ModuleInfo {
			minf_details = hm_details hmi,
			minf_rdr_env = mi_globals $! hm_iface hmi
			}))

	-- ToDo: we should be able to call getModuleInfo on a package module,
	-- even one that isn't loaded yet.

-- | The list of top-level entities defined in a module
modInfoTyThings :: ModuleInfo -> [TyThing]
modInfoTyThings minf = typeEnvElts (md_types (minf_details minf))

modInfoTopLevelScope :: ModuleInfo -> Maybe [Name]
modInfoTopLevelScope minf
  = fmap (map gre_name . globalRdrEnvElts) (minf_rdr_env minf)

modInfoExports :: ModuleInfo -> [Name]
modInfoExports minf = nameSetToList $! (md_exports $! minf_details minf)

isDictonaryId :: Id -> Bool
isDictonaryId id
  = case tcSplitSigmaTy (idType id) of { (tvs, theta, tau) -> isDictTy tau }

#if 0

data ObjectCode
  = ByteCode
  | BinaryCode FilePath

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
{-# DEPRECATED getInfo "we should be using parseName/lookupName instead" #-}
getInfo :: Session -> String -> IO [GetInfoResult]
getInfo s id = withSession s $ \hsc_env -> hscGetInfo hsc_env id

-- | Returns all names in scope in the current interactive context
getNamesInScope :: Session -> IO [Name]
getNamesInScope s = withSession s $ \hsc_env -> do
  return (map gre_name (globalRdrEnvElts (ic_rn_gbl_env (hsc_IC hsc_env))))

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

{-# DEPRECATED browseModule "we should be using getModuleInfo instead" #-}
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
			 obj_linkable = isObjectLinkable (fromJust (hm_linkable mod_info))

#endif /* GHCI */
