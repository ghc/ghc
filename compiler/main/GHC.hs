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
	newSession,

	-- * Flags and settings
	DynFlags(..), DynFlag(..), Severity(..), GhcMode(..), HscTarget(..), dopt,
	parseDynamicFlags,
	getSessionDynFlags,
	setSessionDynFlags,

	-- * Targets
	Target(..), TargetId(..), Phase,
	setTargets,
	getTargets,
	addTarget,
	removeTarget,
	guessTarget,
	
        -- * Extending the program scope 
        extendGlobalRdrScope,  -- :: Session -> [GlobalRdrElt] -> IO ()
        setGlobalRdrScope,     -- :: Session -> [GlobalRdrElt] -> IO ()
        extendGlobalTypeScope, -- :: Session -> [Id] -> IO ()
        setGlobalTypeScope,    -- :: Session -> [Id] -> IO ()

	-- * Loading\/compiling the program
	depanal,
	load, LoadHowMuch(..), SuccessFlag(..),	-- also does depanal
	workingDirectoryChanged,
	checkModule, CheckedModule(..),
	TypecheckedSource, ParsedSource, RenamedSource,

	-- * Parsing Haddock comments
	parseHaddockComment,

	-- * Inspecting the module structure of the program
	ModuleGraph, ModSummary(..), ms_mod_name, ModLocation(..),
	getModuleGraph,
	isLoaded,
	topSortModuleGraph,

	-- * Inspecting modules
	ModuleInfo,
	getModuleInfo,
	modInfoTyThings,
	modInfoTopLevelScope,
	modInfoPrintUnqualified,
	modInfoExports,
	modInfoInstances,
	modInfoIsExportedName,
	modInfoLookupName,
	lookupGlobalName,

	-- * Printing
	PrintUnqualified, alwaysQualify,

	-- * Interactive evaluation
	getBindings, getPrintUnqual,
        findModule,
#ifdef GHCI
	setContext, getContext,	
	getNamesInScope,
	getRdrNamesInScope,
	moduleIsInterpreted,
	getInfo,
	exprType,
	typeKind,
	parseName,
	RunResult(..),
	runStmt,
	showModule,
	compileExpr, HValue, dynCompileExpr,
	lookupName,

        obtainTerm,  
#endif

	-- * Abstract syntax elements

        -- ** Packages
        PackageId,

	-- ** Modules
	Module, mkModule, pprModule, moduleName, modulePackageId,
        ModuleName, mkModuleName, moduleNameString,

	-- ** Names
	Name, 
	nameModule, pprParenSymName, nameSrcLoc,
	NamedThing(..),
	RdrName(Qual,Unqual),
	
	-- ** Identifiers
	Id, idType,
	isImplicitId, isDeadBinder,
	isExportedId, isLocalId, isGlobalId,
	isRecordSelector,
	isPrimOpId, isFCallId, isClassOpId_maybe,
	isDataConWorkId, idDataCon,
	isBottomingId, isDictonaryId,
	recordSelectorFieldLabel,

	-- ** Type constructors
	TyCon, 
	tyConTyVars, tyConDataCons, tyConArity,
	isClassTyCon, isSynTyCon, isNewTyCon, isPrimTyCon, isFunTyCon,
	isOpenTyCon,
	synTyConDefn, synTyConType, synTyConResKind,

	-- ** Type variables
	TyVar,
	alphaTyVars,

	-- ** Data constructors
	DataCon,
	dataConSig, dataConType, dataConTyCon, dataConFieldLabels,
	dataConIsInfix, isVanillaDataCon,
	dataConStrictMarks,  
	StrictnessMark(..), isMarkedStrict,

	-- ** Classes
	Class, 
	classMethods, classSCTheta, classTvsFds,
	pprFundeps,

	-- ** Instances
	Instance, 
	instanceDFunId, pprInstance, pprInstanceHdr,

	-- ** Types and Kinds
	Type, dropForAlls, splitForAllTys, funResultTy, pprParendType,
	Kind,
	PredType,
	ThetaType, pprThetaArrow,

	-- ** Entities
	TyThing(..), 

	-- ** Syntax
	module HsSyn, -- ToDo: remove extraneous bits

	-- ** Fixities
	FixityDirection(..), 
	defaultFixity, maxPrecedence, 
	negateFixity,
	compareFixity,

	-- ** Source locations
	SrcLoc, pprDefnLoc,

	-- * Exceptions
	GhcException(..), showGhcException,

	-- * Miscellaneous
	sessionHscEnv,
	cyclicModuleErr,
  ) where

{-
 ToDo:

  * inline bits of HscMain here to simplify layering: hscTcExpr, hscStmt.
  * what StaticFlags should we expose, if any?
-}

#include "HsVersions.h"

#ifdef GHCI
import TcRnDriver	( tcRnLookupRdrName, tcRnGetInfo,
			  tcRnLookupName, getModuleExports )
import RdrName		( plusGlobalRdrEnv, Provenance(..), 
			  ImportSpec(..), ImpDeclSpec(..), ImpItemSpec(..),
			  mkGlobalRdrEnv )
import HscMain		( hscParseIdentifier, hscStmt, hscTcExpr, hscKcType )
import Name		( nameOccName )
import Type		( tidyType )
import VarEnv		( emptyTidyEnv )
import GHC.Exts         ( unsafeCoerce# )

-- For breakpoints
import Breakpoints      ( SiteNumber, Coord, nullBkptHandler, 
                          BkptHandler(..), BkptLocation, noDbgSites )
import Linker           ( initDynLinker )
import PrelNames        ( breakpointJumpName, breakpointCondJumpName, 
                          breakpointAutoJumpName )

import GHC.Exts         ( Int(..), Ptr(..), int2Addr#, indexArray# )
import GHC.Base         ( Opaque(..) )
import Foreign.StablePtr( deRefStablePtr, castPtrToStablePtr )
import Foreign          ( unsafePerformIO )
import Data.Maybe       ( fromMaybe)
import qualified Linker

import Data.Dynamic     ( Dynamic )
import RtClosureInspect ( cvObtainTerm, Term )
import Linker		( HValue, getHValue, extendLinkEnv )
#endif

import Packages		( initPackages )
import NameSet		( NameSet, nameSetToList, elemNameSet )
import RdrName		( GlobalRdrEnv, GlobalRdrElt(..), RdrName(..), 
			  globalRdrEnvElts, extendGlobalRdrEnv,
                          emptyGlobalRdrEnv )
import HsSyn 
import Type		( Kind, Type, dropForAlls, PredType, ThetaType,
			  pprThetaArrow, pprParendType, splitForAllTys,
			  funResultTy )
import Id		( Id, idType, isImplicitId, isDeadBinder,
                          isExportedId, isLocalId, isGlobalId,
                          isRecordSelector, recordSelectorFieldLabel,
                          isPrimOpId, isFCallId, isClassOpId_maybe,
                          isDataConWorkId, idDataCon,
                          isBottomingId )
import Var		( TyVar, varName )
import TysPrim		( alphaTyVars )
import TyCon		( TyCon, isClassTyCon, isSynTyCon, isNewTyCon,
			  isPrimTyCon, isFunTyCon, isOpenTyCon, tyConArity,
			  tyConTyVars, tyConDataCons, synTyConDefn,
			  synTyConType, synTyConResKind )
import Class		( Class, classSCTheta, classTvsFds, classMethods )
import FunDeps		( pprFundeps )
import DataCon		( DataCon, dataConWrapId, dataConSig, dataConTyCon,
			  dataConFieldLabels, dataConStrictMarks, 
			  dataConIsInfix, isVanillaDataCon )
import Name		( Name, nameModule, NamedThing(..), nameSrcLoc )
import OccName		( parenSymOcc )
import NameEnv		( nameEnvElts )
import InstEnv		( Instance, instanceDFunId, pprInstance, pprInstanceHdr )
import SrcLoc
import DriverPipeline
import DriverPhases	( Phase(..), isHaskellSrcFilename, startPhase )
import HeaderInfo	( getImports, getOptions )
import Finder
import HscMain		( newHscEnv, hscFileCheck, HscChecked(..) )
import HscTypes
import DynFlags
import SysTools     ( initSysTools, cleanTempFiles, cleanTempFilesExcept,
                      cleanTempDirs )
import Module
import UniqFM
import PackageConfig    ( PackageId, stringToPackageId )
import FiniteMap
import Panic
import Digraph
import Bag		( unitBag, listToBag )
import ErrUtils		( Severity(..), showPass, fatalErrorMsg, debugTraceMsg,
			  mkPlainErrMsg, printBagOfErrors, printBagOfWarnings,
			  WarnMsg )
import qualified ErrUtils
import Util
import StringBuffer	( StringBuffer, hGetStringBuffer )
import Outputable
import BasicTypes
import TcType           ( tcSplitSigmaTy, isDictTy )
import Maybes		( expectJust, mapCatMaybes )
import HaddockParse     ( parseHaddockParagraphs, parseHaddockString )
import HaddockLex       ( tokenise )

import Control.Concurrent
import System.Directory ( getModificationTime, doesFileExist )
import Data.Maybe	( isJust, isNothing )
import Data.List	( partition, nub )
import qualified Data.List as List
import Control.Monad	( unless, when )
import System.Exit	( exitWith, ExitCode(..) )
import System.Time	( ClockTime )
import Control.Exception as Exception hiding (handle)
import Data.IORef
import Data.Traversable ( traverse )
import System.IO
import System.IO.Error	( isDoesNotExistError )
import Prelude hiding (init)

#if __GLASGOW_HASKELL__ < 600
import System.IO as System.IO.Error ( try )
#else
import System.IO.Error	( try )
#endif

-- -----------------------------------------------------------------------------
-- Exception handlers

-- | Install some default exception handlers and run the inner computation.
-- Unless you want to handle exceptions yourself, you should wrap this around
-- the top level of your program.  The default handlers output the error
-- message(s) to stderr and exit cleanly.
defaultErrorHandler :: DynFlags -> IO a -> IO a
defaultErrorHandler dflags inner = 
  -- top-level exception handler: any unrecognised exception is a compiler bug.
  handle (\exception -> do
  	   hFlush stdout
	   case exception of
		-- an IO exception probably isn't our fault, so don't panic
		IOException _ ->
		  fatalErrorMsg dflags (text (show exception))
		AsyncException StackOverflow ->
		  fatalErrorMsg dflags (text "stack overflow: use +RTS -K<size> to increase it")
		_other ->
		  fatalErrorMsg dflags (text (show (Panic (show exception))))
	   exitWith (ExitFailure 1)
         ) $

  -- program errors: messages with locations attached.  Sometimes it is
  -- convenient to just throw these as exceptions.
  handleDyn (\dyn -> do printBagOfErrors dflags (unitBag dyn)
			exitWith (ExitFailure 1)) $

  -- error messages propagated as exceptions
  handleDyn (\dyn -> do
  		hFlush stdout
  		case dyn of
		     PhaseFailed _ code -> exitWith code
		     Interrupted -> exitWith (ExitFailure 1)
		     _ -> do fatalErrorMsg dflags (text (show (dyn :: GhcException)))
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
               do cleanTempFiles dflags
                  cleanTempDirs dflags
          )
          -- exceptions will be blocked while we clean the temporary files,
          -- so there shouldn't be any difficulty if we receive further
          -- signals.
    inner


-- | Starts a new session.  A session consists of a set of loaded
-- modules, a set of options (DynFlags), and an interactive context.
-- ToDo: GhcMode should say "keep typechecked code" and\/or "keep renamed
-- code".
newSession :: GhcMode -> Maybe FilePath -> IO Session
newSession mode mb_top_dir = do
  -- catch ^C
  main_thread <- myThreadId
  modifyMVar_ interruptTargetThread (return . (main_thread :))
  installSignalHandlers

  dflags0 <- initSysTools mb_top_dir defaultDynFlags
  dflags  <- initDynFlags dflags0
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

-- | Updates the DynFlags in a Session.  This also reads
-- the package database (unless it has already been read),
-- and prepares the compilers knowledge about packages.  It
-- can be called again to load new packages: just add new
-- package flags to (packageFlags dflags).
--
-- Returns a list of new packages that may need to be linked in using
-- the dynamic linker (see 'linkPackages') as a result of new package
-- flags.  If you are not doing linking or doing static linking, you
-- can ignore the list of packages returned.
--
setSessionDynFlags :: Session -> DynFlags -> IO [PackageId]
setSessionDynFlags (Session ref) dflags = do
  hsc_env <- readIORef ref
  (dflags', preload) <- initPackages dflags
  writeIORef ref $! hsc_env{ hsc_dflags = dflags' }
  return preload

-- | If there is no -o option, guess the name of target executable
-- by using top-level source file name as a base.
guessOutputFile :: Session -> IO ()
guessOutputFile s = modifySession s $ \env ->
    let dflags = hsc_dflags env
        mod_graph = hsc_mod_graph env
        mainModuleSrcPath, guessedName :: Maybe String
        mainModuleSrcPath = do
            let isMain = (== mainModIs dflags) . ms_mod
            [ms] <- return (filter isMain mod_graph)
            ml_hs_file (ms_location ms)
        guessedName = fmap basenameOf mainModuleSrcPath
    in
    case outputFile dflags of
        Just _ -> env
        Nothing -> env { hsc_dflags = dflags { outputFile = guessedName } }

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
guessTarget :: String -> Maybe Phase -> IO Target
guessTarget file (Just phase)
   = return (Target (TargetFile file (Just phase)) Nothing)
guessTarget file Nothing
   | isHaskellSrcFilename file
   = return (Target (TargetFile file Nothing) Nothing)
   | otherwise
   = do exists <- doesFileExist hs_file
	if exists
	   then return (Target (TargetFile hs_file Nothing) Nothing)
	   else do
	exists <- doesFileExist lhs_file
	if exists
	   then return (Target (TargetFile lhs_file Nothing) Nothing)
	   else do
	return (Target (TargetModule (mkModuleName file)) Nothing)
     where 
	 hs_file  = file `joinFileExt` "hs"
	 lhs_file = file `joinFileExt` "lhs"

-- -----------------------------------------------------------------------------
-- Extending the program scope

extendGlobalRdrScope :: Session -> [GlobalRdrElt] -> IO ()
extendGlobalRdrScope session rdrElts
    = modifySession session $ \hscEnv ->
      let global_rdr = hsc_global_rdr_env hscEnv
      in hscEnv{ hsc_global_rdr_env = foldl extendGlobalRdrEnv global_rdr rdrElts }

setGlobalRdrScope :: Session -> [GlobalRdrElt] -> IO ()
setGlobalRdrScope session rdrElts
    = modifySession session $ \hscEnv ->
      hscEnv{ hsc_global_rdr_env = foldl extendGlobalRdrEnv emptyGlobalRdrEnv rdrElts }

extendGlobalTypeScope :: Session -> [Id] -> IO ()
extendGlobalTypeScope session ids
    = modifySession session $ \hscEnv ->
      let global_type = hsc_global_type_env hscEnv
      in hscEnv{ hsc_global_type_env = extendTypeEnvWithIds global_type ids }

setGlobalTypeScope :: Session -> [Id] -> IO ()
setGlobalTypeScope session ids
    = modifySession session $ \hscEnv ->
      hscEnv{ hsc_global_type_env = extendTypeEnvWithIds emptyTypeEnv ids }

-- -----------------------------------------------------------------------------
-- Parsing Haddock comments

parseHaddockComment :: String -> Either String (HsDoc RdrName)
parseHaddockComment string = parseHaddockParagraphs (tokenise string)

-- -----------------------------------------------------------------------------
-- Loading the program

-- Perform a dependency analysis starting from the current targets
-- and update the session with the new module graph.
depanal :: Session -> [ModuleName] -> Bool -> IO (Maybe ModuleGraph)
depanal (Session ref) excluded_mods allow_dup_roots = do
  hsc_env <- readIORef ref
  let
	 dflags  = hsc_dflags hsc_env
	 gmode   = ghcMode (hsc_dflags hsc_env)
	 targets = hsc_targets hsc_env
	 old_graph = hsc_mod_graph hsc_env
	
  showPass dflags "Chasing dependencies"
  when (gmode == BatchCompile) $
	debugTraceMsg dflags 2 (hcat [
		     text "Chasing modules from: ",
	     		hcat (punctuate comma (map pprTarget targets))])

  r <- downsweep hsc_env old_graph excluded_mods allow_dup_roots
  case r of
    Just mod_graph -> writeIORef ref hsc_env{ hsc_mod_graph = mod_graph }
    _ -> return ()
  return r

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
   | LoadUpTo ModuleName
   | LoadDependenciesOf ModuleName

-- | Try to load the program.  If a Module is supplied, then just
-- attempt to load up to this target.  If no Module is supplied,
-- then try to load all targets.
load :: Session -> LoadHowMuch -> IO SuccessFlag
load s@(Session ref) how_much
   = do 
	-- Dependency analysis first.  Note that this fixes the module graph:
	-- even if we don't get a fully successful upsweep, the full module
	-- graph is still retained in the Session.  We can tell which modules
	-- were successfully loaded by inspecting the Session's HPT.
	mb_graph <- depanal s [] False
	case mb_graph of	   
	   Just mod_graph -> load2 s how_much mod_graph 
	   Nothing        -> return Failed

load2 s@(Session ref) how_much mod_graph = do
        guessOutputFile s
	hsc_env <- readIORef ref

        let hpt1      = hsc_HPT hsc_env
        let dflags    = hsc_dflags hsc_env
        let ghci_mode = ghcMode dflags -- this never changes

	-- The "bad" boot modules are the ones for which we have
	-- B.hs-boot in the module graph, but no B.hs
	-- The downsweep should have ensured this does not happen
	-- (see msDeps)
        let all_home_mods = [ms_mod_name s 
			    | s <- mod_graph, not (isBootSummary s)]
#ifdef DEBUG
	    bad_boot_mods = [s 	      | s <- mod_graph, isBootSummary s,
					not (ms_mod_name s `elem` all_home_mods)]
#endif
	ASSERT( null bad_boot_mods ) return ()

        -- mg2_with_srcimps drops the hi-boot nodes, returning a 
	-- graph with cycles.  Among other things, it is used for
        -- backing out partially complete cycles following a failed
        -- upsweep, and for removing from hpt all the modules
        -- not in strict downwards closure, during calls to compile.
        let mg2_with_srcimps :: [SCC ModSummary]
	    mg2_with_srcimps = topSortModuleGraph True mod_graph Nothing

	-- If we can determine that any of the {-# SOURCE #-} imports
	-- are definitely unnecessary, then emit a warning.
	warnUnnecessarySourceImports dflags mg2_with_srcimps

 	let
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

	debugTraceMsg dflags 2 (text "Stable obj:" <+> ppr stable_obj $$
				text "Stable BCO:" <+> ppr stable_bco)

	-- Unload any modules which are going to be re-linked this time around.
	let stable_linkables = [ linkable
			       | m <- stable_obj++stable_bco,
				 Just hmi <- [lookupUFM pruned_hpt m],
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
			    AcyclicSCC ms -> ms_mod_name ms == mod; _ -> False )
		  List.init partial_mg0
		| otherwise
		= partial_mg0
  
	    stable_mg = 
		[ AcyclicSCC ms
	        | AcyclicSCC ms <- full_mg,
		  ms_mod_name ms `elem` stable_obj++stable_bco,
		  ms_mod_name ms `notElem` [ ms_mod_name ms' | 
						AcyclicSCC ms' <- partial_mg ] ]

	    mg = stable_mg ++ partial_mg

	-- clean up between compilations
	let cleanup = cleanTempFilesExcept dflags
			  (ppFilesFromSummaries (flattenSCCs mg2_with_srcimps))

	debugTraceMsg dflags 2 (hang (text "Ready for upsweep") 
				   2 (ppr mg))
        (upsweep_ok, hsc_env1, modsUpswept)
           <- upsweep (hsc_env { hsc_HPT = emptyHomePackageTable })
			   pruned_hpt stable_mods cleanup mg

	-- Make modsDone be the summaries for each home module now
	-- available; this should equal the domain of hpt3.
        -- Get in in a roughly top .. bottom order (hence reverse).

        let modsDone = reverse modsUpswept

        -- Try and do linking in some form, depending on whether the
        -- upsweep was completely or only partially successful.

        if succeeded upsweep_ok

         then 
           -- Easy; just relink it all.
           do debugTraceMsg dflags 2 (text "Upsweep completely successful.")

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
	      let 
	 	main_mod = mainModIs dflags
		a_root_is_Main = any ((==main_mod).ms_mod) mod_graph
		do_linking = a_root_is_Main || no_hs_main

	      when (ghci_mode == BatchCompile && isJust ofile && not do_linking) $
	        debugTraceMsg dflags 1 (text ("Warning: output was redirected with -o, " ++
				              "but no output will be generated\n" ++
				              "because there is no " ++ moduleNameString (moduleName main_mod) ++ " module."))

	      -- link everything together
              linkresult <- link ghci_mode dflags do_linking (hsc_HPT hsc_env1)

	      loadFinish Succeeded linkresult ref hsc_env1

         else 
           -- Tricky.  We need to back out the effects of compiling any
           -- half-done cycles, both so as to clean up the top level envs
           -- and to avoid telling the interactive linker to link them.
           do debugTraceMsg dflags 2 (text "Upsweep partially successful.")

              let modsDone_names
                     = map ms_mod modsDone
              let mods_to_zap_names 
                     = findPartiallyCompletedCycles modsDone_names 
			  mg2_with_srcimps
              let mods_to_keep
                     = filter ((`notElem` mods_to_zap_names).ms_mod) 
			  modsDone

              let hpt4 = retainInTopLevelEnvs (map ms_mod_name mods_to_keep) 
					      (hsc_HPT hsc_env1)

	      -- Clean up after ourselves
	      cleanTempFilesExcept dflags (ppFilesFromSummaries mods_to_keep)

	      -- there should be no Nothings where linkables should be, now
	      ASSERT(all (isJust.hm_linkable) 
			(eltsUFM (hsc_HPT hsc_env))) do
	
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
ppFilesFromSummaries summaries = map ms_hspp_file summaries

-- -----------------------------------------------------------------------------
-- Check module

data CheckedModule = 
  CheckedModule { parsedSource      :: ParsedSource,
		  renamedSource     :: Maybe RenamedSource,
		  typecheckedSource :: Maybe TypecheckedSource,
		  checkedModuleInfo :: Maybe ModuleInfo
	        }
	-- ToDo: improvements that could be made here:
	--  if the module succeeded renaming but not typechecking,
	--  we can still get back the GlobalRdrEnv and exports, so
	--  perhaps the ModuleInfo should be split up into separate
	--  fields within CheckedModule.

type ParsedSource      = Located (HsModule RdrName)
type RenamedSource     = (HsGroup Name, [LImportDecl Name], Maybe [LIE Name],
                          Maybe (HsDoc Name), HaddockModInfo Name)
type TypecheckedSource = LHsBinds Id

-- NOTE:
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


-- | This is the way to get access to parsed and typechecked source code
-- for a module.  'checkModule' loads all the dependencies of the specified
-- module in the Session, and then attempts to typecheck the module.  If
-- successful, it returns the abstract syntax for the module.
checkModule :: Session -> ModuleName -> IO (Maybe CheckedModule)
checkModule session@(Session ref) mod = do
	-- load up the dependencies first
   r <- load session (LoadDependenciesOf mod)
   if (failed r) then return Nothing else do

	-- now parse & typecheck the module
   hsc_env <- readIORef ref   
   let mg  = hsc_mod_graph hsc_env
   case [ ms | ms <- mg, ms_mod_name ms == mod ] of
	[] -> return Nothing
	(ms:_) -> do 
	   mbChecked <- hscFileCheck hsc_env{hsc_dflags=ms_hspp_opts ms} ms
	   case mbChecked of
             Nothing -> return Nothing
             Just (HscChecked parsed renamed Nothing) ->
		   return (Just (CheckedModule {
					parsedSource = parsed,
					renamedSource = renamed,
					typecheckedSource = Nothing,
					checkedModuleInfo = Nothing }))
             Just (HscChecked parsed renamed
			   (Just (tc_binds, rdr_env, details))) -> do
		   let minf = ModuleInfo {
				minf_type_env  = md_types details,
				minf_exports   = availsToNameSet $
                                                     md_exports details,
				minf_rdr_env   = Just rdr_env,
				minf_instances = md_insts details
			      }
		   return (Just (CheckedModule {
					parsedSource = parsed,
					renamedSource = renamed,
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
	-> [ModuleName]			-- all home modules
	-> ([ModuleName],		-- stableObject
	    [ModuleName])		-- stableBCO

checkStability hpt sccs all_home_mods = foldl checkSCC ([],[]) sccs
  where
   checkSCC (stable_obj, stable_bco) scc0
     | stableObjects = (scc_mods ++ stable_obj, stable_bco)
     | stableBCOs    = (stable_obj, scc_mods ++ stable_bco)
     | otherwise     = (stable_obj, stable_bco)
     where
	scc = flattenSCC scc0
	scc_mods = map ms_mod_name scc
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
	     same_as_prev t = case lookupUFM hpt (ms_mod_name ms) of
				Just hmi  | Just l <- hm_linkable hmi
				 -> isObjectLinkable l && t == linkableTime l
				_other  -> True
		-- why '>=' rather than '>' above?  If the filesystem stores
		-- times to the nearset second, we may occasionally find that
		-- the object & source have the same modification time, 
		-- especially if the source was automatically generated
		-- and compiled.  Using >= is slightly unsafe, but it matches
		-- make's behaviour.

	bco_ok ms
	  = case lookupUFM hpt (ms_mod_name ms) of
		Just hmi  | Just l <- hm_linkable hmi ->
			not (isObjectLinkable l) && 
			linkableTime l >= ms_hs_date ms
		_other  -> False

ms_allimps :: ModSummary -> [ModuleName]
ms_allimps ms = map unLoc (ms_srcimps ms ++ ms_imps ms)

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
   -> ([ModuleName],[ModuleName])
   -> HomePackageTable

pruneHomePackageTable hpt summ (stable_obj, stable_bco)
  = mapUFM prune hpt
  where prune hmi
	  | is_stable modl = hmi'
	  | otherwise      = hmi'{ hm_details = emptyModDetails }
	  where
	   modl = moduleName (mi_module (hm_iface hmi))
	   hmi' | Just l <- hm_linkable hmi, linkableTime l < ms_hs_date ms
		= hmi{ hm_linkable = Nothing }
		| otherwise
		= hmi
		where ms = expectJust "prune" (lookupUFM ms_map modl)

        ms_map = listToUFM [(ms_mod_name ms, ms) | ms <- summ]

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
    -> ([ModuleName],[ModuleName]) -- stable modules (see checkStability)
    -> IO ()			-- How to clean up unwanted tmp files
    -> [SCC ModSummary]		-- Mods to do (the worklist)
    -> IO (SuccessFlag,
           HscEnv,		-- With an updated HPT
           [ModSummary])	-- Mods which succeeded

upsweep hsc_env old_hpt stable_mods cleanup mods
   = upsweep' hsc_env old_hpt stable_mods cleanup mods 1 (length mods)

upsweep' hsc_env old_hpt stable_mods cleanup
     [] _ _
   = return (Succeeded, hsc_env, [])

upsweep' hsc_env old_hpt stable_mods cleanup
     (CyclicSCC ms:_) _ _
   = do fatalErrorMsg (hsc_dflags hsc_env) (cyclicModuleErr ms)
        return (Failed, hsc_env, [])

upsweep' hsc_env old_hpt stable_mods cleanup
     (AcyclicSCC mod:mods) mod_index nmods
   = do -- putStrLn ("UPSWEEP_MOD: hpt = " ++ 
	--	     show (map (moduleUserString.moduleName.mi_module.hm_iface) 
	--		       (moduleEnvElts (hsc_HPT hsc_env)))

        mb_mod_info <- upsweep_mod hsc_env old_hpt stable_mods mod 
                       mod_index nmods

	cleanup		-- Remove unwanted tmp files between compilations

        case mb_mod_info of
	    Nothing -> return (Failed, hsc_env, [])
	    Just mod_info -> do 
		{ let this_mod = ms_mod_name mod

			-- Add new info to hsc_env
		      hpt1     = addToUFM (hsc_HPT hsc_env) this_mod mod_info
		      hsc_env1 = hsc_env { hsc_HPT = hpt1 }

			-- Space-saving: delete the old HPT entry
			-- for mod BUT if mod is a hs-boot
			-- node, don't delete it.  For the
			-- interface, the HPT entry is probaby for the
			-- main Haskell source file.  Deleting it
			-- would force .. (what?? --SDM)
		      old_hpt1 | isBootSummary mod = old_hpt
			       | otherwise = delFromUFM old_hpt this_mod

		; (restOK, hsc_env2, modOKs) 
			<- upsweep' hsc_env1 old_hpt1 stable_mods cleanup 
				mods (mod_index+1) nmods
		; return (restOK, hsc_env2, mod:modOKs)
		}


-- Compile a single module.  Always produce a Linkable for it if 
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: HscEnv
            -> HomePackageTable
	    -> ([ModuleName],[ModuleName])
            -> ModSummary
            -> Int  -- index of module
            -> Int  -- total number of modules
            -> IO (Maybe HomeModInfo)	-- Nothing => Failed

upsweep_mod hsc_env old_hpt (stable_obj, stable_bco) summary mod_index nmods
   = do 
        let 
	    this_mod_name = ms_mod_name summary
	    this_mod    = ms_mod summary
	    mb_obj_date = ms_obj_date summary
	    obj_fn	= ml_obj_file (ms_location summary)
	    hs_date     = ms_hs_date summary

	    compile_it :: Maybe Linkable -> IO (Maybe HomeModInfo)
	    compile_it  = upsweep_compile hsc_env old_hpt this_mod_name 
				summary mod_index nmods

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
		    is_stable_obj = this_mod_name `elem` stable_obj
		    is_stable_bco = this_mod_name `elem` stable_bco

		    old_hmi = lookupUFM old_hpt this_mod_name

-- Run hsc to compile a module
upsweep_compile hsc_env old_hpt this_mod summary
                mod_index nmods
                mb_old_linkable = do
  let
	-- The old interface is ok if it's in the old HPT 
	--	a) we're compiling a source file, and the old HPT
	--	   entry is for a source file
	--	b) we're compiling a hs-boot file
	-- Case (b) allows an hs-boot file to get the interface of its
	-- real source file on the second iteration of the compilation
	-- manager, but that does no harm.  Otherwise the hs-boot file
	-- will always be recompiled

        mb_old_iface 
		= case lookupUFM old_hpt this_mod of
		     Nothing	 			  -> Nothing
		     Just hm_info | isBootSummary summary -> Just iface
				  | not (mi_boot iface)   -> Just iface
				  | otherwise		  -> Nothing
				   where 
				     iface = hm_iface hm_info

  compresult <- compile hsc_env summary mb_old_linkable mb_old_iface
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
retainInTopLevelEnvs :: [ModuleName] -> HomePackageTable -> HomePackageTable
retainInTopLevelEnvs keep_these hpt
   = listToUFM   [ (mod, expectJust "retain" mb_mod_info)
		 | mod <- keep_these
		 , let mb_mod_info = lookupUFM hpt mod
		 , isJust mb_mod_info ]

-- ---------------------------------------------------------------------------
-- Topological sort of the module graph

topSortModuleGraph
	  :: Bool 		-- Drop hi-boot nodes? (see below)
	  -> [ModSummary]
	  -> Maybe ModuleName
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
  -> ([(ModSummary, Int, [Int])], HscSource -> ModuleName -> Maybe Int)
moduleGraphNodes drop_hs_boot_nodes summaries = (nodes, lookup_key)
   where
	-- Drop hs-boot nodes by using HsSrcFile as the key
	hs_boot_key | drop_hs_boot_nodes = HsSrcFile
		    | otherwise		 = HsBootFile	

	-- We use integers as the keys for the SCC algorithm
	nodes :: [(ModSummary, Int, [Int])]	
	nodes = [(s, expectJust "topSort" $ 
			lookup_key (ms_hsc_src s) (ms_mod_name s),
		     out_edge_keys hs_boot_key (map unLoc (ms_srcimps s)) ++
		     out_edge_keys HsSrcFile   (map unLoc (ms_imps s)) ++
		     (-- see [boot-edges] below
		      if drop_hs_boot_nodes || ms_hsc_src s == HsBootFile 
			then [] 
			else case lookup_key HsBootFile (ms_mod_name s) of
				Nothing -> []
				Just k  -> [k])
		 )
		| s <- summaries
		, not (isBootSummary s && drop_hs_boot_nodes) ]
		-- Drop the hi-boot ones if told to do so

	-- [boot-edges] if this is a .hs and there is an equivalent
	-- .hs-boot, add a link from the former to the latter.  This
	-- has the effect of detecting bogus cases where the .hs-boot
	-- depends on the .hs, by introducing a cycle.  Additionally,
	-- it ensures that we will always process the .hs-boot before
	-- the .hs, and so the HomePackageTable will always have the
	-- most up to date information.

	key_map :: NodeMap Int
	key_map = listToFM ([(moduleName (ms_mod s), ms_hsc_src s)
			    | s <- summaries]
			   `zip` [1..])

	lookup_key :: HscSource -> ModuleName -> Maybe Int
	lookup_key hs_src mod = lookupFM key_map (mod, hs_src)

	out_edge_keys :: HscSource -> [ModuleName] -> [Int]
        out_edge_keys hi_boot ms = mapCatMaybes (lookup_key hi_boot) ms
		-- If we want keep_hi_boot_nodes, then we do lookup_key with
		-- the IsBootInterface parameter True; else False


type NodeKey   = (ModuleName, HscSource)  -- The nodes of the graph are 
type NodeMap a = FiniteMap NodeKey a	  -- keyed by (mod, src_file_type) pairs

msKey :: ModSummary -> NodeKey
msKey (ModSummary { ms_mod = mod, ms_hsc_src = boot }) = (moduleName mod,boot)

mkNodeMap :: [ModSummary] -> NodeMap ModSummary
mkNodeMap summaries = listToFM [ (msKey s, s) | s <- summaries]
	
nodeMapElts :: NodeMap a -> [a]
nodeMapElts = eltsFM

ms_mod_name :: ModSummary -> ModuleName
ms_mod_name = moduleName . ms_mod

-- If there are {-# SOURCE #-} imports between strongly connected
-- components in the topological sort, then those imports can
-- definitely be replaced by ordinary non-SOURCE imports: if SOURCE
-- were necessary, then the edge would be part of a cycle.
warnUnnecessarySourceImports :: DynFlags -> [SCC ModSummary] -> IO ()
warnUnnecessarySourceImports dflags sccs = 
  printBagOfWarnings dflags (listToBag (concat (map (check.flattenSCC) sccs)))
  where check ms =
	   let mods_in_this_cycle = map ms_mod_name ms in
	   [ warn m i | m <- ms, i <- ms_srcimps m,
			unLoc i `notElem`  mods_in_this_cycle ]

	warn :: ModSummary -> Located ModuleName -> WarnMsg
	warn ms (L loc mod) = 
	   mkPlainErrMsg loc
		(ptext SLIT("Warning: {-# SOURCE #-} unnecessary in import of ")
		 <+> quotes (ppr mod))

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
	  -> [ModuleName]	-- Ignore dependencies on these; treat
				-- them as if they were package modules
	  -> Bool		-- True <=> allow multiple targets to have 
				-- 	    the same module name; this is 
				--	    very useful for ghc -M
	  -> IO (Maybe [ModSummary])
		-- The elts of [ModSummary] all have distinct
		-- (Modules, IsBoot) identifiers, unless the Bool is true
		-- in which case there can be repeats
downsweep hsc_env old_summaries excl_mods allow_dup_roots
   = -- catch error messages and return them
     handleDyn (\err_msg -> printBagOfErrors (hsc_dflags hsc_env) (unitBag err_msg) >> return Nothing) $ do
       rootSummaries <- mapM getRootSummary roots
       let root_map = mkRootMap rootSummaries
       checkDuplicates root_map
       summs <- loop (concatMap msDeps rootSummaries) root_map
       return (Just summs)
     where
	roots = hsc_targets hsc_env

	old_summary_map :: NodeMap ModSummary
	old_summary_map = mkNodeMap old_summaries

	getRootSummary :: Target -> IO ModSummary
	getRootSummary (Target (TargetFile file mb_phase) maybe_buf)
	   = do exists <- doesFileExist file
		if exists 
		    then summariseFile hsc_env old_summaries file mb_phase maybe_buf
		    else throwDyn $ mkPlainErrMsg noSrcSpan $
			   text "can't find file:" <+> text file
	getRootSummary (Target (TargetModule modl) maybe_buf)
 	   = do maybe_summary <- summariseModule hsc_env old_summary_map False 
					   (L rootLoc modl) maybe_buf excl_mods
		case maybe_summary of
		   Nothing -> packageModErr modl
		   Just s  -> return s

	rootLoc = mkGeneralSrcSpan FSLIT("<command line>")

	-- In a root module, the filename is allowed to diverge from the module
	-- name, so we have to check that there aren't multiple root files
	-- defining the same module (otherwise the duplicates will be silently
 	-- ignored, leading to confusing behaviour).
	checkDuplicates :: NodeMap [ModSummary] -> IO ()
	checkDuplicates root_map 
	   | allow_dup_roots = return ()
	   | null dup_roots  = return ()
	   | otherwise	     = multiRootsErr (head dup_roots)
	   where
	     dup_roots :: [[ModSummary]]	-- Each at least of length 2
	     dup_roots = filterOut isSingleton (nodeMapElts root_map)

	loop :: [(Located ModuleName,IsBootInterface)]
			-- Work list: process these modules
	     -> NodeMap [ModSummary]
		 	-- Visited set; the range is a list because
			-- the roots can have the same module names
			-- if allow_dup_roots is True
	     -> IO [ModSummary]
			-- The result includes the worklist, except
			-- for those mentioned in the visited set
	loop [] done 	  = return (concat (nodeMapElts done))
	loop ((wanted_mod, is_boot) : ss) done 
	  | Just summs <- lookupFM done key
	  = if isSingleton summs then
		loop ss done
	    else
		do { multiRootsErr summs; return [] }
	  | otherwise	      = do { mb_s <- summariseModule hsc_env old_summary_map 
						 is_boot wanted_mod Nothing excl_mods
				   ; case mb_s of
					Nothing -> loop ss done
					Just s  -> loop (msDeps s ++ ss) 
							(addToFM done key [s]) }
	  where
	    key = (unLoc wanted_mod, if is_boot then HsBootFile else HsSrcFile)

mkRootMap :: [ModSummary] -> NodeMap [ModSummary]
mkRootMap summaries = addListToFM_C (++) emptyFM 
			[ (msKey s, [s]) | s <- summaries ]

msDeps :: ModSummary -> [(Located ModuleName, IsBootInterface)]
-- (msDeps s) returns the dependencies of the ModSummary s.
-- A wrinkle is that for a {-# SOURCE #-} import we return
--	*both* the hs-boot file
--	*and* the source file
-- as "dependencies".  That ensures that the list of all relevant
-- modules always contains B.hs if it contains B.hs-boot.
-- Remember, this pass isn't doing the topological sort.  It's
-- just gathering the list of all relevant ModSummaries
msDeps s = 
    concat [ [(m,True), (m,False)] | m <- ms_srcimps s ] 
	 ++ [ (m,False) | m <- ms_imps s ] 

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

summariseFile
	:: HscEnv
	-> [ModSummary]			-- old summaries
	-> FilePath			-- source file name
	-> Maybe Phase			-- start phase
	-> Maybe (StringBuffer,ClockTime)
	-> IO ModSummary

summariseFile hsc_env old_summaries file mb_phase maybe_buf
	-- we can use a cached summary if one is available and the
	-- source file hasn't changed,  But we have to look up the summary
	-- by source file, rather than module name as we do in summarise.
   | Just old_summary <- findSummaryBySourceFile old_summaries file
   = do
	let location = ms_location old_summary

		-- return the cached summary if the source didn't change
	src_timestamp <- case maybe_buf of
			   Just (_,t) -> return t
			   Nothing    -> getModificationTime file
		-- The file exists; we checked in getRootSummary above.
		-- If it gets removed subsequently, then this 
		-- getModificationTime may fail, but that's the right
		-- behaviour.

	if ms_hs_date old_summary == src_timestamp 
	   then do -- update the object-file timestamp
		  obj_timestamp <- getObjTimestamp location False
		  return old_summary{ ms_obj_date = obj_timestamp }
	   else
		new_summary

   | otherwise
   = new_summary
  where
    new_summary = do
   	let dflags = hsc_dflags hsc_env

	(dflags', hspp_fn, buf)
	    <- preprocessFile dflags file mb_phase maybe_buf

        (srcimps,the_imps, L _ mod_name) <- getImports dflags' buf hspp_fn

	-- Make a ModLocation for this file
	location <- mkHomeModLocation dflags mod_name file

	-- Tell the Finder cache where it is, so that subsequent calls
	-- to findModule will find it, even if it's not on any search path
	mod <- addHomeModuleToFinder hsc_env mod_name location

        src_timestamp <- case maybe_buf of
			   Just (_,t) -> return t
			   Nothing    -> getModificationTime file
			-- getMofificationTime may fail

	obj_timestamp <- modificationTimeIfExists (ml_obj_file location)

        return (ModSummary { ms_mod = mod, ms_hsc_src = HsSrcFile,
			     ms_location = location,
                             ms_hspp_file = hspp_fn,
                             ms_hspp_opts = dflags',
			     ms_hspp_buf  = Just buf,
                             ms_srcimps = srcimps, ms_imps = the_imps,
			     ms_hs_date = src_timestamp,
			     ms_obj_date = obj_timestamp })

findSummaryBySourceFile :: [ModSummary] -> FilePath -> Maybe ModSummary
findSummaryBySourceFile summaries file
  = case [ ms | ms <- summaries, HsSrcFile <- [ms_hsc_src ms],
			         expectJust "findSummaryBySourceFile" (ml_hs_file (ms_location ms)) == file ] of
	[] -> Nothing
	(x:xs) -> Just x

-- Summarise a module, and pick up source and timestamp.
summariseModule
	  :: HscEnv
	  -> NodeMap ModSummary	-- Map of old summaries
	  -> IsBootInterface	-- True <=> a {-# SOURCE #-} import
	  -> Located ModuleName	-- Imported module to be summarised
	  -> Maybe (StringBuffer, ClockTime)
	  -> [ModuleName]		-- Modules to exclude
	  -> IO (Maybe ModSummary)	-- Its new summary

summariseModule hsc_env old_summary_map is_boot (L loc wanted_mod) maybe_buf excl_mods
  | wanted_mod `elem` excl_mods
  = return Nothing

  | Just old_summary <- lookupFM old_summary_map (wanted_mod, hsc_src)
  = do	 	-- Find its new timestamp; all the 
		-- ModSummaries in the old map have valid ml_hs_files
	let location = ms_location old_summary
	    src_fn = expectJust "summariseModule" (ml_hs_file location)

		-- check the modification time on the source file, and
		-- return the cached summary if it hasn't changed.  If the
		-- file has disappeared, we need to call the Finder again.
	case maybe_buf of
	   Just (_,t) -> check_timestamp old_summary location src_fn t
	   Nothing    -> do
		m <- System.IO.Error.try (getModificationTime src_fn)
		case m of
		   Right t -> check_timestamp old_summary location src_fn t
		   Left e | isDoesNotExistError e -> find_it
		          | otherwise             -> ioError e

  | otherwise  = find_it
  where
    dflags = hsc_dflags hsc_env

    hsc_src = if is_boot then HsBootFile else HsSrcFile

    check_timestamp old_summary location src_fn src_timestamp
	| ms_hs_date old_summary == src_timestamp = do
		-- update the object-file timestamp
	   	obj_timestamp <- getObjTimestamp location is_boot
		return (Just old_summary{ ms_obj_date = obj_timestamp })
	| otherwise = 
		-- source changed: re-summarise.
		new_summary location (ms_mod old_summary) src_fn src_timestamp

    find_it = do
	-- Don't use the Finder's cache this time.  If the module was
	-- previously a package module, it may have now appeared on the
	-- search path, so we want to consider it to be a home module.  If
	-- the module was previously a home module, it may have moved.
	uncacheModule hsc_env wanted_mod
	found <- findImportedModule hsc_env wanted_mod Nothing
	case found of
	     Found location mod 
		| isJust (ml_hs_file location) ->
			-- Home package
			 just_found location mod
		| otherwise -> 
			-- Drop external-pkg
			ASSERT(modulePackageId mod /= thisPackage dflags)
			return Nothing
		where
			
	     err -> noModError dflags loc wanted_mod err
			-- Not found

    just_found location mod = do
	  	-- Adjust location to point to the hs-boot source file, 
		-- hi file, object file, when is_boot says so
	let location' | is_boot   = addBootSuffixLocn location
		      | otherwise = location
	    src_fn = expectJust "summarise2" (ml_hs_file location')

		-- Check that it exists
	  	-- It might have been deleted since the Finder last found it
	maybe_t <- modificationTimeIfExists src_fn
	case maybe_t of
	  Nothing -> noHsFileErr loc src_fn
	  Just t  -> new_summary location' mod src_fn t


    new_summary location mod src_fn src_timestamp
      = do
	-- Preprocess the source file and get its imports
	-- The dflags' contains the OPTIONS pragmas
	(dflags', hspp_fn, buf) <- preprocessFile dflags src_fn Nothing maybe_buf
        (srcimps, the_imps, L mod_loc mod_name) <- getImports dflags' buf hspp_fn

	when (mod_name /= wanted_mod) $
		throwDyn $ mkPlainErrMsg mod_loc $ 
			      text "file name does not match module name"
			      <+> quotes (ppr mod_name)

		-- Find the object timestamp, and return the summary
	obj_timestamp <- getObjTimestamp location is_boot

	return (Just ( ModSummary { ms_mod       = mod, 
				    ms_hsc_src   = hsc_src,
				    ms_location  = location,
				    ms_hspp_file = hspp_fn,
                                    ms_hspp_opts = dflags',
				    ms_hspp_buf  = Just buf,
				    ms_srcimps   = srcimps,
				    ms_imps      = the_imps,
				    ms_hs_date   = src_timestamp,
				    ms_obj_date  = obj_timestamp }))


getObjTimestamp location is_boot
  = if is_boot then return Nothing
	       else modificationTimeIfExists (ml_obj_file location)


preprocessFile :: DynFlags -> FilePath -> Maybe Phase -> Maybe (StringBuffer,ClockTime)
  -> IO (DynFlags, FilePath, StringBuffer)
preprocessFile dflags src_fn mb_phase Nothing
  = do
	(dflags', hspp_fn) <- preprocess dflags (src_fn, mb_phase)
	buf <- hGetStringBuffer hspp_fn
	return (dflags', hspp_fn, buf)

preprocessFile dflags src_fn mb_phase (Just (buf, time))
  = do
	-- case we bypass the preprocessing stage?
	let 
	    local_opts = getOptions buf src_fn
	--
	(dflags', errs) <- parseDynamicFlags dflags (map unLoc local_opts)

	let
	    needs_preprocessing
		| Just (Unlit _) <- mb_phase    = True
	        | Nothing <- mb_phase, Unlit _ <- startPhase src_fn  = True
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

noModError :: DynFlags -> SrcSpan -> ModuleName -> FindResult -> IO ab
-- ToDo: we don't have a proper line number for this error
noModError dflags loc wanted_mod err
  = throwDyn $ mkPlainErrMsg loc $ cannotFindModule dflags wanted_mod err
				
noHsFileErr loc path
  = throwDyn $ mkPlainErrMsg loc $ text "Can't find" <+> text path
 
packageModErr mod
  = throwDyn $ mkPlainErrMsg noSrcSpan $
	text "module" <+> quotes (ppr mod) <+> text "is a package module"

multiRootsErr :: [ModSummary] -> IO ()
multiRootsErr summs@(summ1:_)
  = throwDyn $ mkPlainErrMsg noSrcSpan $
	text "module" <+> quotes (ppr mod) <+> 
	text "is defined in multiple files:" <+>
	sep (map text files)
  where
    mod = ms_mod summ1
    files = map (expectJust "checkDup" . ml_hs_file . ms_location) summs

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
workingDirectoryChanged s = withSession s $ flushFinderCaches

-- -----------------------------------------------------------------------------
-- inspecting the session

-- | Get the module dependency graph.
getModuleGraph :: Session -> IO ModuleGraph -- ToDo: DiGraph ModSummary
getModuleGraph s = withSession s (return . hsc_mod_graph)

isLoaded :: Session -> ModuleName -> IO Bool
isLoaded s m = withSession s $ \hsc_env ->
  return $! isJust (lookupUFM (hsc_HPT hsc_env) m)

getBindings :: Session -> IO [TyThing]
getBindings s = withSession s (return . nameEnvElts . ic_type_env . hsc_IC)

getPrintUnqual :: Session -> IO PrintUnqualified
getPrintUnqual s = withSession s (return . icPrintUnqual . hsc_IC)

-- | Container for information about a 'Module'.
data ModuleInfo = ModuleInfo {
	minf_type_env  :: TypeEnv,
	minf_exports   :: NameSet, -- ToDo, [AvailInfo] like ModDetails?
	minf_rdr_env   :: Maybe GlobalRdrEnv,	-- Nothing for a compiled/package mod
	minf_instances :: [Instance]
	-- ToDo: this should really contain the ModIface too
  }
	-- We don't want HomeModInfo here, because a ModuleInfo applies
	-- to package modules too.

-- | Request information about a loaded 'Module'
getModuleInfo :: Session -> Module -> IO (Maybe ModuleInfo)
getModuleInfo s mdl = withSession s $ \hsc_env -> do
  let mg = hsc_mod_graph hsc_env
  if mdl `elem` map ms_mod mg
	then getHomeModuleInfo hsc_env (moduleName mdl)
	else do
  {- if isHomeModule (hsc_dflags hsc_env) mdl
	then return Nothing
	else -} getPackageModuleInfo hsc_env mdl
   -- getPackageModuleInfo will attempt to find the interface, so
   -- we don't want to call it for a home module, just in case there
   -- was a problem loading the module and the interface doesn't
   -- exist... hence the isHomeModule test here.  (ToDo: reinstate)

getPackageModuleInfo :: HscEnv -> Module -> IO (Maybe ModuleInfo)
getPackageModuleInfo hsc_env mdl = do
#ifdef GHCI
  (_msgs, mb_avails) <- getModuleExports hsc_env mdl
  case mb_avails of
    Nothing -> return Nothing
    Just avails -> do
	eps <- readIORef (hsc_EPS hsc_env)
	let 
            names  = availsToNameSet avails
	    pte    = eps_PTE eps
	    tys    = [ ty | name <- concatMap availNames avails,
			    Just ty <- [lookupTypeEnv pte name] ]
	--
	return (Just (ModuleInfo {
			minf_type_env  = mkTypeEnv tys,
			minf_exports   = names,
			minf_rdr_env   = Just $! nameSetToGlobalRdrEnv names (moduleName mdl),
			minf_instances = error "getModuleInfo: instances for package module unimplemented"
		}))
#else
  -- bogusly different for non-GHCI (ToDo)
  return Nothing
#endif

getHomeModuleInfo hsc_env mdl = 
  case lookupUFM (hsc_HPT hsc_env) mdl of
    Nothing  -> return Nothing
    Just hmi -> do
      let details = hm_details hmi
      return (Just (ModuleInfo {
			minf_type_env  = md_types details,
			minf_exports   = availsToNameSet (md_exports details),
			minf_rdr_env   = mi_globals $! hm_iface hmi,
			minf_instances = md_insts details
			}))

-- | The list of top-level entities defined in a module
modInfoTyThings :: ModuleInfo -> [TyThing]
modInfoTyThings minf = typeEnvElts (minf_type_env minf)

modInfoTopLevelScope :: ModuleInfo -> Maybe [Name]
modInfoTopLevelScope minf
  = fmap (map gre_name . globalRdrEnvElts) (minf_rdr_env minf)

modInfoExports :: ModuleInfo -> [Name]
modInfoExports minf = nameSetToList $! minf_exports minf

-- | Returns the instances defined by the specified module.
-- Warning: currently unimplemented for package modules.
modInfoInstances :: ModuleInfo -> [Instance]
modInfoInstances = minf_instances

modInfoIsExportedName :: ModuleInfo -> Name -> Bool
modInfoIsExportedName minf name = elemNameSet name (minf_exports minf)

modInfoPrintUnqualified :: ModuleInfo -> Maybe PrintUnqualified
modInfoPrintUnqualified minf = fmap mkPrintUnqualified (minf_rdr_env minf)

modInfoLookupName :: Session -> ModuleInfo -> Name -> IO (Maybe TyThing)
modInfoLookupName s minf name = withSession s $ \hsc_env -> do
   case lookupTypeEnv (minf_type_env minf) name of
     Just tyThing -> return (Just tyThing)
     Nothing      -> do
       eps <- readIORef (hsc_EPS hsc_env)
       return $! lookupType (hsc_dflags hsc_env) 
			    (hsc_HPT hsc_env) (eps_PTE eps) name

isDictonaryId :: Id -> Bool
isDictonaryId id
  = case tcSplitSigmaTy (idType id) of { (tvs, theta, tau) -> isDictTy tau }

-- | Looks up a global name: that is, any top-level name in any
-- visible module.  Unlike 'lookupName', lookupGlobalName does not use
-- the interactive context, and therefore does not require a preceding
-- 'setContext'.
lookupGlobalName :: Session -> Name -> IO (Maybe TyThing)
lookupGlobalName s name = withSession s $ \hsc_env -> do
   eps <- readIORef (hsc_EPS hsc_env)
   return $! lookupType (hsc_dflags hsc_env) 
			(hsc_HPT hsc_env) (eps_PTE eps) name

-- -----------------------------------------------------------------------------
-- Misc exported utils

dataConType :: DataCon -> Type
dataConType dc = idType (dataConWrapId dc)

-- | print a 'NamedThing', adding parentheses if the name is an operator.
pprParenSymName :: NamedThing a => a -> SDoc
pprParenSymName a = parenSymOcc (getOccName a) (ppr (getName a))

-- ----------------------------------------------------------------------------

#if 0

-- ToDo:
--   - Data and Typeable instances for HsSyn.

-- ToDo: check for small transformations that happen to the syntax in
-- the typechecker (eg. -e ==> negate e, perhaps for fromIntegral)

-- ToDo: maybe use TH syntax instead of IfaceSyn?  There's already a way
-- to get from TyCons, Ids etc. to TH syntax (reify).

-- :browse will use either lm_toplev or inspect lm_interface, depending
-- on whether the module is interpreted or not.

-- This is for reconstructing refactored source code
-- Calls the lexer repeatedly.
-- ToDo: add comment tokens to token stream
getTokenStream :: Session -> Module -> IO [Located Token]
#endif

-- -----------------------------------------------------------------------------
-- Interactive evaluation

-- | Takes a 'ModuleName' and possibly a 'PackageId', and consults the
-- filesystem and package database to find the corresponding 'Module', 
-- using the algorithm that is used for an @import@ declaration.
findModule :: Session -> ModuleName -> Maybe PackageId -> IO Module
findModule s mod_name maybe_pkg = withSession s $ \hsc_env ->
  findModule' hsc_env mod_name maybe_pkg

findModule' hsc_env mod_name maybe_pkg =
  let
        dflags = hsc_dflags hsc_env
        hpt    = hsc_HPT hsc_env
        this_pkg = thisPackage dflags
  in
  case lookupUFM hpt mod_name of
    Just mod_info -> return (mi_module (hm_iface mod_info))
    _not_a_home_module -> do
	  res <- findImportedModule hsc_env mod_name Nothing
	  case res of
	    Found _ m | modulePackageId m /= this_pkg -> return m
		      | otherwise -> throwDyn (CmdLineError (showSDoc $
					text "module" <+> pprModule m <+>
					text "is not loaded"))
	    err -> let msg = cannotFindModule dflags mod_name err in
		   throwDyn (CmdLineError (showSDoc msg))

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
setContext (Session ref) toplev_mods export_mods = do 
  hsc_env <- readIORef ref
  let old_ic  = hsc_IC     hsc_env
      hpt     = hsc_HPT    hsc_env
  --
  export_env  <- mkExportEnv hsc_env export_mods
  toplev_envs <- mapM (mkTopLevEnv hpt) toplev_mods
  let all_env = foldr plusGlobalRdrEnv export_env toplev_envs
  writeIORef ref hsc_env{ hsc_IC = old_ic { ic_toplev_scope = toplev_mods,
					    ic_exports      = export_mods,
					    ic_rn_gbl_env   = all_env }}


-- Make a GlobalRdrEnv based on the exports of the modules only.
mkExportEnv :: HscEnv -> [Module] -> IO GlobalRdrEnv
mkExportEnv hsc_env mods = do
  stuff <- mapM (getModuleExports hsc_env) mods
  let 
	(_msgs, mb_name_sets) = unzip stuff
	gres = [ nameSetToGlobalRdrEnv (availsToNameSet avails) (moduleName mod)
  	       | (Just avails, mod) <- zip mb_name_sets mods ]
  --
  return $! foldr plusGlobalRdrEnv emptyGlobalRdrEnv gres

nameSetToGlobalRdrEnv :: NameSet -> ModuleName -> GlobalRdrEnv
nameSetToGlobalRdrEnv names mod =
  mkGlobalRdrEnv [ GRE  { gre_name = name, gre_prov = vanillaProv mod }
		 | name <- nameSetToList names ]

vanillaProv :: ModuleName -> Provenance
-- We're building a GlobalRdrEnv as if the user imported
-- all the specified modules into the global interactive module
vanillaProv mod_name = Imported [ImpSpec { is_decl = decl, is_item = ImpAll}]
  where
    decl = ImpDeclSpec { is_mod = mod_name, is_as = mod_name, 
			 is_qual = False, 
			 is_dloc = srcLocSpan interactiveSrcLoc }

mkTopLevEnv :: HomePackageTable -> Module -> IO GlobalRdrEnv
mkTopLevEnv hpt modl
  = case lookupUFM hpt (moduleName modl) of
      Nothing -> throwDyn (ProgramError ("mkTopLevEnv: not a home module " ++ 
                                                showSDoc (ppr modl)))
      Just details ->
	 case mi_globals (hm_iface details) of
		Nothing  -> 
		   throwDyn (ProgramError ("mkTopLevEnv: not interpreted " 
						++ showSDoc (ppr modl)))
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
 if modulePackageId modl /= thisPackage (hsc_dflags h)
        then return False
        else case lookupUFM (hsc_HPT h) (moduleName modl) of
                Just details       -> return (isJust (mi_globals (hm_iface details)))
                _not_a_home_module -> return False

-- | Looks up an identifier in the current interactive context (for :info)
getInfo :: Session -> Name -> IO (Maybe (TyThing,Fixity,[Instance]))
getInfo s name = withSession s $ \hsc_env -> tcRnGetInfo hsc_env name

-- | Returns all names in scope in the current interactive context
getNamesInScope :: Session -> IO [Name]
getNamesInScope s = withSession s $ \hsc_env -> do
  return (map gre_name (globalRdrEnvElts (ic_rn_gbl_env (hsc_IC hsc_env))))

getRdrNamesInScope :: Session -> IO [RdrName]
getRdrNamesInScope  s = withSession s $ \hsc_env -> do
  let env = ic_rn_gbl_env (hsc_IC hsc_env)
  return (concat (map greToRdrNames (globalRdrEnvElts env)))

-- ToDo: move to RdrName
greToRdrNames :: GlobalRdrElt -> [RdrName]
greToRdrNames GRE{ gre_name = name, gre_prov = prov }
  = case prov of
     LocalDef -> [unqual]
     Imported specs -> concat (map do_spec (map is_decl specs))
  where
    occ = nameOccName name
    unqual = Unqual occ
    do_spec decl_spec
	| is_qual decl_spec = [qual]
	| otherwise         = [unqual,qual]
	where qual = Qual (is_as decl_spec) occ

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

-- | Returns the 'TyThing' for a 'Name'.  The 'Name' may refer to any
-- entity known to GHC, including 'Name's defined using 'runStmt'.
lookupName :: Session -> Name -> IO (Maybe TyThing)
lookupName s name = withSession s $ \hsc_env -> tcRnLookupName hsc_env name

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
-- Compile an expression into a dynamic

dynCompileExpr :: Session -> String -> IO (Maybe Dynamic)
dynCompileExpr ses expr = do
    (full,exports) <- getContext ses
    setContext ses full $
        (mkModule
            (stringToPackageId "base") (mkModuleName "Data.Dynamic")
        ):exports
    let stmt = "let __dynCompileExpr = Data.Dynamic.toDyn (" ++ expr ++ ")"
    res <- withSession ses (flip hscStmt stmt)
    setContext ses full exports
    case res of
        Nothing -> return Nothing
        Just (_, names, hvals) -> do
            vals <- (unsafeCoerce# hvals :: IO [Dynamic])
            case (names,vals) of
                (_:[], v:[])    -> return (Just v)
                _               -> panic "dynCompileExpr"

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

-- When running a computation, we redirect ^C exceptions to the running
-- thread.  ToDo: we might want a way to continue even if the target
-- thread doesn't die when it receives the exception... "this thread
-- is not responding".
sandboxIO :: IO a -> IO (Either Exception a)
sandboxIO thing = do
  m <- newEmptyMVar
  ts <- takeMVar interruptTargetThread
  child <- forkIO (do res <- Exception.try thing; putMVar m res)
  putMVar interruptTargetThread (child:ts)
  takeMVar m `finally` modifyMVar_ interruptTargetThread (return.tail)

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

-----------------------------------------------------------------------------
-- show a module and it's source/object filenames

showModule :: Session -> ModSummary -> IO String
showModule s mod_summary = withSession s $ \hsc_env -> do
  case lookupUFM (hsc_HPT hsc_env) (ms_mod_name mod_summary) of
	Nothing	      -> panic "missing linkable"
	Just mod_info -> return (showModMsg (hscTarget (hsc_dflags hsc_env)) (not obj_linkable) mod_summary)
		      where
			 obj_linkable = isObjectLinkable (expectJust "showModule" (hm_linkable mod_info))

obtainTerm :: Session -> Bool -> Id -> IO (Maybe Term)
obtainTerm sess force id = withSession sess $ \hsc_env -> 
              getHValue (varName id) >>= traverse (cvObtainTerm hsc_env force Nothing)

#endif /* GHCI */
