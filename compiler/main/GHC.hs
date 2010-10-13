-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005
--
-- The GHC API
--
-- -----------------------------------------------------------------------------

module GHC (
	-- * Initialisation
	defaultErrorHandler,
	defaultCleanupHandler,

        -- * GHC Monad
        Ghc, GhcT, GhcMonad(..),
        runGhc, runGhcT, initGhcMonad,
        gcatch, gbracket, gfinally,
        clearWarnings, getWarnings, hasWarnings,
        printExceptionAndWarnings, printWarnings,
        handleSourceError, defaultCallbacks, GhcApiCallbacks(..),
        needsTemplateHaskell,

	-- * Flags and settings
	DynFlags(..), DynFlag(..), Severity(..), HscTarget(..), dopt,
        GhcMode(..), GhcLink(..), defaultObjectTarget,
	parseDynamicFlags,
	getSessionDynFlags,
	setSessionDynFlags,
	parseStaticFlags,

	-- * Targets
	Target(..), TargetId(..), Phase,
	setTargets,
	getTargets,
	addTarget,
	removeTarget,
	guessTarget,
	
	-- * Loading\/compiling the program
	depanal,
	load, loadWithLogger, LoadHowMuch(..),
	SuccessFlag(..), succeeded, failed,
        defaultWarnErrLogger, WarnErrLogger,
	workingDirectoryChanged,
        parseModule, typecheckModule, desugarModule, loadModule,
        ParsedModule(..), TypecheckedModule(..), DesugaredModule(..),
	TypecheckedSource, ParsedSource, RenamedSource,   -- ditto
        TypecheckedMod, ParsedMod,
        moduleInfo, renamedSource, typecheckedSource,
        parsedSource, coreModule,
        compileToCoreModule, compileToCoreSimplified,
        compileCoreToObj,
        getModSummary,

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
        modInfoExports,
	modInfoInstances,
	modInfoIsExportedName,
	modInfoLookupName,
	lookupGlobalName,
	findGlobalAnns,
        mkPrintUnqualifiedForModule,

        -- * Querying the environment
        packageDbModules,

	-- * Printing
	PrintUnqualified, alwaysQualify,

	-- * Interactive evaluation
	getBindings, getPrintUnqual,
        findModule,
        lookupModule,
#ifdef GHCI
	setContext, getContext,	
	getNamesInScope,
	getRdrNamesInScope,
        getGRE,
	moduleIsInterpreted,
	getInfo,
	exprType,
	typeKind,
	parseName,
	RunResult(..),  
	runStmt, parseImportDecl, SingleStep(..),
        resume,
        Resume(resumeStmt, resumeThreadId, resumeBreakInfo, resumeSpan,
               resumeHistory, resumeHistoryIx),
        History(historyBreakInfo, historyEnclosingDecl), 
        GHC.getHistorySpan, getHistoryModule,
        getResumeContext,
        abandon, abandonAll,
        InteractiveEval.back,
        InteractiveEval.forward,
	showModule,
        isModuleInterpreted,
	InteractiveEval.compileExpr, HValue, dynCompileExpr,
        GHC.obtainTermFromId, GHC.obtainTermFromVal, reconstructType,
        modInfoModBreaks,
        ModBreaks(..), BreakIndex,
        BreakInfo(breakInfo_number, breakInfo_module),
        BreakArray, setBreakOn, setBreakOff, getBreak,
#endif
        lookupName,

	-- * Abstract syntax elements

        -- ** Packages
        PackageId,

	-- ** Modules
	Module, mkModule, pprModule, moduleName, modulePackageId,
        ModuleName, mkModuleName, moduleNameString,

	-- ** Names
	Name, 
	isExternalName, nameModule, pprParenSymName, nameSrcSpan,
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
	isFamilyTyCon,
	synTyConDefn, synTyConType, synTyConResKind,

	-- ** Type variables
	TyVar,
	alphaTyVars,

	-- ** Data constructors
	DataCon,
	dataConSig, dataConType, dataConTyCon, dataConFieldLabels,
	dataConIsInfix, isVanillaDataCon, dataConUserType,
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
	Type, splitForAllTys, funResultTy, 
	pprParendType, pprTypeApp, 
	Kind,
	PredType,
	ThetaType, pprForAll, pprThetaArrow,

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
        mkSrcLoc, isGoodSrcLoc, noSrcLoc,
	srcLocFile, srcLocLine, srcLocCol,
        SrcSpan,
        mkSrcSpan, srcLocSpan, isGoodSrcSpan, noSrcSpan,
        srcSpanStart, srcSpanEnd,
	srcSpanFile, 
        srcSpanStartLine, srcSpanEndLine, 
        srcSpanStartCol, srcSpanEndCol,

        -- ** Located
	Located(..),

	-- *** Constructing Located
	noLoc, mkGeneralLocated,

	-- *** Deconstructing Located
	getLoc, unLoc,

	-- *** Combining and comparing Located values
	eqLocated, cmpLocated, combineLocs, addCLoc,
        leftmost_smallest, leftmost_largest, rightmost,
        spans, isSubspanOf,

	-- * Exceptions
	GhcException(..), showGhcException,

        -- * Token stream manipulations
        Token,
        getTokenStream, getRichTokenStream,
        showRichTokenStream, addSourceToTokens,

	-- * Miscellaneous
	--sessionHscEnv,
	cyclicModuleErr,
  ) where

{-
 ToDo:

  * inline bits of HscMain here to simplify layering: hscTcExpr, hscStmt.
  * what StaticFlags should we expose, if any?
-}

#include "HsVersions.h"

#ifdef GHCI
import qualified Linker
import Linker           ( HValue )
import ByteCodeInstr
import BreakArray
import InteractiveEval
#endif

import TcRnDriver
import TcIface
import TcRnTypes
import TcRnMonad        ( initIfaceCheck )
import Packages
import NameSet
import RdrName
import qualified HsSyn -- hack as we want to reexport the whole module
import HsSyn hiding ((<.>))
import Type
import Coercion		( synTyConResKind )
import TcType		hiding( typeKind )
import Id
import Var
import TysPrim		( alphaTyVars )
import TyCon
import Class
-- import FunDeps
import DataCon
import Name             hiding ( varName )
-- import OccName		( parenSymOcc )
import InstEnv		( Instance, instanceDFunId, pprInstance, pprInstanceHdr,
                          emptyInstEnv )
import FamInstEnv       ( emptyFamInstEnv )
import SrcLoc
--import CoreSyn
import TidyPgm
import DriverPipeline
import DriverPhases	( Phase(..), isHaskellSrcFilename, startPhase )
import HeaderInfo
import Finder
import HscMain
import HscTypes
import DynFlags
import StaticFlagParser
import qualified StaticFlags
import SysTools     ( initSysTools, cleanTempFiles, cleanTempFilesExcept,
                      cleanTempDirs )
import Annotations
import Module
import UniqFM
import Panic
import Digraph
import Bag		( unitBag, listToBag, emptyBag, isEmptyBag )
import ErrUtils
import MonadUtils
import Util
import StringBuffer	( StringBuffer, hGetStringBuffer, nextChar )
import Outputable
import BasicTypes
import Maybes		( expectJust, mapCatMaybes )
import FastString
import Lexer

import System.Directory ( getModificationTime, doesFileExist,
                          getCurrentDirectory )
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified FiniteMap as Map
import Data.List
import qualified Data.List as List
import Data.Typeable    ( Typeable )
import Data.Word        ( Word8 )
import Control.Monad
import System.Exit	( exitWith, ExitCode(..) )
import System.Time	( ClockTime, getClockTime )
import Exception
import Data.IORef
import System.FilePath
import System.IO
import System.IO.Error	( try, isDoesNotExistError )
import Prelude hiding (init)


-- -----------------------------------------------------------------------------
-- Exception handlers

-- | Install some default exception handlers and run the inner computation.
-- Unless you want to handle exceptions yourself, you should wrap this around
-- the top level of your program.  The default handlers output the error
-- message(s) to stderr and exit cleanly.
defaultErrorHandler :: (ExceptionMonad m, MonadIO m) => DynFlags -> m a -> m a
defaultErrorHandler dflags inner =
  -- top-level exception handler: any unrecognised exception is a compiler bug.
  ghandle (\exception -> liftIO $ do
           hFlush stdout
           case fromException exception of
                -- an IO exception probably isn't our fault, so don't panic
                Just (ioe :: IOException) ->
                  fatalErrorMsg dflags (text (show ioe))
                _ -> case fromException exception of
		     Just UserInterrupt -> exitWith (ExitFailure 1)
                     Just StackOverflow ->
                         fatalErrorMsg dflags (text "stack overflow: use +RTS -K<size> to increase it")
                     _ -> case fromException exception of
                          Just (ex :: ExitCode) -> throw ex
                          _ ->
                              fatalErrorMsg dflags
                                  (text (show (Panic (show exception))))
           exitWith (ExitFailure 1)
         ) $

  -- error messages propagated as exceptions
  handleGhcException
            (\ge -> liftIO $ do
  		hFlush stdout
  		case ge of
		     PhaseFailed _ code -> exitWith code
		     Signal _ -> exitWith (ExitFailure 1)
		     _ -> do fatalErrorMsg dflags (text (show ge))
			     exitWith (ExitFailure 1)
	    ) $
  inner

-- | Install a default cleanup handler to remove temporary files deposited by
-- a GHC run.  This is seperate from 'defaultErrorHandler', because you might
-- want to override the error handling, but still get the ordinary cleanup
-- behaviour.
defaultCleanupHandler :: (ExceptionMonad m, MonadIO m) =>
                         DynFlags -> m a -> m a
defaultCleanupHandler dflags inner =
    -- make sure we clean up after ourselves
    inner `gfinally`
          (liftIO $ do
              cleanTempFiles dflags
              cleanTempDirs dflags
          )
          --  exceptions will be blocked while we clean the temporary files,
          -- so there shouldn't be any difficulty if we receive further
          -- signals.

-- | Print the error message and all warnings.  Useful inside exception
--   handlers.  Clears warnings after printing.
printExceptionAndWarnings :: GhcMonad m => SourceError -> m ()
printExceptionAndWarnings err = do
    let errs = srcErrorMessages err
    warns <- getWarnings
    dflags <- getSessionDynFlags
    if isEmptyBag errs
       -- Empty errors means we failed due to -Werror.  (Since this function
       -- takes a source error as argument, we know for sure _some_ error
       -- did indeed happen.)
       then liftIO $ do
              printBagOfWarnings dflags warns
              printBagOfErrors dflags (unitBag warnIsErrorMsg)
       else liftIO $ printBagOfErrors dflags errs
    clearWarnings

-- | Print all accumulated warnings using 'log_action'.
printWarnings :: GhcMonad m => m ()
printWarnings = do
    dflags <- getSessionDynFlags
    warns <- getWarnings
    liftIO $ printBagOfWarnings dflags warns
    clearWarnings

-- | Run function for the 'Ghc' monad.
--
-- It initialises the GHC session and warnings via 'initGhcMonad'.  Each call
-- to this function will create a new session which should not be shared among
-- several threads.
--
-- Any errors not handled inside the 'Ghc' action are propagated as IO
-- exceptions.

runGhc :: Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
       -> Ghc a           -- ^ The action to perform.
       -> IO a
runGhc mb_top_dir ghc = do
  wref <- newIORef emptyBag
  ref <- newIORef undefined
  let session = Session ref wref
  flip unGhc session $ do
    initGhcMonad mb_top_dir
    ghc
  -- XXX: unregister interrupt handlers here?

-- | Run function for 'GhcT' monad transformer.
--
-- It initialises the GHC session and warnings via 'initGhcMonad'.  Each call
-- to this function will create a new session which should not be shared among
-- several threads.

runGhcT :: (ExceptionMonad m, Functor m, MonadIO m) =>
           Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
        -> GhcT m a        -- ^ The action to perform.
        -> m a
runGhcT mb_top_dir ghct = do
  wref <- liftIO $ newIORef emptyBag
  ref <- liftIO $ newIORef undefined
  let session = Session ref wref
  flip unGhcT session $ do
    initGhcMonad mb_top_dir
    ghct

-- | Initialise a GHC session.
--
-- If you implement a custom 'GhcMonad' you must call this function in the
-- monad run function.  It will initialise the session variable and clear all
-- warnings.
--
-- The first argument should point to the directory where GHC's library files
-- reside.  More precisely, this should be the output of @ghc --print-libdir@
-- of the version of GHC the module using this API is compiled with.  For
-- portability, you should use the @ghc-paths@ package, available at
-- <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/ghc-paths>.

initGhcMonad :: GhcMonad m => Maybe FilePath -> m ()
initGhcMonad mb_top_dir = do
  -- catch ^C
  liftIO $ installSignalHandlers

  liftIO $ StaticFlags.initStaticOpts

  dflags0 <- liftIO $ initDynFlags defaultDynFlags
  dflags <- liftIO $ initSysTools mb_top_dir dflags0
  env <- liftIO $ newHscEnv defaultCallbacks dflags
  setSession env
  clearWarnings

defaultCallbacks :: GhcApiCallbacks
defaultCallbacks =
  GhcApiCallbacks {
    reportModuleCompilationResult =
        \_ mb_err -> defaultWarnErrLogger mb_err
  }

-- -----------------------------------------------------------------------------
-- Flags & settings

-- | Grabs the DynFlags from the Session
getSessionDynFlags :: GhcMonad m => m DynFlags
getSessionDynFlags = withSession (return . hsc_dflags)

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
setSessionDynFlags :: GhcMonad m => DynFlags -> m [PackageId]
setSessionDynFlags dflags = do
  (dflags', preload) <- liftIO $ initPackages dflags
  modifySession (\h -> h{ hsc_dflags = dflags' })
  return preload

-- | If there is no -o option, guess the name of target executable
-- by using top-level source file name as a base.
guessOutputFile :: GhcMonad m => m ()
guessOutputFile = modifySession $ \env ->
    let dflags = hsc_dflags env
        mod_graph = hsc_mod_graph env
        mainModuleSrcPath :: Maybe String
        mainModuleSrcPath = do
            let isMain = (== mainModIs dflags) . ms_mod
            [ms] <- return (filter isMain mod_graph)
            ml_hs_file (ms_location ms)
        name = fmap dropExtension mainModuleSrcPath

#if defined(mingw32_HOST_OS)
        -- we must add the .exe extention unconditionally here, otherwise
        -- when name has an extension of its own, the .exe extension will
        -- not be added by DriverPipeline.exeFileName.  See #2248
        name_exe = fmap (<.> "exe") name
#else
        name_exe = name
#endif
    in
    case outputFile dflags of
        Just _ -> env
        Nothing -> env { hsc_dflags = dflags { outputFile = name_exe } }

-- -----------------------------------------------------------------------------
-- Targets

-- ToDo: think about relative vs. absolute file paths. And what
-- happens when the current directory changes.

-- | Sets the targets for this session.  Each target may be a module name
-- or a filename.  The targets correspond to the set of root modules for
-- the program\/library.  Unloading the current program is achieved by
-- setting the current set of targets to be empty, followed by 'load'.
setTargets :: GhcMonad m => [Target] -> m ()
setTargets targets = modifySession (\h -> h{ hsc_targets = targets })

-- | Returns the current set of targets
getTargets :: GhcMonad m => m [Target]
getTargets = withSession (return . hsc_targets)

-- | Add another target.
addTarget :: GhcMonad m => Target -> m ()
addTarget target
  = modifySession (\h -> h{ hsc_targets = target : hsc_targets h })

-- | Remove a target
removeTarget :: GhcMonad m => TargetId -> m ()
removeTarget target_id
  = modifySession (\h -> h{ hsc_targets = filter (hsc_targets h) })
  where
   filter targets = [ t | t@(Target id _ _) <- targets, id /= target_id ]

-- | Attempts to guess what Target a string refers to.  This function
-- implements the @--make@/GHCi command-line syntax for filenames:
--
--   - if the string looks like a Haskell source filename, then interpret it
--     as such
--
--   - if adding a .hs or .lhs suffix yields the name of an existing file,
--     then use that
--
--   - otherwise interpret the string as a module name
--
guessTarget :: GhcMonad m => String -> Maybe Phase -> m Target
guessTarget str (Just phase)
   = return (Target (TargetFile str (Just phase)) True Nothing)
guessTarget str Nothing
   | isHaskellSrcFilename file
   = return (target (TargetFile file Nothing))
   | otherwise
   = do exists <- liftIO $ doesFileExist hs_file
	if exists
	   then return (target (TargetFile hs_file Nothing))
	   else do
	exists <- liftIO $ doesFileExist lhs_file
	if exists
	   then return (target (TargetFile lhs_file Nothing))
	   else do
        if looksLikeModuleName file
           then return (target (TargetModule (mkModuleName file)))
           else do
        throwGhcException
                 (ProgramError (showSDoc $
                 text "target" <+> quotes (text file) <+> 
                 text "is not a module name or a source file"))
     where 
         (file,obj_allowed)
                | '*':rest <- str = (rest, False)
                | otherwise       = (str,  True)

	 hs_file  = file <.> "hs"
	 lhs_file = file <.> "lhs"

         target tid = Target tid obj_allowed Nothing

-- -----------------------------------------------------------------------------
-- Loading the program

-- | Perform a dependency analysis starting from the current targets
-- and update the session with the new module graph.
--
-- Dependency analysis entails parsing the @import@ directives and may
-- therefore require running certain preprocessors.
--
-- Note that each 'ModSummary' in the module graph caches its 'DynFlags'.
-- These 'DynFlags' are determined by the /current/ session 'DynFlags' and the
-- @OPTIONS@ and @LANGUAGE@ pragmas of the parsed module.  Thus if you want to
-- changes to the 'DynFlags' to take effect you need to call this function
-- again.
--
depanal :: GhcMonad m =>
           [ModuleName]  -- ^ excluded modules
        -> Bool          -- ^ allow duplicate roots
        -> m ModuleGraph
depanal excluded_mods allow_dup_roots = do
  hsc_env <- getSession
  let
	 dflags  = hsc_dflags hsc_env
	 targets = hsc_targets hsc_env
	 old_graph = hsc_mod_graph hsc_env
	
  liftIO $ showPass dflags "Chasing dependencies"
  liftIO $ debugTraceMsg dflags 2 (hcat [
	     text "Chasing modules from: ",
	     hcat (punctuate comma (map pprTarget targets))])

  mod_graph <- downsweep hsc_env old_graph excluded_mods allow_dup_roots
  modifySession $ \_ -> hsc_env { hsc_mod_graph = mod_graph }
  return mod_graph

-- | Describes which modules of the module graph need to be loaded.
data LoadHowMuch
   = LoadAllTargets
     -- ^ Load all targets and its dependencies.
   | LoadUpTo ModuleName
     -- ^ Load only the given module and its dependencies.
   | LoadDependenciesOf ModuleName
     -- ^ Load only the dependencies of the given module, but not the module
     -- itself.

-- | Try to load the program.  See 'LoadHowMuch' for the different modes.
--
-- This function implements the core of GHC's @--make@ mode.  It preprocesses,
-- compiles and loads the specified modules, avoiding re-compilation wherever
-- possible.  Depending on the target (see 'DynFlags.hscTarget') compilating
-- and loading may result in files being created on disk.
--
-- Calls the 'reportModuleCompilationResult' callback after each compiling
-- each module, whether successful or not.
--
-- Throw a 'SourceError' if errors are encountered before the actual
-- compilation starts (e.g., during dependency analysis).  All other errors
-- are reported using the callback.
--
load :: GhcMonad m => LoadHowMuch -> m SuccessFlag
load how_much = do
   mod_graph <- depanal [] False
   load2 how_much mod_graph

-- | A function called to log warnings and errors.
type WarnErrLogger = GhcMonad m => Maybe SourceError -> m ()

defaultWarnErrLogger :: WarnErrLogger
defaultWarnErrLogger Nothing = printWarnings
defaultWarnErrLogger (Just e) = printExceptionAndWarnings e

-- | Try to load the program.  If a Module is supplied, then just
-- attempt to load up to this target.  If no Module is supplied,
-- then try to load all targets.
--
-- The first argument is a function that is called after compiling each
-- module to print wanrings and errors.
--
-- While compiling a module, all 'SourceError's are caught and passed to the
-- logger, however, this function may still throw a 'SourceError' if
-- dependency analysis failed (e.g., due to a parse error).
--
loadWithLogger :: GhcMonad m => WarnErrLogger -> LoadHowMuch -> m SuccessFlag
loadWithLogger logger how_much = do
    -- Dependency analysis first.  Note that this fixes the module graph:
    -- even if we don't get a fully successful upsweep, the full module
    -- graph is still retained in the Session.  We can tell which modules
    -- were successfully loaded by inspecting the Session's HPT.
    withLocalCallbacks (\cbs -> cbs { reportModuleCompilationResult =
                                          \_ -> logger }) $
      load how_much

load2 :: GhcMonad m => LoadHowMuch -> [ModSummary]
      -> m SuccessFlag
load2 how_much mod_graph = do
        guessOutputFile
	hsc_env <- getSession

        let hpt1      = hsc_HPT hsc_env
        let dflags    = hsc_dflags hsc_env

	-- The "bad" boot modules are the ones for which we have
	-- B.hs-boot in the module graph, but no B.hs
	-- The downsweep should have ensured this does not happen
	-- (see msDeps)
        let all_home_mods = [ms_mod_name s 
			    | s <- mod_graph, not (isBootSummary s)]
	    bad_boot_mods = [s 	      | s <- mod_graph, isBootSummary s,
					not (ms_mod_name s `elem` all_home_mods)]
	ASSERT( null bad_boot_mods ) return ()

        -- check that the module given in HowMuch actually exists, otherwise
        -- topSortModuleGraph will bomb later.
        let checkHowMuch (LoadUpTo m)           = checkMod m
            checkHowMuch (LoadDependenciesOf m) = checkMod m
            checkHowMuch _ = id

            checkMod m and_then
                | m `elem` all_home_mods = and_then
                | otherwise = do 
                        liftIO $ errorMsg dflags (text "no such module:" <+>
                                         quotes (ppr m))
                        return Failed

        checkHowMuch how_much $ do

        -- mg2_with_srcimps drops the hi-boot nodes, returning a 
	-- graph with cycles.  Among other things, it is used for
        -- backing out partially complete cycles following a failed
        -- upsweep, and for removing from hpt all the modules
        -- not in strict downwards closure, during calls to compile.
        let mg2_with_srcimps :: [SCC ModSummary]
	    mg2_with_srcimps = topSortModuleGraph True mod_graph Nothing

	-- If we can determine that any of the {-# SOURCE #-} imports
	-- are definitely unnecessary, then emit a warning.
	warnUnnecessarySourceImports mg2_with_srcimps

 	let
	    -- check the stability property for each module.
	    stable_mods@(stable_obj,stable_bco)
	        = checkStability hpt1 mg2_with_srcimps all_home_mods

	    -- prune bits of the HPT which are definitely redundant now,
	    -- to save space.
	    pruned_hpt = pruneHomePackageTable hpt1 
				(flattenSCCs mg2_with_srcimps)
				stable_mods

	_ <- liftIO $ evaluate pruned_hpt

        -- before we unload anything, make sure we don't leave an old
        -- interactive context around pointing to dead bindings.  Also,
        -- write the pruned HPT to allow the old HPT to be GC'd.
        modifySession $ \_ -> hsc_env{ hsc_IC = emptyInteractiveContext,
                                       hsc_HPT = pruned_hpt }

	liftIO $ debugTraceMsg dflags 2 (text "Stable obj:" <+> ppr stable_obj $$
				text "Stable BCO:" <+> ppr stable_bco)

	-- Unload any modules which are going to be re-linked this time around.
	let stable_linkables = [ linkable
			       | m <- stable_obj++stable_bco,
				 Just hmi <- [lookupUFM pruned_hpt m],
				 Just linkable <- [hm_linkable hmi] ]
	liftIO $ unload hsc_env stable_linkables

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
		| LoadDependenciesOf _mod <- how_much
		= ASSERT( case last partial_mg0 of 
			    AcyclicSCC ms -> ms_mod_name ms == _mod; _ -> False )
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

	liftIO $ debugTraceMsg dflags 2 (hang (text "Ready for upsweep")
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
           do liftIO $ debugTraceMsg dflags 2 (text "Upsweep completely successful.")

	      -- Clean up after ourselves
	      liftIO $ cleanTempFilesExcept dflags (ppFilesFromSummaries modsDone)

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
		do_linking = a_root_is_Main || no_hs_main || ghcLink dflags == LinkDynLib

	      when (ghcLink dflags == LinkBinary 
                    && isJust ofile && not do_linking) $
	        liftIO $ debugTraceMsg dflags 1 $
                    text ("Warning: output was redirected with -o, " ++
                          "but no output will be generated\n" ++
			  "because there is no " ++ 
                          moduleNameString (moduleName main_mod) ++ " module.")

	      -- link everything together
              linkresult <- liftIO $ link (ghcLink dflags) dflags do_linking (hsc_HPT hsc_env1)

	      loadFinish Succeeded linkresult hsc_env1

         else 
           -- Tricky.  We need to back out the effects of compiling any
           -- half-done cycles, both so as to clean up the top level envs
           -- and to avoid telling the interactive linker to link them.
           do liftIO $ debugTraceMsg dflags 2 (text "Upsweep partially successful.")

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
	      liftIO $ cleanTempFilesExcept dflags (ppFilesFromSummaries mods_to_keep)

	      -- there should be no Nothings where linkables should be, now
	      ASSERT(all (isJust.hm_linkable) 
			(eltsUFM (hsc_HPT hsc_env))) do
	
	      -- Link everything together
              linkresult <- liftIO $ link (ghcLink dflags) dflags False hpt4

	      let hsc_env4 = hsc_env1{ hsc_HPT = hpt4 }
	      loadFinish Failed linkresult hsc_env4

-- Finish up after a load.

-- If the link failed, unload everything and return.
loadFinish :: GhcMonad m =>
              SuccessFlag -> SuccessFlag -> HscEnv
           -> m SuccessFlag
loadFinish _all_ok Failed hsc_env
  = do liftIO $ unload hsc_env []
       modifySession $ \_ -> discardProg hsc_env
       return Failed

-- Empty the interactive context and set the module context to the topmost
-- newly loaded module, or the Prelude if none were loaded.
loadFinish all_ok Succeeded hsc_env
  = do modifySession $ \_ -> hsc_env{ hsc_IC = emptyInteractiveContext }
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
ppFilesFromSummaries :: [ModSummary] -> [FilePath]
ppFilesFromSummaries summaries = map ms_hspp_file summaries

-- -----------------------------------------------------------------------------

class ParsedMod m where
  modSummary   :: m -> ModSummary
  parsedSource :: m -> ParsedSource

class ParsedMod m => TypecheckedMod m where
  renamedSource     :: m -> Maybe RenamedSource
  typecheckedSource :: m -> TypecheckedSource
  moduleInfo        :: m -> ModuleInfo
  tm_internals      :: m -> (TcGblEnv, ModDetails)
	-- ToDo: improvements that could be made here:
	--  if the module succeeded renaming but not typechecking,
	--  we can still get back the GlobalRdrEnv and exports, so
	--  perhaps the ModuleInfo should be split up into separate
	--  fields.

class TypecheckedMod m => DesugaredMod m where
  coreModule :: m -> ModGuts

-- | The result of successful parsing.
data ParsedModule =
  ParsedModule { pm_mod_summary   :: ModSummary
               , pm_parsed_source :: ParsedSource }

instance ParsedMod ParsedModule where
  modSummary m    = pm_mod_summary m
  parsedSource m = pm_parsed_source m

-- | The result of successful typechecking.  It also contains the parser
--   result.
data TypecheckedModule =
  TypecheckedModule { tm_parsed_module       :: ParsedModule
                    , tm_renamed_source      :: Maybe RenamedSource
                    , tm_typechecked_source  :: TypecheckedSource
                    , tm_checked_module_info :: ModuleInfo
                    , tm_internals_          :: (TcGblEnv, ModDetails)
                    }

instance ParsedMod TypecheckedModule where
  modSummary m   = modSummary (tm_parsed_module m)
  parsedSource m = parsedSource (tm_parsed_module m)

instance TypecheckedMod TypecheckedModule where
  renamedSource m     = tm_renamed_source m
  typecheckedSource m = tm_typechecked_source m
  moduleInfo m = tm_checked_module_info m
  tm_internals m      = tm_internals_ m

-- | The result of successful desugaring (i.e., translation to core).  Also
--  contains all the information of a typechecked module.
data DesugaredModule =
  DesugaredModule { dm_typechecked_module :: TypecheckedModule
                  , dm_core_module        :: ModGuts
             }

instance ParsedMod DesugaredModule where
  modSummary m   = modSummary (dm_typechecked_module m)
  parsedSource m = parsedSource (dm_typechecked_module m)

instance TypecheckedMod DesugaredModule where
  renamedSource m     = renamedSource (dm_typechecked_module m)
  typecheckedSource m = typecheckedSource (dm_typechecked_module m)
  moduleInfo m        = moduleInfo (dm_typechecked_module m)
  tm_internals m      = tm_internals_ (dm_typechecked_module m)

instance DesugaredMod DesugaredModule where
  coreModule m = dm_core_module m

type ParsedSource      = Located (HsModule RdrName)
type RenamedSource     = (HsGroup Name, [LImportDecl Name], Maybe [LIE Name],
                          Maybe LHsDocString)
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

-- | Return the 'ModSummary' of a module with the given name.
--
-- The module must be part of the module graph (see 'hsc_mod_graph' and
-- 'ModuleGraph').  If this is not the case, this function will throw a
-- 'GhcApiError'.
--
-- This function ignores boot modules and requires that there is only one
-- non-boot module with the given name.
getModSummary :: GhcMonad m => ModuleName -> m ModSummary
getModSummary mod = do
   mg <- liftM hsc_mod_graph getSession
   case [ ms | ms <- mg, ms_mod_name ms == mod, not (isBootSummary ms) ] of
     [] -> throw $ mkApiErr (text "Module not part of module graph")
     [ms] -> return ms
     multiple -> throw $ mkApiErr (text "getModSummary is ambiguous: " <+> ppr multiple)

-- | Parse a module.
--
-- Throws a 'SourceError' on parse error.
parseModule :: GhcMonad m => ModSummary -> m ParsedModule
parseModule ms = do
   rdr_module <- withTempSession
                     (\e -> e { hsc_dflags = ms_hspp_opts ms }) $
                   hscParse ms
   return (ParsedModule ms rdr_module)

-- | Typecheck and rename a parsed module.
--
-- Throws a 'SourceError' if either fails.
typecheckModule :: GhcMonad m => ParsedModule -> m TypecheckedModule
typecheckModule pmod = do
 let ms = modSummary pmod
 withTempSession (\e -> e { hsc_dflags = ms_hspp_opts ms }) $ do
   (tc_gbl_env, rn_info)
       <- hscTypecheckRename ms (parsedSource pmod)
   details <- makeSimpleDetails tc_gbl_env
   return $
     TypecheckedModule {
       tm_internals_          = (tc_gbl_env, details),
       tm_parsed_module       = pmod,
       tm_renamed_source      = rn_info,
       tm_typechecked_source  = tcg_binds tc_gbl_env,
       tm_checked_module_info =
         ModuleInfo {
           minf_type_env  = md_types details,
           minf_exports   = availsToNameSet $ md_exports details,
           minf_rdr_env   = Just (tcg_rdr_env tc_gbl_env),
           minf_instances = md_insts details
#ifdef GHCI
           ,minf_modBreaks = emptyModBreaks
#endif
         }}

-- | Desugar a typechecked module.
desugarModule :: GhcMonad m => TypecheckedModule -> m DesugaredModule
desugarModule tcm = do
 let ms = modSummary tcm
 withTempSession (\e -> e { hsc_dflags = ms_hspp_opts ms }) $ do
   let (tcg, _) = tm_internals tcm
   guts <- hscDesugar ms tcg
   return $
     DesugaredModule {
       dm_typechecked_module = tcm,
       dm_core_module        = guts
     }

-- | Load a module.  Input doesn't need to be desugared.
--
-- A module must be loaded before dependent modules can be typechecked.  This
-- always includes generating a 'ModIface' and, depending on the
-- 'DynFlags.hscTarget', may also include code generation.
--
-- This function will always cause recompilation and will always overwrite
-- previous compilation results (potentially files on disk).
--
loadModule :: (TypecheckedMod mod, GhcMonad m) => mod -> m mod
loadModule tcm = do
   let ms = modSummary tcm
   let mod = ms_mod_name ms
   let loc = ms_location ms
   let (tcg, _details) = tm_internals tcm
   hpt_new <-
       withTempSession (\e -> e { hsc_dflags = ms_hspp_opts ms }) $ do

         let compilerBackend comp env ms' _ _mb_old_iface _ =
               withTempSession (\_ -> env) $
                 hscBackend comp tcg ms' Nothing

         hsc_env <- getSession
         mod_info <- do
             mb_linkable <- 
                  case ms_obj_date ms of
                     Just t | t > ms_hs_date ms  -> do
                         l <- liftIO $ findObjectLinkable (ms_mod ms) 
                                                  (ml_obj_file loc) t
                         return (Just l)
                     _otherwise -> return Nothing
                                                
             compile' (compilerBackend hscNothingCompiler
                      ,compilerBackend hscInteractiveCompiler
                      ,hscCheckRecompBackend hscBatchCompiler tcg)
                      hsc_env ms 1 1 Nothing mb_linkable
         -- compile' shouldn't change the environment
         return $ addToUFM (hsc_HPT hsc_env) mod mod_info
   modifySession $ \e -> e{ hsc_HPT = hpt_new }
   return tcm


-- | This is the way to get access to the Core bindings corresponding
-- to a module. 'compileToCore' parses, typechecks, and
-- desugars the module, then returns the resulting Core module (consisting of
-- the module name, type declarations, and function declarations) if
-- successful.
compileToCoreModule :: GhcMonad m => FilePath -> m CoreModule
compileToCoreModule = compileCore False

-- | Like compileToCoreModule, but invokes the simplifier, so
-- as to return simplified and tidied Core.
compileToCoreSimplified :: GhcMonad m => FilePath -> m CoreModule
compileToCoreSimplified = compileCore True
{-
-- | Provided for backwards-compatibility: compileToCore returns just the Core
-- bindings, but for most purposes, you probably want to call
-- compileToCoreModule.
compileToCore :: GhcMonad m => FilePath -> m [CoreBind]
compileToCore fn = do
   mod <- compileToCoreModule session fn
   return $ cm_binds mod
-}
-- | Takes a CoreModule and compiles the bindings therein
-- to object code. The first argument is a bool flag indicating
-- whether to run the simplifier.
-- The resulting .o, .hi, and executable files, if any, are stored in the
-- current directory, and named according to the module name.
-- This has only so far been tested with a single self-contained module.
compileCoreToObj :: GhcMonad m => Bool -> CoreModule -> m ()
compileCoreToObj simplify cm@(CoreModule{ cm_module = mName }) = do
  dflags      <- getSessionDynFlags
  currentTime <- liftIO $ getClockTime
  cwd         <- liftIO $ getCurrentDirectory
  modLocation <- liftIO $ mkHiOnlyModLocation dflags (hiSuf dflags) cwd
                   ((moduleNameSlashes . moduleName) mName)

  let modSummary = ModSummary { ms_mod = mName,
         ms_hsc_src = ExtCoreFile,
         ms_location = modLocation,
         -- By setting the object file timestamp to Nothing,
         -- we always force recompilation, which is what we
         -- want. (Thus it doesn't matter what the timestamp
         -- for the (nonexistent) source file is.)
         ms_hs_date = currentTime,
         ms_obj_date = Nothing,
         -- Only handling the single-module case for now, so no imports.
         ms_srcimps = [],
         ms_imps = [],
         -- No source file
         ms_hspp_file = "",
         ms_hspp_opts = dflags,
         ms_hspp_buf = Nothing
      }

  let maybe_simplify mod_guts | simplify = hscSimplify mod_guts
                              | otherwise = return mod_guts
  guts <- maybe_simplify (mkModGuts cm)
  (iface, changed, _details, cgguts)
      <- hscNormalIface guts Nothing
  hscWriteIface iface changed modSummary
  _ <- hscGenHardCode cgguts modSummary
  return ()

-- Makes a "vanilla" ModGuts.
mkModGuts :: CoreModule -> ModGuts
mkModGuts coreModule = ModGuts {
  mg_module = cm_module coreModule,
  mg_boot = False,
  mg_exports = [],
  mg_deps = noDependencies,
  mg_dir_imps = emptyModuleEnv,
  mg_used_names = emptyNameSet,
  mg_rdr_env = emptyGlobalRdrEnv,
  mg_fix_env = emptyFixityEnv,
  mg_types = emptyTypeEnv,
  mg_insts = [],
  mg_fam_insts = [],
  mg_rules = [],
  mg_binds = cm_binds coreModule,
  mg_foreign = NoStubs,
  mg_warns = NoWarnings,
  mg_anns = [],
  mg_hpc_info = emptyHpcInfo False,
  mg_modBreaks = emptyModBreaks,
  mg_vect_info = noVectInfo,
  mg_inst_env = emptyInstEnv,
  mg_fam_inst_env = emptyFamInstEnv
}

compileCore :: GhcMonad m => Bool -> FilePath -> m CoreModule
compileCore simplify fn = do
   -- First, set the target to the desired filename
   target <- guessTarget fn Nothing
   addTarget target
   _ <- load LoadAllTargets
   -- Then find dependencies
   modGraph <- depanal [] True
   case find ((== fn) . msHsFilePath) modGraph of
     Just modSummary -> do
       -- Now we have the module name;
       -- parse, typecheck and desugar the module
       mod_guts <- coreModule `fmap`
                      -- TODO: space leaky: call hsc* directly?
                      (desugarModule =<< typecheckModule =<< parseModule modSummary)
       liftM gutsToCoreModule $
         if simplify
          then do
             -- If simplify is true: simplify (hscSimplify), then tidy
             -- (tidyProgram).
             hsc_env <- getSession
             simpl_guts <- hscSimplify mod_guts
             tidy_guts <- liftIO $ tidyProgram hsc_env simpl_guts
             return $ Left tidy_guts
          else
             return $ Right mod_guts

     Nothing -> panic "compileToCoreModule: target FilePath not found in\
                           module dependency graph"
  where -- two versions, based on whether we simplify (thus run tidyProgram,
        -- which returns a (CgGuts, ModDetails) pair, or not (in which case
        -- we just have a ModGuts.
        gutsToCoreModule :: Either (CgGuts, ModDetails) ModGuts -> CoreModule
        gutsToCoreModule (Left (cg, md))  = CoreModule {
          cm_module = cg_module cg,    cm_types = md_types md,
          cm_imports = cg_dir_imps cg, cm_binds = cg_binds cg
        }
        gutsToCoreModule (Right mg) = CoreModule {
          cm_module  = mg_module mg,                   cm_types   = mg_types mg,
          cm_imports = moduleEnvKeys (mg_dir_imps mg), cm_binds   = mg_binds mg
         }

-- ---------------------------------------------------------------------------
-- Unloading

unload :: HscEnv -> [Linkable] -> IO ()
unload hsc_env stable_linkables	-- Unload everthing *except* 'stable_linkables'
  = case ghcLink (hsc_dflags hsc_env) of
#ifdef GHCI
	LinkInMemory -> Linker.unload (hsc_dflags hsc_env) stable_linkables
#else
	LinkInMemory -> panic "unload: no interpreter"
                                -- urgh.  avoid warnings:
                                hsc_env stable_linkables
#endif
	_other -> return ()

-- -----------------------------------------------------------------------------

{- |

  Stability tells us which modules definitely do not need to be recompiled.
  There are two main reasons for having stability:
  
   - avoid doing a complete upsweep of the module graph in GHCi when
     modules near the bottom of the tree have not changed.

   - to tell GHCi when it can load object code: we can only load object code
     for a module when we also load object code fo  all of the imports of the
     module.  So we need to know that we will definitely not be recompiling
     any of these modules, and we can use the object code.

  The stability check is as follows.  Both stableObject and
  stableBCO are used during the upsweep phase later.

@
  stable m = stableObject m || stableBCO m

  stableObject m = 
	all stableObject (imports m)
	&& old linkable does not exist, or is == on-disk .o
	&& date(on-disk .o) > date(.hs)

  stableBCO m =
	all stable (imports m)
	&& date(BCO) > date(.hs)
@

  These properties embody the following ideas:

    - if a module is stable, then:

	- if it has been compiled in a previous pass (present in HPT)
	  then it does not need to be compiled or re-linked.

        - if it has not been compiled in a previous pass,
	  then we only need to read its .hi file from disk and
	  link it to produce a 'ModDetails'.

    - if a modules is not stable, we will definitely be at least
      re-linking, and possibly re-compiling it during the 'upsweep'.
      All non-stable modules can (and should) therefore be unlinked
      before the 'upsweep'.

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

        scc_allimps = nub (filter home_module (concatMap ms_home_allimps scc))
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

-- -----------------------------------------------------------------------------

-- | Prune the HomePackageTable
--
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
        chew ((AcyclicSCC _):rest) = chew rest    -- acyclic?  not interesting.
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

-- | The upsweep
--
-- This is where we compile each module in the module graph, in a pass
-- from the bottom to the top of the graph.
--
-- There better had not be any cyclic groups here -- we check for them.

upsweep
    :: GhcMonad m =>
       HscEnv			-- ^ Includes initially-empty HPT
    -> HomePackageTable		-- ^ HPT from last time round (pruned)
    -> ([ModuleName],[ModuleName]) -- ^ stable modules (see checkStability)
    -> IO ()			-- ^ How to clean up unwanted tmp files
    -> [SCC ModSummary]		-- ^ Mods to do (the worklist)
    -> m (SuccessFlag,
         HscEnv,
         [ModSummary])
       -- ^ Returns:
       --
       --  1. A flag whether the complete upsweep was successful.
       --  2. The 'HscEnv' with an updated HPT
       --  3. A list of modules which succeeded loading.

upsweep hsc_env old_hpt stable_mods cleanup sccs = do
   (res, hsc_env, done) <- upsweep' hsc_env old_hpt [] sccs 1 (length sccs)
   return (res, hsc_env, reverse done)
 where

  upsweep' hsc_env _old_hpt done
     [] _ _
   = return (Succeeded, hsc_env, done)

  upsweep' hsc_env _old_hpt done
     (CyclicSCC ms:_) _ _
   = do liftIO $ fatalErrorMsg (hsc_dflags hsc_env) (cyclicModuleErr ms)
        return (Failed, hsc_env, done)

  upsweep' hsc_env old_hpt done
     (AcyclicSCC mod:mods) mod_index nmods
   = do -- putStrLn ("UPSWEEP_MOD: hpt = " ++ 
	--	     show (map (moduleUserString.moduleName.mi_module.hm_iface) 
	--		       (moduleEnvElts (hsc_HPT hsc_env)))
        let logger = reportModuleCompilationResult (hsc_callbacks hsc_env)

        mb_mod_info
            <- handleSourceError
                   (\err -> do logger mod (Just err); return Nothing) $ do
                 mod_info <- upsweep_mod hsc_env old_hpt stable_mods
                                         mod mod_index nmods
                 logger mod Nothing -- log warnings
                 return (Just mod_info)

        liftIO cleanup -- Remove unwanted tmp files between compilations

        case mb_mod_info of
          Nothing -> return (Failed, hsc_env, done)
          Just mod_info -> do
		let this_mod = ms_mod_name mod

			-- Add new info to hsc_env
		    hpt1     = addToUFM (hsc_HPT hsc_env) this_mod mod_info
		    hsc_env1 = hsc_env { hsc_HPT = hpt1 }

			-- Space-saving: delete the old HPT entry
			-- for mod BUT if mod is a hs-boot
			-- node, don't delete it.  For the
			-- interface, the HPT entry is probaby for the
			-- main Haskell source file.  Deleting it
			-- would force the real module to be recompiled
                        -- every time.
		    old_hpt1 | isBootSummary mod = old_hpt
			     | otherwise = delFromUFM old_hpt this_mod

                    done' = mod:done

                        -- fixup our HomePackageTable after we've finished compiling
                        -- a mutually-recursive loop.  See reTypecheckLoop, below.
                hsc_env2 <- liftIO $ reTypecheckLoop hsc_env1 mod done'

		upsweep' hsc_env2 old_hpt1 done' mods (mod_index+1) nmods

-- | Compile a single module.  Always produce a Linkable for it if
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: GhcMonad m =>
               HscEnv
            -> HomePackageTable
	    -> ([ModuleName],[ModuleName])
            -> ModSummary
            -> Int  -- index of module
            -> Int  -- total number of modules
            -> m HomeModInfo

upsweep_mod hsc_env old_hpt (stable_obj, stable_bco) summary mod_index nmods
   =    let 
       	    this_mod_name = ms_mod_name summary
	    this_mod    = ms_mod summary
	    mb_obj_date = ms_obj_date summary
	    obj_fn	= ml_obj_file (ms_location summary)
	    hs_date     = ms_hs_date summary

	    is_stable_obj = this_mod_name `elem` stable_obj
	    is_stable_bco = this_mod_name `elem` stable_bco

	    old_hmi = lookupUFM old_hpt this_mod_name

            -- We're using the dflags for this module now, obtained by
            -- applying any options in its LANGUAGE & OPTIONS_GHC pragmas.
            dflags = ms_hspp_opts summary
            prevailing_target = hscTarget (hsc_dflags hsc_env)
            local_target      = hscTarget dflags

            -- If OPTIONS_GHC contains -fasm or -fvia-C, be careful that
            -- we don't do anything dodgy: these should only work to change
            -- from -fvia-C to -fasm and vice-versa, otherwise we could 
            -- end up trying to link object code to byte code.
            target = if prevailing_target /= local_target
                        && (not (isObjectTarget prevailing_target)
                            || not (isObjectTarget local_target))
                        then prevailing_target
                        else local_target 

            -- store the corrected hscTarget into the summary
            summary' = summary{ ms_hspp_opts = dflags { hscTarget = target } }

	    -- The old interface is ok if
	    --	a) we're compiling a source file, and the old HPT
	    --	   entry is for a source file
	    --	b) we're compiling a hs-boot file
	    -- Case (b) allows an hs-boot file to get the interface of its
	    -- real source file on the second iteration of the compilation
	    -- manager, but that does no harm.  Otherwise the hs-boot file
	    -- will always be recompiled
            
            mb_old_iface 
	    	= case old_hmi of
	    	     Nothing	 			  -> Nothing
	    	     Just hm_info | isBootSummary summary -> Just iface
	    			  | not (mi_boot iface)   -> Just iface
	    			  | otherwise		  -> Nothing
	    			   where 
	    			     iface = hm_iface hm_info

	    compile_it :: GhcMonad m => Maybe Linkable -> m HomeModInfo
	    compile_it  = compile hsc_env summary' mod_index nmods mb_old_iface

            compile_it_discard_iface :: GhcMonad m =>
                                        Maybe Linkable -> m HomeModInfo
            compile_it_discard_iface 
                        = compile hsc_env summary' mod_index nmods Nothing

            -- With the HscNothing target we create empty linkables to avoid
            -- recompilation.  We have to detect these to recompile anyway if
            -- the target changed since the last compile.
            is_fake_linkable
               | Just hmi <- old_hmi, Just l <- hm_linkable hmi =
                  null (linkableUnlinked l)
               | otherwise =
                   -- we have no linkable, so it cannot be fake
                   False

            implies False _ = True
            implies True x  = x

        in
        case () of
         _
                -- Regardless of whether we're generating object code or
                -- byte code, we can always use an existing object file
                -- if it is *stable* (see checkStability).
          | is_stable_obj, Just hmi <- old_hmi -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "skipping stable obj mod:" <+> ppr this_mod_name)
                return hmi
                -- object is stable, and we have an entry in the
                -- old HPT: nothing to do

          | is_stable_obj, isNothing old_hmi -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "compiling stable on-disk mod:" <+> ppr this_mod_name)
                linkable <- liftIO $ findObjectLinkable this_mod obj_fn
                              (expectJust "upsweep1" mb_obj_date)
                compile_it (Just linkable)
                -- object is stable, but we need to load the interface
                -- off disk to make a HMI.

          | not (isObjectTarget target), is_stable_bco,
            (target /= HscNothing) `implies` not is_fake_linkable ->
                ASSERT(isJust old_hmi) -- must be in the old_hpt
                let Just hmi = old_hmi in do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "skipping stable BCO mod:" <+> ppr this_mod_name)
                return hmi
                -- BCO is stable: nothing to do

          | not (isObjectTarget target),
            Just hmi <- old_hmi,
            Just l <- hm_linkable hmi,
            not (isObjectLinkable l),
            (target /= HscNothing) `implies` not is_fake_linkable,
            linkableTime l >= ms_hs_date summary -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "compiling non-stable BCO mod:" <+> ppr this_mod_name)
                compile_it (Just l)
                -- we have an old BCO that is up to date with respect
                -- to the source: do a recompilation check as normal.

          -- When generating object code, if there's an up-to-date
          -- object file on the disk, then we can use it.
          -- However, if the object file is new (compared to any
          -- linkable we had from a previous compilation), then we
          -- must discard any in-memory interface, because this
          -- means the user has compiled the source file
          -- separately and generated a new interface, that we must
          -- read from the disk.
          --
          | isObjectTarget target,
            Just obj_date <- mb_obj_date,
            obj_date >= hs_date -> do
                case old_hmi of
                  Just hmi
                    | Just l <- hm_linkable hmi,
                      isObjectLinkable l && linkableTime l == obj_date -> do
                          liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                                     (text "compiling mod with new on-disk obj:" <+> ppr this_mod_name)
                          compile_it (Just l)
                  _otherwise -> do
                          liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                                     (text "compiling mod with new on-disk obj2:" <+> ppr this_mod_name)
                          linkable <- liftIO $ findObjectLinkable this_mod obj_fn obj_date
                          compile_it_discard_iface (Just linkable)

         _otherwise -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "compiling mod:" <+> ppr this_mod_name)
                compile_it Nothing



-- Filter modules in the HPT
retainInTopLevelEnvs :: [ModuleName] -> HomePackageTable -> HomePackageTable
retainInTopLevelEnvs keep_these hpt
   = listToUFM   [ (mod, expectJust "retain" mb_mod_info)
		 | mod <- keep_these
		 , let mb_mod_info = lookupUFM hpt mod
		 , isJust mb_mod_info ]

-- ---------------------------------------------------------------------------
-- Typecheck module loops

{-
See bug #930.  This code fixes a long-standing bug in --make.  The
problem is that when compiling the modules *inside* a loop, a data
type that is only defined at the top of the loop looks opaque; but
after the loop is done, the structure of the data type becomes
apparent.

The difficulty is then that two different bits of code have
different notions of what the data type looks like.

The idea is that after we compile a module which also has an .hs-boot
file, we re-generate the ModDetails for each of the modules that
depends on the .hs-boot file, so that everyone points to the proper
TyCons, Ids etc. defined by the real module, not the boot module.
Fortunately re-generating a ModDetails from a ModIface is easy: the
function TcIface.typecheckIface does exactly that.

Picking the modules to re-typecheck is slightly tricky.  Starting from
the module graph consisting of the modules that have already been
compiled, we reverse the edges (so they point from the imported module
to the importing module), and depth-first-search from the .hs-boot
node.  This gives us all the modules that depend transitively on the
.hs-boot module, and those are exactly the modules that we need to
re-typecheck.

Following this fix, GHC can compile itself with --make -O2.
-}

reTypecheckLoop :: HscEnv -> ModSummary -> ModuleGraph -> IO HscEnv
reTypecheckLoop hsc_env ms graph
  | not (isBootSummary ms) && 
    any (\m -> ms_mod m == this_mod && isBootSummary m) graph
  = do
        let mss = reachableBackwards (ms_mod_name ms) graph
            non_boot = filter (not.isBootSummary) mss
        debugTraceMsg (hsc_dflags hsc_env) 2 $
           text "Re-typechecking loop: " <> ppr (map ms_mod_name non_boot)
        typecheckLoop hsc_env (map ms_mod_name non_boot)
  | otherwise
  = return hsc_env
 where
  this_mod = ms_mod ms

typecheckLoop :: HscEnv -> [ModuleName] -> IO HscEnv
typecheckLoop hsc_env mods = do
  new_hpt <-
    fixIO $ \new_hpt -> do
      let new_hsc_env = hsc_env{ hsc_HPT = new_hpt }
      mds <- initIfaceCheck new_hsc_env $ 
                mapM (typecheckIface . hm_iface) hmis
      let new_hpt = addListToUFM old_hpt 
                        (zip mods [ hmi{ hm_details = details }
                                  | (hmi,details) <- zip hmis mds ])
      return new_hpt
  return hsc_env{ hsc_HPT = new_hpt }
  where
    old_hpt = hsc_HPT hsc_env
    hmis    = map (expectJust "typecheckLoop" . lookupUFM old_hpt) mods

reachableBackwards :: ModuleName -> [ModSummary] -> [ModSummary]
reachableBackwards mod summaries
  = [ ms | (ms,_,_) <- reachableG (transposeG graph) root ]
  where -- the rest just sets up the graph:
        (graph, lookup_node) = moduleGraphNodes False summaries
        root  = expectJust "reachableBackwards" (lookup_node HsBootFile mod)

-- ---------------------------------------------------------------------------
-- Topological sort of the module graph

type SummaryNode = (ModSummary, Int, [Int])

topSortModuleGraph
	  :: Bool
          -- ^ Drop hi-boot nodes? (see below)
	  -> [ModSummary]
	  -> Maybe ModuleName
             -- ^ Root module name.  If @Nothing@, use the full graph.
	  -> [SCC ModSummary]
-- ^ Calculate SCCs of the module graph, possibly dropping the hi-boot nodes
-- The resulting list of strongly-connected-components is in topologically
-- sorted order, starting with the module(s) at the bottom of the
-- dependency graph (ie compile them first) and ending with the ones at
-- the top.
--
-- Drop hi-boot nodes (first boolean arg)? 
--
-- - @False@:	treat the hi-boot summaries as nodes of the graph,
--		so the graph must be acyclic
--
-- - @True@:	eliminate the hi-boot nodes, and instead pretend
--		the a source-import of Foo is an import of Foo
--		The resulting graph has no hi-boot nodes, but can be cyclic

topSortModuleGraph drop_hs_boot_nodes summaries mb_root_mod
  = map (fmap summaryNodeSummary) $ stronglyConnCompG initial_graph
  where
    (graph, lookup_node) = moduleGraphNodes drop_hs_boot_nodes summaries
    
    initial_graph = case mb_root_mod of
        Nothing -> graph
        Just root_mod ->
            -- restrict the graph to just those modules reachable from
            -- the specified module.  We do this by building a graph with
            -- the full set of nodes, and determining the reachable set from
            -- the specified node.
            let root | Just node <- lookup_node HsSrcFile root_mod, graph `hasVertexG` node = node
                     | otherwise = ghcError (ProgramError "module does not exist")
            in graphFromEdgedVertices (seq root (reachableG graph root))

summaryNodeKey :: SummaryNode -> Int
summaryNodeKey (_, k, _) = k

summaryNodeSummary :: SummaryNode -> ModSummary
summaryNodeSummary (s, _, _) = s

moduleGraphNodes :: Bool -> [ModSummary]
  -> (Graph SummaryNode, HscSource -> ModuleName -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries = (graphFromEdgedVertices nodes, lookup_node)
  where
    numbered_summaries = zip summaries [1..]

    lookup_node :: HscSource -> ModuleName -> Maybe SummaryNode
    lookup_node hs_src mod = Map.lookup (mod, hs_src) node_map

    lookup_key :: HscSource -> ModuleName -> Maybe Int
    lookup_key hs_src mod = fmap summaryNodeKey (lookup_node hs_src mod)

    node_map :: NodeMap SummaryNode
    node_map = Map.fromList [ ((moduleName (ms_mod s), ms_hsc_src s), node)
                            | node@(s, _, _) <- nodes ]

    -- We use integers as the keys for the SCC algorithm
    nodes :: [SummaryNode]
    nodes = [ (s, key, out_keys)
            | (s, key) <- numbered_summaries
             -- Drop the hi-boot ones if told to do so
            , not (isBootSummary s && drop_hs_boot_nodes)
            , let out_keys = out_edge_keys hs_boot_key (map unLoc (ms_home_srcimps s)) ++
                             out_edge_keys HsSrcFile   (map unLoc (ms_home_imps s)) ++
                             (-- see [boot-edges] below
                              if drop_hs_boot_nodes || ms_hsc_src s == HsBootFile 
                              then [] 
                              else case lookup_key HsBootFile (ms_mod_name s) of
                                    Nothing -> []
                                    Just k  -> [k]) ]

    -- [boot-edges] if this is a .hs and there is an equivalent
    -- .hs-boot, add a link from the former to the latter.  This
    -- has the effect of detecting bogus cases where the .hs-boot
    -- depends on the .hs, by introducing a cycle.  Additionally,
    -- it ensures that we will always process the .hs-boot before
    -- the .hs, and so the HomePackageTable will always have the
    -- most up to date information.

    -- Drop hs-boot nodes by using HsSrcFile as the key
    hs_boot_key | drop_hs_boot_nodes = HsSrcFile
                | otherwise          = HsBootFile

    out_edge_keys :: HscSource -> [ModuleName] -> [Int]
    out_edge_keys hi_boot ms = mapCatMaybes (lookup_key hi_boot) ms
        -- If we want keep_hi_boot_nodes, then we do lookup_key with
        -- the IsBootInterface parameter True; else False


type NodeKey   = (ModuleName, HscSource)  -- The nodes of the graph are 
type NodeMap a = Map NodeKey a	  -- keyed by (mod, src_file_type) pairs

msKey :: ModSummary -> NodeKey
msKey (ModSummary { ms_mod = mod, ms_hsc_src = boot }) = (moduleName mod,boot)

mkNodeMap :: [ModSummary] -> NodeMap ModSummary
mkNodeMap summaries = Map.fromList [ (msKey s, s) | s <- summaries]
	
nodeMapElts :: NodeMap a -> [a]
nodeMapElts = Map.elems

-- | If there are {-# SOURCE #-} imports between strongly connected
-- components in the topological sort, then those imports can
-- definitely be replaced by ordinary non-SOURCE imports: if SOURCE
-- were necessary, then the edge would be part of a cycle.
warnUnnecessarySourceImports :: GhcMonad m => [SCC ModSummary] -> m ()
warnUnnecessarySourceImports sccs =
  logWarnings (listToBag (concatMap (check.flattenSCC) sccs))
  where check ms =
	   let mods_in_this_cycle = map ms_mod_name ms in
	   [ warn i | m <- ms, i <- ms_home_srcimps m,
	              unLoc i `notElem`  mods_in_this_cycle ]

	warn :: Located ModuleName -> WarnMsg
	warn (L loc mod) = 
	   mkPlainErrMsg loc
		(ptext (sLit "Warning: {-# SOURCE #-} unnecessary in import of ")
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

downsweep :: GhcMonad m =>
             HscEnv
	  -> [ModSummary]	-- Old summaries
	  -> [ModuleName]	-- Ignore dependencies on these; treat
				-- them as if they were package modules
	  -> Bool		-- True <=> allow multiple targets to have 
				-- 	    the same module name; this is 
				--	    very useful for ghc -M
	  -> m [ModSummary]
		-- The elts of [ModSummary] all have distinct
		-- (Modules, IsBoot) identifiers, unless the Bool is true
		-- in which case there can be repeats
downsweep hsc_env old_summaries excl_mods allow_dup_roots
   = do -- catch error messages and return them
     --handleErrMsg   -- should be covered by GhcMonad now
     --          (\err_msg -> printBagOfErrors (hsc_dflags hsc_env) (unitBag err_msg) >> return Nothing) $ do
       rootSummaries <- mapM getRootSummary roots
       let root_map = mkRootMap rootSummaries
       checkDuplicates root_map
       summs <- loop (concatMap msDeps rootSummaries) root_map
       return summs
     where
	roots = hsc_targets hsc_env

	old_summary_map :: NodeMap ModSummary
	old_summary_map = mkNodeMap old_summaries

	getRootSummary :: GhcMonad m => Target -> m ModSummary
	getRootSummary (Target (TargetFile file mb_phase) obj_allowed maybe_buf)
	   = do exists <- liftIO $ doesFileExist file
		if exists 
		    then summariseFile hsc_env old_summaries file mb_phase 
                                       obj_allowed maybe_buf
		    else throwOneError $ mkPlainErrMsg noSrcSpan $
			   text "can't find file:" <+> text file
	getRootSummary (Target (TargetModule modl) obj_allowed maybe_buf)
 	   = do maybe_summary <- summariseModule hsc_env old_summary_map False 
					   (L rootLoc modl) obj_allowed 
                                           maybe_buf excl_mods
		case maybe_summary of
		   Nothing -> packageModErr modl
		   Just s  -> return s

	rootLoc = mkGeneralSrcSpan (fsLit "<command line>")

	-- In a root module, the filename is allowed to diverge from the module
	-- name, so we have to check that there aren't multiple root files
	-- defining the same module (otherwise the duplicates will be silently
 	-- ignored, leading to confusing behaviour).
	checkDuplicates :: GhcMonad m => NodeMap [ModSummary] -> m ()
	checkDuplicates root_map 
	   | allow_dup_roots = return ()
	   | null dup_roots  = return ()
	   | otherwise	     = liftIO $ multiRootsErr (head dup_roots)
	   where
	     dup_roots :: [[ModSummary]]	-- Each at least of length 2
	     dup_roots = filterOut isSingleton (nodeMapElts root_map)

	loop :: GhcMonad m =>
                [(Located ModuleName,IsBootInterface)]
			-- Work list: process these modules
	     -> NodeMap [ModSummary]
		 	-- Visited set; the range is a list because
			-- the roots can have the same module names
			-- if allow_dup_roots is True
	     -> m [ModSummary]
			-- The result includes the worklist, except
			-- for those mentioned in the visited set
	loop [] done 	  = return (concat (nodeMapElts done))
	loop ((wanted_mod, is_boot) : ss) done 
	  | Just summs <- Map.lookup key done
	  = if isSingleton summs then
		loop ss done
	    else
		do { liftIO $ multiRootsErr summs; return [] }
	  | otherwise
          = do mb_s <- summariseModule hsc_env old_summary_map 
                                       is_boot wanted_mod True
                                       Nothing excl_mods
               case mb_s of
                   Nothing -> loop ss done
                   Just s  -> loop (msDeps s ++ ss) (Map.insert key [s] done)
	  where
	    key = (unLoc wanted_mod, if is_boot then HsBootFile else HsSrcFile)

-- XXX Does the (++) here need to be flipped?
mkRootMap :: [ModSummary] -> NodeMap [ModSummary]
mkRootMap summaries = Map.insertListWith (flip (++))
                                         [ (msKey s, [s]) | s <- summaries ]
                                         Map.empty

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
    concat [ [(m,True), (m,False)] | m <- ms_home_srcimps s ] 
	 ++ [ (m,False) | m <- ms_home_imps s ] 

home_imps :: [Located (ImportDecl RdrName)] -> [Located ModuleName]
home_imps imps = [ ideclName i |  L _ i <- imps, isLocal (ideclPkgQual i) ]
  where isLocal Nothing = True
        isLocal (Just pkg) | pkg == fsLit "this" = True -- "this" is special
        isLocal _ = False

ms_home_allimps :: ModSummary -> [ModuleName]
ms_home_allimps ms = map unLoc (ms_home_srcimps ms ++ ms_home_imps ms)

ms_home_srcimps :: ModSummary -> [Located ModuleName]
ms_home_srcimps = home_imps . ms_srcimps

ms_home_imps :: ModSummary -> [Located ModuleName]
ms_home_imps = home_imps . ms_imps

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
	:: GhcMonad m =>
           HscEnv
	-> [ModSummary]			-- old summaries
	-> FilePath			-- source file name
	-> Maybe Phase			-- start phase
        -> Bool                         -- object code allowed?
	-> Maybe (StringBuffer,ClockTime)
	-> m ModSummary

summariseFile hsc_env old_summaries file mb_phase obj_allowed maybe_buf
	-- we can use a cached summary if one is available and the
	-- source file hasn't changed,  But we have to look up the summary
	-- by source file, rather than module name as we do in summarise.
   | Just old_summary <- findSummaryBySourceFile old_summaries file
   = do
	let location = ms_location old_summary

		-- return the cached summary if the source didn't change
	src_timestamp <- case maybe_buf of
			   Just (_,t) -> return t
			   Nothing    -> liftIO $ getModificationTime file
		-- The file exists; we checked in getRootSummary above.
		-- If it gets removed subsequently, then this 
		-- getModificationTime may fail, but that's the right
		-- behaviour.

	if ms_hs_date old_summary == src_timestamp 
	   then do -- update the object-file timestamp
        	  obj_timestamp <-
                    if isObjectTarget (hscTarget (hsc_dflags hsc_env)) 
                        || obj_allowed -- bug #1205
                        then liftIO $ getObjTimestamp location False
                        else return Nothing
		  return old_summary{ ms_obj_date = obj_timestamp }
	   else
		new_summary

   | otherwise
   = new_summary
  where
    new_summary = do
   	let dflags = hsc_dflags hsc_env

	(dflags', hspp_fn, buf)
	    <- preprocessFile hsc_env file mb_phase maybe_buf

        (srcimps,the_imps, L _ mod_name) <- getImports dflags' buf hspp_fn file

	-- Make a ModLocation for this file
	location <- liftIO $ mkHomeModLocation dflags mod_name file

	-- Tell the Finder cache where it is, so that subsequent calls
	-- to findModule will find it, even if it's not on any search path
	mod <- liftIO $ addHomeModuleToFinder hsc_env mod_name location

        src_timestamp <- case maybe_buf of
			   Just (_,t) -> return t
			   Nothing    -> liftIO $ getModificationTime file
			-- getMofificationTime may fail

        -- when the user asks to load a source file by name, we only
        -- use an object file if -fobject-code is on.  See #1205.
	obj_timestamp <-
            if isObjectTarget (hscTarget (hsc_dflags hsc_env)) 
               || obj_allowed -- bug #1205
                then liftIO $ modificationTimeIfExists (ml_obj_file location)
                else return Nothing

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
	(x:_) -> Just x

-- Summarise a module, and pick up source and timestamp.
summariseModule
	  :: GhcMonad m =>
             HscEnv
	  -> NodeMap ModSummary	-- Map of old summaries
	  -> IsBootInterface	-- True <=> a {-# SOURCE #-} import
	  -> Located ModuleName	-- Imported module to be summarised
          -> Bool               -- object code allowed?
	  -> Maybe (StringBuffer, ClockTime)
	  -> [ModuleName]		-- Modules to exclude
	  -> m (Maybe ModSummary)	-- Its new summary

summariseModule hsc_env old_summary_map is_boot (L loc wanted_mod) 
                obj_allowed maybe_buf excl_mods
  | wanted_mod `elem` excl_mods
  = return Nothing

  | Just old_summary <- Map.lookup (wanted_mod, hsc_src) old_summary_map
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
		m <- liftIO $ System.IO.Error.try (getModificationTime src_fn)
		case m of
		   Right t -> check_timestamp old_summary location src_fn t
		   Left e | isDoesNotExistError e -> find_it
		          | otherwise             -> liftIO $ ioError e

  | otherwise  = find_it
  where
    dflags = hsc_dflags hsc_env

    hsc_src = if is_boot then HsBootFile else HsSrcFile

    check_timestamp old_summary location src_fn src_timestamp
	| ms_hs_date old_summary == src_timestamp = do
		-- update the object-file timestamp
                obj_timestamp <- liftIO $
                    if isObjectTarget (hscTarget (hsc_dflags hsc_env))
                       || obj_allowed -- bug #1205
                       then getObjTimestamp location is_boot
                       else return Nothing
		return (Just old_summary{ ms_obj_date = obj_timestamp })
	| otherwise = 
		-- source changed: re-summarise.
		new_summary location (ms_mod old_summary) src_fn src_timestamp

    find_it = do
	-- Don't use the Finder's cache this time.  If the module was
	-- previously a package module, it may have now appeared on the
	-- search path, so we want to consider it to be a home module.  If
	-- the module was previously a home module, it may have moved.
	liftIO $ uncacheModule hsc_env wanted_mod
	found <- liftIO $ findImportedModule hsc_env wanted_mod Nothing
	case found of
	     Found location mod 
		| isJust (ml_hs_file location) ->
			-- Home package
			 just_found location mod
		| otherwise -> 
			-- Drop external-pkg
			ASSERT(modulePackageId mod /= thisPackage dflags)
			return Nothing
			
	     err -> liftIO $ noModError dflags loc wanted_mod err
			-- Not found

    just_found location mod = do
	  	-- Adjust location to point to the hs-boot source file, 
		-- hi file, object file, when is_boot says so
	let location' | is_boot   = addBootSuffixLocn location
		      | otherwise = location
	    src_fn = expectJust "summarise2" (ml_hs_file location')

		-- Check that it exists
	  	-- It might have been deleted since the Finder last found it
	maybe_t <- liftIO $ modificationTimeIfExists src_fn
	case maybe_t of
	  Nothing -> noHsFileErr loc src_fn
	  Just t  -> new_summary location' mod src_fn t


    new_summary location mod src_fn src_timestamp
      = do
	-- Preprocess the source file and get its imports
	-- The dflags' contains the OPTIONS pragmas
	(dflags', hspp_fn, buf) <- preprocessFile hsc_env src_fn Nothing maybe_buf
        (srcimps, the_imps, L mod_loc mod_name) <- getImports dflags' buf hspp_fn src_fn

	when (mod_name /= wanted_mod) $
		throwOneError $ mkPlainErrMsg mod_loc $ 
			      text "File name does not match module name:" 
			      $$ text "Saw:" <+> quotes (ppr mod_name)
                              $$ text "Expected:" <+> quotes (ppr wanted_mod)

		-- Find the object timestamp, and return the summary
	obj_timestamp <- liftIO $
           if isObjectTarget (hscTarget (hsc_dflags hsc_env))
              || obj_allowed -- bug #1205
              then getObjTimestamp location is_boot
              else return Nothing

	return (Just (ModSummary { ms_mod       = mod,
			      ms_hsc_src   = hsc_src,
			      ms_location  = location,
			      ms_hspp_file = hspp_fn,
                              ms_hspp_opts = dflags',
			      ms_hspp_buf  = Just buf,
			      ms_srcimps   = srcimps,
			      ms_imps      = the_imps,
			      ms_hs_date   = src_timestamp,
			      ms_obj_date  = obj_timestamp }))


getObjTimestamp :: ModLocation -> Bool -> IO (Maybe ClockTime)
getObjTimestamp location is_boot
  = if is_boot then return Nothing
	       else modificationTimeIfExists (ml_obj_file location)


preprocessFile :: GhcMonad m =>
                  HscEnv
               -> FilePath
               -> Maybe Phase -- ^ Starting phase
               -> Maybe (StringBuffer,ClockTime)
               -> m (DynFlags, FilePath, StringBuffer)
preprocessFile hsc_env src_fn mb_phase Nothing
  = do
	(dflags', hspp_fn) <- preprocess hsc_env (src_fn, mb_phase)
	buf <- liftIO $ hGetStringBuffer hspp_fn
	return (dflags', hspp_fn, buf)

preprocessFile hsc_env src_fn mb_phase (Just (buf, _time))
  = do
        let dflags = hsc_dflags hsc_env
	-- case we bypass the preprocessing stage?
	let 
	    local_opts = getOptions dflags buf src_fn
	--
	(dflags', leftovers, warns)
            <- parseDynamicNoPackageFlags dflags local_opts
        checkProcessArgsResult leftovers
        handleFlagWarnings dflags' warns

	let
	    needs_preprocessing
		| Just (Unlit _) <- mb_phase    = True
	        | Nothing <- mb_phase, Unlit _ <- startPhase src_fn  = True
		  -- note: local_opts is only required if there's no Unlit phase
		| xopt Opt_Cpp dflags'		= True
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
  = throwOneError $ mkPlainErrMsg loc $ cannotFindModule dflags wanted_mod err
				
noHsFileErr :: GhcMonad m => SrcSpan -> String -> m a
noHsFileErr loc path
  = throwOneError $ mkPlainErrMsg loc $ text "Can't find" <+> text path
 
packageModErr :: GhcMonad m => ModuleName -> m a
packageModErr mod
  = throwOneError $ mkPlainErrMsg noSrcSpan $
	text "module" <+> quotes (ppr mod) <+> text "is a package module"

multiRootsErr :: [ModSummary] -> IO ()
multiRootsErr [] = panic "multiRootsErr"
multiRootsErr summs@(summ1:_)
  = throwOneError $ mkPlainErrMsg noSrcSpan $
	text "module" <+> quotes (ppr mod) <+> 
	text "is defined in multiple files:" <+>
	sep (map text files)
  where
    mod = ms_mod summ1
    files = map (expectJust "checkDup" . ml_hs_file . ms_location) summs

cyclicModuleErr :: [ModSummary] -> SDoc
cyclicModuleErr ms
  = hang (ptext (sLit "Module imports form a cycle for modules:"))
       2 (vcat (map show_one ms))
  where
    mods_in_cycle = map ms_mod_name ms
    imp_modname = unLoc . ideclName . unLoc
    just_in_cycle = filter ((`elem` mods_in_cycle) . imp_modname)

    show_one ms = 
           vcat [ show_mod (ms_hsc_src ms) (ms_mod_name ms) <+>
                  maybe empty (parens . text) (ml_hs_file (ms_location ms)),
                  nest 2 $ ptext (sLit "imports:") <+> vcat [
                     pp_imps HsBootFile (just_in_cycle $ ms_srcimps ms),
                     pp_imps HsSrcFile  (just_in_cycle $ ms_imps ms) ]
                ]
    show_mod hsc_src mod = ppr mod <> text (hscSourceString hsc_src)
    pp_imps src imps = fsep (map (show_mod src . unLoc . ideclName . unLoc) imps)


-- | Inform GHC that the working directory has changed.  GHC will flush
-- its cache of module locations, since it may no longer be valid.
-- 
-- Note: Before changing the working directory make sure all threads running
-- in the same session have stopped.  If you change the working directory,
-- you should also unload the current program (set targets to empty,
-- followed by load).
workingDirectoryChanged :: GhcMonad m => m ()
workingDirectoryChanged = withSession $ (liftIO . flushFinderCaches)

-- -----------------------------------------------------------------------------
-- inspecting the session

-- | Get the module dependency graph.
getModuleGraph :: GhcMonad m => m ModuleGraph -- ToDo: DiGraph ModSummary
getModuleGraph = liftM hsc_mod_graph getSession

-- | Determines whether a set of modules requires Template Haskell.
--
-- Note that if the session's 'DynFlags' enabled Template Haskell when
-- 'depanal' was called, then each module in the returned module graph will
-- have Template Haskell enabled whether it is actually needed or not.
needsTemplateHaskell :: ModuleGraph -> Bool
needsTemplateHaskell ms =
    any (xopt Opt_TemplateHaskell . ms_hspp_opts) ms

-- | Return @True@ <==> module is loaded.
isLoaded :: GhcMonad m => ModuleName -> m Bool
isLoaded m = withSession $ \hsc_env ->
  return $! isJust (lookupUFM (hsc_HPT hsc_env) m)

-- | Return the bindings for the current interactive session.
getBindings :: GhcMonad m => m [TyThing]
getBindings = withSession $ \hsc_env ->
   -- we have to implement the shadowing behaviour of ic_tmp_ids here
   -- (see InteractiveContext) and the quickest way is to use an OccEnv.
   let 
       occ_env = mkOccEnv [ (nameOccName (idName id), AnId id) 
                          | id <- ic_tmp_ids (hsc_IC hsc_env) ]
   in
   return (occEnvElts occ_env)

getPrintUnqual :: GhcMonad m => m PrintUnqualified
getPrintUnqual = withSession $ \hsc_env ->
  return (icPrintUnqual (hsc_dflags hsc_env) (hsc_IC hsc_env))

-- | Container for information about a 'Module'.
data ModuleInfo = ModuleInfo {
	minf_type_env  :: TypeEnv,
	minf_exports   :: NameSet, -- ToDo, [AvailInfo] like ModDetails?
	minf_rdr_env   :: Maybe GlobalRdrEnv,	-- Nothing for a compiled/package mod
	minf_instances :: [Instance]
#ifdef GHCI
        ,minf_modBreaks :: ModBreaks 
#endif
	-- ToDo: this should really contain the ModIface too
  }
	-- We don't want HomeModInfo here, because a ModuleInfo applies
	-- to package modules too.

-- | Request information about a loaded 'Module'
getModuleInfo :: GhcMonad m => Module -> m (Maybe ModuleInfo)  -- XXX: Maybe X
getModuleInfo mdl = withSession $ \hsc_env -> do
  let mg = hsc_mod_graph hsc_env
  if mdl `elem` map ms_mod mg
	then liftIO $ getHomeModuleInfo hsc_env (moduleName mdl)
	else do
  {- if isHomeModule (hsc_dflags hsc_env) mdl
	then return Nothing
	else -} liftIO $ getPackageModuleInfo hsc_env mdl
   -- getPackageModuleInfo will attempt to find the interface, so
   -- we don't want to call it for a home module, just in case there
   -- was a problem loading the module and the interface doesn't
   -- exist... hence the isHomeModule test here.  (ToDo: reinstate)

getPackageModuleInfo :: HscEnv -> Module -> IO (Maybe ModuleInfo)
#ifdef GHCI
getPackageModuleInfo hsc_env mdl = do
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
			minf_rdr_env   = Just $! availsToGlobalRdrEnv (moduleName mdl) avails,
			minf_instances = error "getModuleInfo: instances for package module unimplemented",
                        minf_modBreaks = emptyModBreaks  
		}))
#else
getPackageModuleInfo _hsc_env _mdl = do
  -- bogusly different for non-GHCI (ToDo)
  return Nothing
#endif

getHomeModuleInfo :: HscEnv -> ModuleName -> IO (Maybe ModuleInfo)
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
#ifdef GHCI
                       ,minf_modBreaks = getModBreaks hmi
#endif
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

mkPrintUnqualifiedForModule :: GhcMonad m =>
                               ModuleInfo
                            -> m (Maybe PrintUnqualified) -- XXX: returns a Maybe X
mkPrintUnqualifiedForModule minf = withSession $ \hsc_env -> do
  return (fmap (mkPrintUnqualified (hsc_dflags hsc_env)) (minf_rdr_env minf))

modInfoLookupName :: GhcMonad m =>
                     ModuleInfo -> Name
                  -> m (Maybe TyThing) -- XXX: returns a Maybe X
modInfoLookupName minf name = withSession $ \hsc_env -> do
   case lookupTypeEnv (minf_type_env minf) name of
     Just tyThing -> return (Just tyThing)
     Nothing      -> do
       eps <- liftIO $ readIORef (hsc_EPS hsc_env)
       return $! lookupType (hsc_dflags hsc_env) 
			    (hsc_HPT hsc_env) (eps_PTE eps) name

#ifdef GHCI
modInfoModBreaks :: ModuleInfo -> ModBreaks
modInfoModBreaks = minf_modBreaks  
#endif

isDictonaryId :: Id -> Bool
isDictonaryId id
  = case tcSplitSigmaTy (idType id) of { (_tvs, _theta, tau) -> isDictTy tau }

-- | Looks up a global name: that is, any top-level name in any
-- visible module.  Unlike 'lookupName', lookupGlobalName does not use
-- the interactive context, and therefore does not require a preceding
-- 'setContext'.
lookupGlobalName :: GhcMonad m => Name -> m (Maybe TyThing)
lookupGlobalName name = withSession $ \hsc_env -> do
   liftIO $ lookupTypeHscEnv hsc_env name

findGlobalAnns :: (GhcMonad m, Typeable a) => ([Word8] -> a) -> AnnTarget Name -> m [a]
findGlobalAnns deserialize target = withSession $ \hsc_env -> do
    ann_env <- liftIO $ prepareAnnotations hsc_env Nothing
    return (findAnns deserialize ann_env target)

#ifdef GHCI
-- | get the GlobalRdrEnv for a session
getGRE :: GhcMonad m => m GlobalRdrEnv
getGRE = withSession $ \hsc_env-> return $ ic_rn_gbl_env (hsc_IC hsc_env)
#endif

-- -----------------------------------------------------------------------------

-- | Return all /external/ modules available in the package database.
-- Modules from the current session (i.e., from the 'HomePackageTable') are
-- not included.
packageDbModules :: GhcMonad m =>
                    Bool  -- ^ Only consider exposed packages.
                 -> m [Module]
packageDbModules only_exposed = do
   dflags <- getSessionDynFlags
   let pkgs = eltsUFM (pkgIdMap (pkgState dflags))
   return $
     [ mkModule pid modname | p <- pkgs
                            , not only_exposed || exposed p
                            , let pid = packageConfigId p
                            , modname <- exposedModules p ]

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

#endif

-- Extract the filename, stringbuffer content and dynflags associed to a module
--
-- XXX: Explain pre-conditions
getModuleSourceAndFlags :: GhcMonad m => Module -> m (String, StringBuffer, DynFlags)
getModuleSourceAndFlags mod = do
  m <- getModSummary (moduleName mod)
  case ml_hs_file $ ms_location m of
    Nothing -> throw $ mkApiErr (text "No source available for module " <+> ppr mod)
    Just sourceFile -> do
        source <- liftIO $ hGetStringBuffer sourceFile
        return (sourceFile, source, ms_hspp_opts m)


-- | Return module source as token stream, including comments.
--
-- The module must be in the module graph and its source must be available.
-- Throws a 'HscTypes.SourceError' on parse error.
getTokenStream :: GhcMonad m => Module -> m [Located Token]
getTokenStream mod = do
  (sourceFile, source, flags) <- getModuleSourceAndFlags mod
  let startLoc = mkSrcLoc (mkFastString sourceFile) 1 1
  case lexTokenStream source startLoc flags of
    POk _ ts  -> return ts
    PFailed span err -> throw $ mkSrcErr (unitBag $ mkPlainErrMsg span err)

-- | Give even more information on the source than 'getTokenStream'
-- This function allows reconstructing the source completely with
-- 'showRichTokenStream'.
getRichTokenStream :: GhcMonad m => Module -> m [(Located Token, String)]
getRichTokenStream mod = do
  (sourceFile, source, flags) <- getModuleSourceAndFlags mod
  let startLoc = mkSrcLoc (mkFastString sourceFile) 1 1
  case lexTokenStream source startLoc flags of
    POk _ ts -> return $ addSourceToTokens startLoc source ts
    PFailed span err -> throw $ mkSrcErr (unitBag $ mkPlainErrMsg span err)

-- | Given a source location and a StringBuffer corresponding to this
-- location, return a rich token stream with the source associated to the
-- tokens.
addSourceToTokens :: SrcLoc -> StringBuffer -> [Located Token]
                  -> [(Located Token, String)]
addSourceToTokens _ _ [] = []
addSourceToTokens loc buf (t@(L span _) : ts)
    | not (isGoodSrcSpan span) = (t,"") : addSourceToTokens loc buf ts
    | otherwise = (t,str) : addSourceToTokens newLoc newBuf ts
    where
      (newLoc, newBuf, str) = go "" loc buf
      start = srcSpanStart span
      end = srcSpanEnd span
      go acc loc buf | loc < start = go acc nLoc nBuf
                     | start <= loc && loc < end = go (ch:acc) nLoc nBuf
                     | otherwise = (loc, buf, reverse acc)
          where (ch, nBuf) = nextChar buf
                nLoc = advanceSrcLoc loc ch


-- | Take a rich token stream such as produced from 'getRichTokenStream' and
-- return source code almost identical to the original code (except for
-- insignificant whitespace.)
showRichTokenStream :: [(Located Token, String)] -> String
showRichTokenStream ts = go startLoc ts ""
    where sourceFile = srcSpanFile (getLoc . fst . head $ ts)
          startLoc = mkSrcLoc sourceFile 1 1
          go _ [] = id
          go loc ((L span _, str):ts)
              | not (isGoodSrcSpan span) = go loc ts
              | locLine == tokLine = ((replicate (tokCol - locCol) ' ') ++)
                                     . (str ++)
                                     . go tokEnd ts
              | otherwise = ((replicate (tokLine - locLine) '\n') ++)
                            . ((replicate tokCol ' ') ++)
                            . (str ++)
                            . go tokEnd ts
              where (locLine, locCol) = (srcLocLine loc, srcLocCol loc)
                    (tokLine, tokCol) = (srcSpanStartLine span, srcSpanStartCol span)
                    tokEnd = srcSpanEnd span

-- -----------------------------------------------------------------------------
-- Interactive evaluation

-- | Takes a 'ModuleName' and possibly a 'PackageId', and consults the
-- filesystem and package database to find the corresponding 'Module', 
-- using the algorithm that is used for an @import@ declaration.
findModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module
findModule mod_name maybe_pkg = withSession $ \hsc_env -> do
  let 
    dflags   = hsc_dflags hsc_env
    this_pkg = thisPackage dflags
  --
  case maybe_pkg of
    Just pkg | fsToPackageId pkg /= this_pkg && pkg /= fsLit "this" -> liftIO $ do
      res <- findImportedModule hsc_env mod_name maybe_pkg
      case res of
        Found _ m -> return m
        err       -> noModError dflags noSrcSpan mod_name err
    _otherwise -> do
      home <- lookupLoadedHomeModule mod_name
      case home of
        Just m  -> return m
        Nothing -> liftIO $ do
           res <- findImportedModule hsc_env mod_name maybe_pkg
           case res of
             Found loc m | modulePackageId m /= this_pkg -> return m
                         | otherwise -> modNotLoadedError m loc
             err -> noModError dflags noSrcSpan mod_name err

modNotLoadedError :: Module -> ModLocation -> IO a
modNotLoadedError m loc = ghcError $ CmdLineError $ showSDoc $
   text "module is not loaded:" <+> 
   quotes (ppr (moduleName m)) <+>
   parens (text (expectJust "modNotLoadedError" (ml_hs_file loc)))

-- | Like 'findModule', but differs slightly when the module refers to
-- a source file, and the file has not been loaded via 'load'.  In
-- this case, 'findModule' will throw an error (module not loaded),
-- but 'lookupModule' will check to see whether the module can also be
-- found in a package, and if so, that package 'Module' will be
-- returned.  If not, the usual module-not-found error will be thrown.
--
lookupModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module
lookupModule mod_name (Just pkg) = findModule mod_name (Just pkg)
lookupModule mod_name Nothing = withSession $ \hsc_env -> do
  home <- lookupLoadedHomeModule mod_name
  case home of
    Just m  -> return m
    Nothing -> liftIO $ do
      res <- findExposedPackageModule hsc_env mod_name Nothing
      case res of
        Found _ m -> return m
	err       -> noModError (hsc_dflags hsc_env) noSrcSpan mod_name err

lookupLoadedHomeModule  :: GhcMonad m => ModuleName -> m (Maybe Module)
lookupLoadedHomeModule mod_name = withSession $ \hsc_env ->
  case lookupUFM (hsc_HPT hsc_env) mod_name of
    Just mod_info      -> return (Just (mi_module (hm_iface mod_info)))
    _not_a_home_module -> return Nothing

#ifdef GHCI
getHistorySpan :: GhcMonad m => History -> m SrcSpan
getHistorySpan h = withSession $ \hsc_env ->
                          return$ InteractiveEval.getHistorySpan hsc_env h

obtainTermFromVal :: GhcMonad m => Int ->  Bool -> Type -> a -> m Term
obtainTermFromVal bound force ty a =
    withSession $ \hsc_env ->
      liftIO $ InteractiveEval.obtainTermFromVal hsc_env bound force ty a

obtainTermFromId :: GhcMonad m => Int -> Bool -> Id -> m Term
obtainTermFromId bound force id =
    withSession $ \hsc_env ->
      liftIO $ InteractiveEval.obtainTermFromId hsc_env bound force id

#endif

-- | Returns the 'TyThing' for a 'Name'.  The 'Name' may refer to any
-- entity known to GHC, including 'Name's defined using 'runStmt'.
lookupName :: GhcMonad m => Name -> m (Maybe TyThing)
lookupName name = withSession $ \hsc_env -> do
  mb_tything <- ioMsg $ tcRnLookupName hsc_env name
  return mb_tything
  -- XXX: calls panic in some circumstances;  is that ok?

