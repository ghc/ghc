--
-- (c) The University of Glasgow, 2004
--
-- The GHC API
--

module GHC (
	-- * Initialisation
	GhcSession,
	GhcMode(..),
	defaultErrorHandler,
	defaultCleanupHandler,
	init,
	newSession,

	-- * Flags and settings
	DynFlags(..),
	DynFlag(..),
	getSessionDynFlags,
	setSessionDynFlags,
	setMsgHandler,
  ) where

import HscTypes		( GhcMode(..) )
import qualified ErrUtils

-- -----------------------------------------------------------------------------
-- Initialisation

-- | abstract type representing a session with GHC.  A session
-- includes the currently loaded modules, and any bindings made using
-- interactive evaluation.
data Session = 
  Session {
	sess_hscenv :: IORef HscEnv  -- will include the InteractiveContext
  }

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
defaultCleanupHandler :: IO a -> IO a
defaultCleanupHandler inner = 
   -- make sure we clean up after ourselves
   later (do  forget_it <- readIORef v_Keep_tmp_files
	      unless forget_it $ do
	      verb <- dynFlag verbosity
	      cleanTempFiles verb
     ) $
	-- exceptions will be blocked while we clean the temporary files,
	-- so there shouldn't be any difficulty if we receive further
	-- signals.
   inner


-- | Initialises GHC.  This must be done /once/ only.  Takes the
-- command-line arguments.  All command-line arguments beginning with
-- '-' are interpreted as flags.  All others are returned.
--
init :: [String] -> IO [String]
init args = do
   -- catch ^C
   installSignalHandlers

   argv <- getArgs
   let (minusB_args, argv') = partition (prefixMatch "-B") argv
   top_dir <- initSysTools minusB_args

	-- Process all the other arguments, and get the source files
   non_static <- processArgs static_flags argv' []
   mode <- readIORef v_CmdLineMode

	-- Read all package.conf files (system, user, -package-conf)
   readPackageConfigs

	-- load explicit packages (those named with -package on the cmdline)
   loadExplicitPackages

	-- -O and --interactive are not a good combination
	-- ditto with any kind of way selection
   orig_ways <- readIORef v_Ways
   when (notNull orig_ways && isInteractive mode) $
      do throwDyn (UsageError 
                   "--interactive can't be used with -prof, -ticky, -unreg or -smp.")

	-- Find the build tag, and re-process the build-specific options.
	-- Also add in flags for unregisterised compilation, if 
	-- GhcUnregisterised=YES.
   way_opts <- findBuildTag
   let unreg_opts | cGhcUnregisterised == "YES" = unregFlags
		  | otherwise = []
   pkg_extra_opts <- getPackageExtraGhcOpts
   extra_non_static <- processArgs static_flags 
			   (unreg_opts ++ way_opts ++ pkg_extra_opts) []

	-- Give the static flags to hsc
   static_opts <- buildStaticHscOpts
   writeIORef v_Static_hsc_opts static_opts

   -- build the default DynFlags (these may be adjusted on a per
   -- module basis by OPTIONS pragmas and settings in the interpreter).

   stg_todo  <- buildStgToDo

   -- set the "global" HscLang.  The HscLang can be further adjusted on a module
   -- by module basis, using only the -fvia-C and -fasm flags.  If the global
   -- HscLang is not HscC or HscAsm, -fvia-C and -fasm have no effect.
   dyn_flags <- getDynFlags
   let lang = case mode of 
		 DoInteractive  -> HscInterpreted
		 DoEval _	-> HscInterpreted
		 _other		-> hscLang dyn_flags

   setDynFlags (dyn_flags{ stgToDo  = stg_todo,
                  	   hscLang  = lang,
			   -- leave out hscOutName for now
	                   hscOutName = panic "Main.main:hscOutName not set",
		  	   verbosity = case mode of
				 	 DoEval _ -> 0
				 	 _other   -> 1
			})

	-- The rest of the arguments are "dynamic"
	-- Leftover ones are presumably files
   fileish_args <- processArgs dynamic_flags (extra_non_static ++ non_static) []

	-- save the "initial DynFlags" away
   saveDynFlags

	-- and return the leftover args
   return fileish_args


-- | Starts a new session.  A session consists of a set of loaded
-- modules, a set of options (DynFlags), and an interactive context.
-- ToDo: GhcMode should say "keep typechecked code" and/or "keep renamed
-- code".
newSession :: GhcMode -> IO Session
newSession mode = do
  dflags <- getDynFlags
  env <- newHscEnv mode dflags
  ref <- newIORef env
  panic "do we need to set v_CmdLineMode? finder uses it."
  return (Session {sess_hscenv = ref})

-- -----------------------------------------------------------------------------
-- Flags & settings

-- | Grabs the DynFlags from the Session
getSessionDynFlags :: Session -> IO DynFlags
getSessionDynFlags sess = do
  env <- readIORef (sess_hscenv sess)
  return (hsc_dflags env)

-- | Updates the DynFlags in a Session
updateSessionDynFlags :: Session -> DynFlags -> IO ()
updateSessionDynFlags sess dflags = do
  env <- readIORef (sess_hscenv sess)
  writeIORef (sess_hscenv sess) env{hsc_dflags=dflags}

-- | Messages during compilation (eg. warnings and progress messages)
-- are reported using this callback.  By default, these messages are
-- printed to stderr.
setMsgHandler :: (String -> IO ()) -> IO ()
setMsgHandler = ErrUtils.setMsgHandler

-- -----------------------------------------------------------------------------
-- Loading a program

-- | A compilation target.
data Target = Target TargetId (Maybe StringBuffer)
	-- A target may be supplied with the actual text of the
	-- module.  If so, use this instead of the file contents (this
	-- is for use in an IDE where the file hasn't been saved by
	-- the user yet).

data TargetId
  = TargetModule String		-- A module name: search for the file
  | TargetFile   FilePath 	-- A filename: parse it to find the module name.

-- ToDo: think about relative vs. absolute file paths. And what
-- happens when the current directory changes.

-- | Sets the targets for this session.  Each target may be a module name
-- or a filename.  The targets correspond to the set of root modules for
-- the program/library.  Unloading the current program is achieved by
-- setting the current set of targets to be empty.
setTargets :: Session -> [Target] -> IO ()

-- | returns the current set of targets
--getTargets :: Session -> IO [Target]

-- Add another target, or update an existing target with new content.
addTarget :: Session -> Target -> IO Module

-- Remove a target
removeTarget :: Session -> Module -> IO ()

-- Try to load the program.  If a Module is supplied, then just
-- attempt to load up to this target.  If no Module is supplied,
-- then try to load all targets.
load :: Session -> Maybe Module -> IO LoadResult

-- | The result of load.
data LoadResult
  = LoadOk	Errors	-- ^ all specified targets were loaded successfully.
  | LoadFailed  Errors	-- ^ not all modules were loaded.

type Errors = [ErrMsg]

data ErrMsg = ErrMsg { 
	errMsgSeverity  :: Severity,  -- warning, error, etc.
	errMsgSpans     :: [SrcSpan],
	errMsgShortDoc  :: Doc,
	errMsgExtraInfo :: Doc
	}

-- -----------------------------------------------------------------------------
-- inspecting the session

-- | Get the set of modules in the current session
getLoadedModules :: Session -> IO [Module]

-- | Get the module dependency graph
getModuleGraph :: Session -> IO (DiGraph ModSummary)

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

-- -----------------------------------------------------------------------------
-- Interactive evaluation

-- | Set the interactive evaluation context.
--
-- Setting the context doesn't throw away any bindings; the bindings
-- we've built up in the InteractiveContext simply move to the new
-- module.  They always shadow anything in scope in the current context.
setContext :: Session
	   -> [Module]	-- entire top level scope of these modules
	   -> [Module]	-- exports only of these modules
	   -> IO ()

-- | Get the interactive evaluation context.
getContext :: Session -> IO ([Module],[Module])

-- | Looks up an identifier in the current interactive context (for :info)
lookupThing :: Session -> String -> IO [TyThing]

-- | Looks up a Name in the current interactive context (for inspecting
-- the result names from 'runStmt').
lookupName :: Session -> Name -> IO TyThing

-- | Get the type of an expression
exprType :: Session -> String -> IO (Either Errors Type)

-- | Get the kind of a  type
typeKind  :: Session -> String -> IO (Either Errors Kind)

data RunResult
  = RunOk [Name] 		-- ^ names bound by this evaluation
  | RunFailed Errors 		-- ^ statement failed compilation
  | RunException Exception	-- ^ statement raised an exception

-- | Run a statement in the current interactive context.  Statemenet
-- may bind multple values.
runStmt :: Session -> String -> IO RunResult

-- | Return a list of the transient bindings in the current interactive
-- context (i.e. those bindings made via runStmt).
getInteractiveBindings :: Session -> IO [TyThing]
