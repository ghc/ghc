%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%
\begin{code}
-- | Main API for compiling plain Haskell source code.
--
-- This module implements compilation of a Haskell source.  It is
-- /not/ concerned with preprocessing of source files; this is handled
-- in "DriverPipeline".
--
-- There are various entry points depending on what mode we're in:
-- "batch" mode (@--make@), "one-shot" mode (@-c@, @-S@ etc.), and
-- "interactive" mode (GHCi).  There are also entry points for
-- individual passes: parsing, typechecking/renaming, desugaring, and
-- simplification.
--
-- All the functions here take an 'HscEnv' as a parameter, but none of
-- them return a new one: 'HscEnv' is treated as an immutable value
-- from here on in (although it has mutable components, for the
-- caches).
--
-- Warning messages are dealt with consistently throughout this API:
-- during compilation warnings are collected, and before any function
-- in @HscMain@ returns, the warnings are either printed, or turned
-- into a real compialtion error if the @-Werror@ flag is enabled.
--
module HscMain
    ( 
    -- * Making an HscEnv
      newHscEnv

    -- * Compiling complete source files
    , Compiler
    , HscStatus' (..)
    , InteractiveStatus, HscStatus
    , hscCompileOneShot
    , hscCompileBatch
    , hscCompileNothing
    , hscCompileInteractive
    , hscCompileCmmFile
    , hscCompileCore

    -- * Running passes separately
    , hscParse
    , hscTypecheckRename
    , hscDesugar
    , makeSimpleIface
    , makeSimpleDetails
    , hscSimplify -- ToDo, shouldn't really export this

    -- ** Backends
    , hscOneShotBackendOnly
    , hscBatchBackendOnly
    , hscNothingBackendOnly
    , hscInteractiveBackendOnly

    -- * Support for interactive evaluation
    , hscParseIdentifier
    , hscTcRcLookupName
    , hscTcRnGetInfo
#ifdef GHCI
    , hscRnImportDecls
    , hscGetModuleExports
    , hscTcRnLookupRdrName
    , hscStmt, hscStmtWithLocation
    , hscTcExpr, hscImport, hscKcType
    , hscCompileCoreExpr
#endif

    ) where

#ifdef GHCI
import ByteCodeGen	( byteCodeGen, coreExprToBCOs )
import Linker		( HValue, linkExpr )
import CoreTidy		( tidyExpr )
import Type		( Type )
import TcType           ( tyVarsOfTypes )
import PrelNames	( iNTERACTIVE )
import {- Kind parts of -} Type		( Kind )
import Id      	     	( idType )
import CoreLint		( lintUnfolding )
import DsMeta		( templateHaskellNames )
import VarSet
import VarEnv		( emptyTidyEnv )
import Panic
#endif

import Id		( Id )
import Module
import Packages
import RdrName
import HsSyn
import CoreSyn
import StringBuffer
import Parser
import Lexer hiding (getDynFlags)
import SrcLoc
import TcRnDriver
import TcIface		( typecheckIface )
import TcRnMonad
import IfaceEnv		( initNameCache )
import LoadIface	( ifaceStats, initExternalPackageState )
import PrelInfo		( wiredInThings, basicKnownKeyNames )
import MkIface
import Desugar
import SimplCore
import TidyPgm
import CorePrep
import CoreToStg	( coreToStg )
import qualified StgCmm	( codeGen )
import StgSyn
import CostCentre
import ProfInit
import TyCon            ( TyCon, isDataTyCon )
import Name		( Name, NamedThing(..) )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
import OldCmm           ( Cmm )
import PprCmm		( pprCmms )
import CmmParse		( parseCmmFile )
import CmmBuildInfoTables
import CmmPipeline
import CmmInfo
import OptimizationFuel ( initOptFuelState )
import CmmCvt
import CmmContFlowOpt   ( runCmmContFlowOpts )
import CodeOutput
import NameEnv          ( emptyNameEnv )
import NameSet          ( emptyNameSet )
import InstEnv
import FamInstEnv       ( emptyFamInstEnv )
import Fingerprint      ( Fingerprint )

import DynFlags
import ErrUtils
import UniqSupply	( mkSplitUniqSupply )

import Outputable
import HscStats		( ppSourceStats )
import HscTypes
import MkExternalCore	( emitExternalCore )
import FastString
import UniqFM		( emptyUFM )
import UniqSupply       ( initUs_ )
import Bag
import Exception

import Control.Monad
import Data.Maybe       ( catMaybes )
import Data.IORef
\end{code}
#include "HsVersions.h"


%************************************************************************
%*									*
		Initialisation
%*									*
%************************************************************************

\begin{code}
newHscEnv :: DynFlags -> IO HscEnv
newHscEnv dflags
  = do 	{ eps_var <- newIORef initExternalPackageState
        ; us      <- mkSplitUniqSupply 'r'
        ; nc_var  <- newIORef (initNameCache us knownKeyNames)
        ; fc_var  <- newIORef emptyUFM
	; mlc_var <- newIORef emptyModuleEnv
        ; optFuel <- initOptFuelState
	; return (HscEnv { hsc_dflags = dflags,
			   hsc_targets = [],
			   hsc_mod_graph = [],
			   hsc_IC      = emptyInteractiveContext,
			   hsc_HPT     = emptyHomePackageTable,
			   hsc_EPS     = eps_var,
			   hsc_NC      = nc_var,
			   hsc_FC      = fc_var,
			   hsc_MLC     = mlc_var,
			   hsc_OptFuel = optFuel,
                           hsc_type_env_var = Nothing } ) }


knownKeyNames :: [Name]      -- Put here to avoid loops involving DsMeta,
                             -- where templateHaskellNames are defined
knownKeyNames
  = map getName wiredInThings 
    ++ basicKnownKeyNames
#ifdef GHCI
    ++ templateHaskellNames
#endif

-- -----------------------------------------------------------------------------
-- The Hsc monad: collecting warnings

newtype Hsc a = Hsc (HscEnv -> WarningMessages -> IO (a, WarningMessages))

instance Monad Hsc where
  return a = Hsc $ \_ w -> return (a, w)
  Hsc m >>= k = Hsc $ \e w -> do (a, w1) <- m e w
                                 case k a of
                                    Hsc k' -> k' e w1

instance MonadIO Hsc where
  liftIO io = Hsc $ \_ w -> do a <- io; return (a, w)

runHsc :: HscEnv -> Hsc a -> IO a
runHsc hsc_env (Hsc hsc) = do
  (a, w) <- hsc hsc_env emptyBag
  printOrThrowWarnings (hsc_dflags hsc_env) w
  return a

getWarnings :: Hsc WarningMessages
getWarnings = Hsc $ \_ w -> return (w, w)

clearWarnings :: Hsc ()
clearWarnings = Hsc $ \_ _w -> return ((), emptyBag)

logWarnings :: WarningMessages -> Hsc ()
logWarnings w = Hsc $ \_ w0 -> return ((), w0 `unionBags` w)

getHscEnv :: Hsc HscEnv
getHscEnv = Hsc $ \e w -> return (e, w)

getDynFlags :: Hsc DynFlags
getDynFlags = Hsc $ \e w -> return (hsc_dflags e, w)

handleWarnings :: Hsc ()
handleWarnings = do
  dflags <- getDynFlags
  w <- getWarnings
  liftIO $ printOrThrowWarnings dflags w
  clearWarnings

-- | log warning in the monad, and if there are errors then
-- throw a SourceError exception.
logWarningsReportErrors :: Messages -> Hsc ()
logWarningsReportErrors (warns,errs) = do
  logWarnings warns
  when (not (isEmptyBag errs)) $ do
    liftIO $ throwIO $ mkSrcErr errs

-- | Deal with errors and warnings returned by a compilation step
--
-- In order to reduce dependencies to other parts of the compiler, functions
-- outside the "main" parts of GHC return warnings and errors as a parameter
-- and signal success via by wrapping the result in a 'Maybe' type.  This
-- function logs the returned warnings and propagates errors as exceptions
-- (of type 'SourceError').
--
-- This function assumes the following invariants:
--
--  1. If the second result indicates success (is of the form 'Just x'),
--     there must be no error messages in the first result.
--
--  2. If there are no error messages, but the second result indicates failure
--     there should be warnings in the first result.  That is, if the action
--     failed, it must have been due to the warnings (i.e., @-Werror@).
ioMsgMaybe :: IO (Messages, Maybe a) -> Hsc a
ioMsgMaybe ioA = do
  ((warns,errs), mb_r) <- liftIO $ ioA
  logWarnings warns
  case mb_r of
    Nothing -> liftIO $ throwIO (mkSrcErr errs)
    Just r  -> ASSERT( isEmptyBag errs ) return r

-- | like ioMsgMaybe, except that we ignore error messages and return
-- 'Nothing' instead.
ioMsgMaybe' :: IO (Messages, Maybe a) -> Hsc (Maybe a)
ioMsgMaybe' ioA = do
  ((warns,_errs), mb_r) <- liftIO $ ioA
  logWarnings warns
  return mb_r

-- -----------------------------------------------------------------------------
-- | Lookup things in the compiler's environment

#ifdef GHCI
hscTcRnLookupRdrName :: HscEnv -> RdrName -> IO [Name]
hscTcRnLookupRdrName hsc_env rdr_name = 
  runHsc hsc_env $ ioMsgMaybe $ tcRnLookupRdrName hsc_env rdr_name
#endif

hscTcRcLookupName :: HscEnv -> Name -> IO (Maybe TyThing)
hscTcRcLookupName hsc_env name = 
  runHsc hsc_env $ ioMsgMaybe' $ tcRnLookupName hsc_env name
    -- ignore errors: the only error we're likely to get is
    -- "name not found", and the Maybe in the return type
    -- is used to indicate that.

hscTcRnGetInfo :: HscEnv -> Name -> IO (Maybe (TyThing, Fixity, [Instance]))
hscTcRnGetInfo hsc_env name =
  runHsc hsc_env $ ioMsgMaybe' $ tcRnGetInfo hsc_env name

#ifdef GHCI
hscGetModuleExports :: HscEnv -> Module -> IO (Maybe [AvailInfo])
hscGetModuleExports hsc_env mdl =
  runHsc hsc_env $ ioMsgMaybe' $ getModuleExports hsc_env mdl

-- -----------------------------------------------------------------------------
-- | Rename some import declarations

hscRnImportDecls
        :: HscEnv
        -> Module
        -> [LImportDecl RdrName]
        -> IO GlobalRdrEnv

-- It is important that we use tcRnImports instead of calling rnImports directly
-- because tcRnImports will force-load any orphan modules necessary, making extra
-- instances/family instances visible (GHC #4832)
hscRnImportDecls hsc_env this_mod import_decls
  = runHsc hsc_env $ ioMsgMaybe $ 
    initTc hsc_env HsSrcFile False this_mod $
    fmap tcg_rdr_env $ 
    tcRnImports hsc_env this_mod loc import_decls
  where
    loc = mkGeneralSrcSpan (mkFastString "In a call to hscRnImportDecls")
#endif

-- -----------------------------------------------------------------------------
-- | parse a file, returning the abstract syntax

hscParse :: HscEnv -> ModSummary -> IO (Located (HsModule RdrName))
hscParse hsc_env mod_summary = runHsc hsc_env $ hscParse' mod_summary

-- internal version, that doesn't fail due to -Werror
hscParse' :: ModSummary -> Hsc (Located (HsModule RdrName))
hscParse' mod_summary
 = do
   dflags <- getDynFlags
   let 
       src_filename  = ms_hspp_file mod_summary
       maybe_src_buf = ms_hspp_buf  mod_summary

   --------------------------  Parser  ----------------
   liftIO $ showPass dflags "Parser"
   {-# SCC "Parser" #-} do

	-- sometimes we already have the buffer in memory, perhaps
	-- because we needed to parse the imports out of it, or get the
	-- module name.
   buf <- case maybe_src_buf of
            Just b  -> return b
            Nothing -> liftIO $ hGetStringBuffer src_filename

   let loc  = mkRealSrcLoc (mkFastString src_filename) 1 1

   case unP parseModule (mkPState dflags buf loc) of
     PFailed span err ->
         liftIO $ throwOneError (mkPlainErrMsg span err)

     POk pst rdr_module -> do
         logWarningsReportErrors (getMessages pst)
         liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" $
                                ppr rdr_module
         liftIO $ dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics" $
                                ppSourceStats False rdr_module
         return rdr_module
          -- ToDo: free the string buffer later.

-- XXX: should this really be a Maybe X?  Check under which circumstances this
-- can become a Nothing and decide whether this should instead throw an
-- exception/signal an error.
type RenamedStuff = 
        (Maybe (HsGroup Name, [LImportDecl Name], Maybe [LIE Name],
                Maybe LHsDocString))

-- | Rename and typecheck a module, additionally returning the renamed syntax
hscTypecheckRename :: HscEnv -> ModSummary -> Located (HsModule RdrName)
                   -> IO (TcGblEnv, RenamedStuff)
hscTypecheckRename hsc_env mod_summary rdr_module
  = runHsc hsc_env $ do
      tc_result
          <- {-# SCC "Typecheck-Rename" #-}
              ioMsgMaybe $ 
                  tcRnModule hsc_env (ms_hsc_src mod_summary) True rdr_module

      let -- This 'do' is in the Maybe monad!
          rn_info = do decl <- tcg_rn_decls tc_result
                       let imports = tcg_rn_imports tc_result
                           exports = tcg_rn_exports tc_result
                           doc_hdr  = tcg_doc_hdr tc_result
                       return (decl,imports,exports,doc_hdr)

      return (tc_result, rn_info)

-- | Convert a typechecked module to Core
hscDesugar :: HscEnv -> ModSummary -> TcGblEnv -> IO ModGuts
hscDesugar hsc_env mod_summary tc_result
  = runHsc hsc_env $ hscDesugar' mod_summary tc_result

hscDesugar' :: ModSummary -> TcGblEnv -> Hsc ModGuts
hscDesugar' mod_summary tc_result
 = do
      hsc_env <- getHscEnv
      r <- ioMsgMaybe $ 
             deSugar hsc_env (ms_location mod_summary) tc_result

      handleWarnings
                -- always check -Werror after desugaring, this is 
                -- the last opportunity for warnings to arise before
                -- the backend.
      return r

-- | Make a 'ModIface' from the results of typechecking.  Used when
-- not optimising, and the interface doesn't need to contain any
-- unfoldings or other cross-module optimisation info.
-- ToDo: the old interface is only needed to get the version numbers,
-- we should use fingerprint versions instead.
makeSimpleIface :: HscEnv -> 
                   Maybe ModIface -> TcGblEnv -> ModDetails
                -> IO (ModIface,Bool)
makeSimpleIface hsc_env maybe_old_iface tc_result details
  = runHsc hsc_env $
     ioMsgMaybe $ 
       mkIfaceTc hsc_env (fmap mi_iface_hash maybe_old_iface) details tc_result

-- | Make a 'ModDetails' from the results of typechecking.  Used when
-- typechecking only, as opposed to full compilation.
makeSimpleDetails :: HscEnv -> TcGblEnv -> IO ModDetails
makeSimpleDetails hsc_env tc_result = mkBootModDetailsTc hsc_env tc_result
\end{code}

%************************************************************************
%*									*
		The main compiler pipeline
%*									*
%************************************************************************

                   --------------------------------
                        The compilation proper
                   --------------------------------


It's the task of the compilation proper to compile Haskell, hs-boot and
core files to either byte-code, hard-code (C, asm, LLVM, ect) or to
nothing at all (the module is still parsed and type-checked. This
feature is mostly used by IDE's and the likes).
Compilation can happen in either 'one-shot', 'batch', 'nothing',
or 'interactive' mode. 'One-shot' mode targets hard-code, 'batch' mode
targets hard-code, 'nothing' mode targets nothing and 'interactive' mode
targets byte-code.
The modes are kept separate because of their different types and meanings.
In 'one-shot' mode, we're only compiling a single file and can therefore
discard the new ModIface and ModDetails. This is also the reason it only
targets hard-code; compiling to byte-code or nothing doesn't make sense
when we discard the result.
'Batch' mode is like 'one-shot' except that we keep the resulting ModIface
and ModDetails. 'Batch' mode doesn't target byte-code since that require
us to return the newly compiled byte-code.
'Nothing' mode has exactly the same type as 'batch' mode but they're still
kept separate. This is because compiling to nothing is fairly special: We
don't output any interface files, we don't run the simplifier and we don't
generate any code.
'Interactive' mode is similar to 'batch' mode except that we return the
compiled byte-code together with the ModIface and ModDetails.

Trying to compile a hs-boot file to byte-code will result in a run-time
error. This is the only thing that isn't caught by the type-system.

\begin{code}

-- Status of a compilation to hard-code or nothing.
data HscStatus' a
    = HscNoRecomp
    | HscRecomp
       (Maybe FilePath)
            -- Has stub files.  This is a hack. We can't compile C files here
            -- since it's done in DriverPipeline. For now we just return True
            -- if we want the caller to compile them for us.
       a

-- This is a bit ugly.  Since we use a typeclass below and would like to avoid
-- functional dependencies, we have to parameterise the typeclass over the
-- result type.  Therefore we need to artificially distinguish some types.  We
-- do this by adding type tags which will simply be ignored by the caller.
type HscStatus         = HscStatus' ()
type InteractiveStatus = HscStatus' (Maybe (CompiledByteCode, ModBreaks))
    -- INVARIANT: result is @Nothing@ <=> input was a boot file

type OneShotResult     = HscStatus
type BatchResult       = (HscStatus, ModIface, ModDetails)
type NothingResult     = (HscStatus, ModIface, ModDetails)
type InteractiveResult = (InteractiveStatus, ModIface, ModDetails)

-- FIXME: The old interface and module index are only using in 'batch' and
--        'interactive' mode. They should be removed from 'oneshot' mode.
type Compiler result =  HscEnv
                     -> ModSummary
                     -> SourceModified
                     -> Maybe ModIface      -- Old interface, if available
                     -> Maybe (Int,Int)     -- Just (i,n) <=> module i of n (for msgs)
                     -> IO result

data HsCompiler a
  = HsCompiler {
    -- | Called when no recompilation is necessary.
    hscNoRecomp :: ModIface
                -> Hsc a,

    -- | Called to recompile the module.
    hscRecompile :: ModSummary -> Maybe Fingerprint
                 -> Hsc a,

    hscBackend :: TcGblEnv -> ModSummary -> Maybe Fingerprint
               -> Hsc a,

    -- | Code generation for Boot modules.
    hscGenBootOutput :: TcGblEnv -> ModSummary -> Maybe Fingerprint
                     -> Hsc a,

    -- | Code generation for normal modules.
    hscGenOutput :: ModGuts  -> ModSummary -> Maybe Fingerprint
                 -> Hsc a
  }

genericHscCompile :: HsCompiler a
                  -> (HscEnv -> Maybe (Int,Int) -> RecompReason -> ModSummary -> IO ())
                  -> HscEnv -> ModSummary -> SourceModified
                  -> Maybe ModIface -> Maybe (Int, Int)
                  -> IO a
genericHscCompile compiler hscMessage hsc_env
                  mod_summary source_modified
                  mb_old_iface0 mb_mod_index
 = do
     (recomp_reqd, mb_checked_iface)
         <- {-# SCC "checkOldIface" #-}
            checkOldIface hsc_env mod_summary 
                          source_modified mb_old_iface0
     -- save the interface that comes back from checkOldIface.
     -- In one-shot mode we don't have the old iface until this
     -- point, when checkOldIface reads it from the disk.
     let mb_old_hash = fmap mi_iface_hash mb_checked_iface

     let
       skip iface = do
         hscMessage hsc_env mb_mod_index RecompNotRequired mod_summary
         runHsc hsc_env $ hscNoRecomp compiler iface

       compile reason = do
         hscMessage hsc_env mb_mod_index reason mod_summary
         runHsc hsc_env $ hscRecompile compiler mod_summary mb_old_hash

       stable = case source_modified of
                  SourceUnmodifiedAndStable -> True
                  _ -> False

        -- If the module used TH splices when it was last compiled,
        -- then the recompilation check is not accurate enough (#481)
        -- and we must ignore it.  However, if the module is stable
        -- (none of the modules it depends on, directly or indirectly,
        -- changed), then we *can* skip recompilation.  This is why
        -- the SourceModified type contains SourceUnmodifiedAndStable,
        -- and it's pretty important: otherwise ghc --make would
        -- always recompile TH modules, even if nothing at all has
        -- changed.  Stability is just the same check that make is
        -- doing for us in one-shot mode.

     case mb_checked_iface of
       Just iface | not recomp_reqd ->
           if mi_used_th iface && not stable
               then compile RecompForcedByTH
               else skip iface
       _otherwise ->
           compile RecompRequired


hscCheckRecompBackend :: HsCompiler a -> TcGblEnv -> Compiler a
hscCheckRecompBackend compiler tc_result 
                   hsc_env mod_summary source_modified mb_old_iface _m_of_n
  = do
     (recomp_reqd, mb_checked_iface)
         <- {-# SCC "checkOldIface" #-}
            checkOldIface hsc_env mod_summary
                          source_modified mb_old_iface

     let mb_old_hash = fmap mi_iface_hash mb_checked_iface
     case mb_checked_iface of
       Just iface | not recomp_reqd
           -> runHsc hsc_env $ 
                 hscNoRecomp compiler
                             iface{ mi_globals = Just (tcg_rdr_env tc_result) }
       _otherwise
           -> runHsc hsc_env $
                 hscBackend compiler tc_result mod_summary mb_old_hash

genericHscRecompile :: HsCompiler a
                    -> ModSummary -> Maybe Fingerprint
                    -> Hsc a
genericHscRecompile compiler mod_summary mb_old_hash
  | ExtCoreFile <- ms_hsc_src mod_summary =
      panic "GHC does not currently support reading External Core files"
  | otherwise = do
      tc_result <- hscFileFrontEnd mod_summary
      hscBackend compiler tc_result mod_summary mb_old_hash

genericHscBackend :: HsCompiler a
                  -> TcGblEnv -> ModSummary -> Maybe Fingerprint
                  -> Hsc a
genericHscBackend compiler tc_result mod_summary mb_old_hash
  | HsBootFile <- ms_hsc_src mod_summary =
      hscGenBootOutput compiler tc_result mod_summary mb_old_hash
  | otherwise = do
      guts <- hscDesugar' mod_summary tc_result
      hscGenOutput compiler guts mod_summary mb_old_hash

compilerBackend :: HsCompiler a -> TcGblEnv -> Compiler a
compilerBackend comp tcg hsc_env ms' _ _mb_old_iface _ =
  runHsc hsc_env $
    hscBackend comp tcg ms' Nothing

--------------------------------------------------------------
-- Compilers
--------------------------------------------------------------

hscOneShotCompiler :: HsCompiler OneShotResult
hscOneShotCompiler =
  HsCompiler {

    hscNoRecomp = \_old_iface -> do
      hsc_env <- getHscEnv
      liftIO $ dumpIfaceStats hsc_env
      return HscNoRecomp

  , hscRecompile = genericHscRecompile hscOneShotCompiler

  , hscBackend = \ tc_result mod_summary mb_old_hash -> do
       dflags <- getDynFlags
       case hscTarget dflags of
         HscNothing -> return (HscRecomp Nothing ())
         _otherw    -> genericHscBackend hscOneShotCompiler
                                         tc_result mod_summary mb_old_hash

  , hscGenBootOutput = \tc_result mod_summary mb_old_iface -> do
       (iface, changed, _) <- hscSimpleIface tc_result mb_old_iface
       hscWriteIface iface changed mod_summary
       return (HscRecomp Nothing ())

  , hscGenOutput = \guts0 mod_summary mb_old_iface -> do
       guts <- hscSimplify' guts0
       (iface, changed, _details, cgguts) <- hscNormalIface guts mb_old_iface
       hscWriteIface iface changed mod_summary
       hasStub <- hscGenHardCode cgguts mod_summary
       return (HscRecomp hasStub ())
  }

-- Compile Haskell, boot and extCore in OneShot mode.
hscCompileOneShot :: Compiler OneShotResult
hscCompileOneShot hsc_env mod_summary src_changed mb_old_iface mb_i_of_n
  = do
       -- One-shot mode needs a knot-tying mutable variable for interface
       -- files.  See TcRnTypes.TcGblEnv.tcg_type_env_var.
      type_env_var <- newIORef emptyNameEnv
      let
         mod = ms_mod mod_summary
         hsc_env' = hsc_env{ hsc_type_env_var = Just (mod, type_env_var) }
      ---
      genericHscCompile hscOneShotCompiler
                        oneShotMsg hsc_env' mod_summary src_changed
                        mb_old_iface mb_i_of_n


hscOneShotBackendOnly :: TcGblEnv -> Compiler OneShotResult
hscOneShotBackendOnly = compilerBackend hscOneShotCompiler

--------------------------------------------------------------

hscBatchCompiler :: HsCompiler BatchResult
hscBatchCompiler =
  HsCompiler {

    hscNoRecomp = \iface -> do
       details <- genModDetails iface
       return (HscNoRecomp, iface, details)

  , hscRecompile = genericHscRecompile hscBatchCompiler

  , hscBackend = genericHscBackend hscBatchCompiler

  , hscGenBootOutput = \tc_result mod_summary mb_old_iface -> do
       (iface, changed, details) <- hscSimpleIface tc_result mb_old_iface
       hscWriteIface iface changed mod_summary
       return (HscRecomp Nothing (), iface, details)

  , hscGenOutput = \guts0 mod_summary mb_old_iface -> do
       guts <- hscSimplify' guts0
       (iface, changed, details, cgguts) <- hscNormalIface guts mb_old_iface
       hscWriteIface iface changed mod_summary
       hasStub <- hscGenHardCode cgguts mod_summary
       return (HscRecomp hasStub (), iface, details)
  }

-- Compile Haskell, boot and extCore in batch mode.
hscCompileBatch :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileBatch = genericHscCompile hscBatchCompiler batchMsg

hscBatchBackendOnly :: TcGblEnv -> Compiler BatchResult
hscBatchBackendOnly = hscCheckRecompBackend hscBatchCompiler

--------------------------------------------------------------

hscInteractiveCompiler :: HsCompiler InteractiveResult
hscInteractiveCompiler =
  HsCompiler {
    hscNoRecomp = \iface -> do
       details <- genModDetails iface
       return (HscNoRecomp, iface, details)

  , hscRecompile = genericHscRecompile hscInteractiveCompiler

  , hscBackend = genericHscBackend hscInteractiveCompiler

  , hscGenBootOutput = \tc_result _mod_summary mb_old_iface -> do
       (iface, _changed, details) <- hscSimpleIface tc_result mb_old_iface
       return (HscRecomp Nothing Nothing, iface, details)

  , hscGenOutput = \guts0 mod_summary mb_old_iface -> do
       guts <- hscSimplify' guts0
       (iface, _changed, details, cgguts) <- hscNormalIface guts mb_old_iface
       hscInteractive (iface, details, cgguts) mod_summary
  }

-- Compile Haskell, extCore to bytecode.
hscCompileInteractive :: Compiler (InteractiveStatus, ModIface, ModDetails)
hscCompileInteractive = genericHscCompile hscInteractiveCompiler batchMsg

hscInteractiveBackendOnly :: TcGblEnv -> Compiler InteractiveResult
hscInteractiveBackendOnly = compilerBackend hscInteractiveCompiler

--------------------------------------------------------------

hscNothingCompiler :: HsCompiler NothingResult
hscNothingCompiler =
  HsCompiler {
    hscNoRecomp = \iface -> do
       details <- genModDetails iface
       return (HscNoRecomp, iface, details)

  , hscRecompile = genericHscRecompile hscNothingCompiler

  , hscBackend = \tc_result _mod_summary mb_old_iface -> do
       handleWarnings
       (iface, _changed, details) <- hscSimpleIface tc_result mb_old_iface
       return (HscRecomp Nothing (), iface, details)

  , hscGenBootOutput = \_ _ _ ->
        panic "hscCompileNothing: hscGenBootOutput should not be called"

  , hscGenOutput = \_ _ _ ->
        panic "hscCompileNothing: hscGenOutput should not be called"
  }

-- Type-check Haskell and .hs-boot only (no external core)
hscCompileNothing :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileNothing = genericHscCompile hscNothingCompiler batchMsg

hscNothingBackendOnly :: TcGblEnv -> Compiler NothingResult
hscNothingBackendOnly = compilerBackend hscNothingCompiler

--------------------------------------------------------------
-- NoRecomp handlers
--------------------------------------------------------------

genModDetails :: ModIface -> Hsc ModDetails
genModDetails old_iface
  = do
      hsc_env <- getHscEnv
      new_details <- {-# SCC "tcRnIface" #-}
                     liftIO $ initIfaceCheck hsc_env $
                              typecheckIface old_iface
      liftIO $ dumpIfaceStats hsc_env
      return new_details

--------------------------------------------------------------
-- Progress displayers.
--------------------------------------------------------------

data RecompReason = RecompNotRequired | RecompRequired | RecompForcedByTH
  deriving Eq

oneShotMsg :: HscEnv -> Maybe (Int,Int) -> RecompReason -> ModSummary -> IO ()
oneShotMsg hsc_env _mb_mod_index recomp _mod_summary =
  case recomp of
    RecompNotRequired ->
            compilationProgressMsg (hsc_dflags hsc_env) $
                   "compilation IS NOT required"
    _other ->
            return ()

batchMsg :: HscEnv -> Maybe (Int,Int) -> RecompReason -> ModSummary -> IO ()
batchMsg hsc_env mb_mod_index recomp mod_summary
 = case recomp of
     RecompRequired -> showMsg "Compiling "
     RecompNotRequired
       | verbosity (hsc_dflags hsc_env) >= 2 -> showMsg "Skipping  "
       | otherwise -> return ()
     RecompForcedByTH -> showMsg "Compiling [TH] "
   where
     showMsg msg =
        compilationProgressMsg (hsc_dflags hsc_env) $
         (showModuleIndex mb_mod_index ++
         msg ++ showModMsg (hscTarget (hsc_dflags hsc_env)) (recomp == RecompRequired) mod_summary)

--------------------------------------------------------------
-- FrontEnds
--------------------------------------------------------------

hscFileFrontEnd :: ModSummary -> Hsc TcGblEnv
hscFileFrontEnd mod_summary = do
    rdr_module <- hscParse' mod_summary
    hsc_env <- getHscEnv
    tcg_env <-
        {-# SCC "Typecheck-Rename" #-}
        ioMsgMaybe $
            tcRnModule hsc_env (ms_hsc_src mod_summary) False rdr_module
    dflags <- getDynFlags
    -- XXX: See Note [Safe Haskell API]
    if safeHaskellOn dflags
        then do
            tcg_env1 <- checkSafeImports dflags hsc_env tcg_env
            if safeLanguageOn dflags
                then do
                    -- we also nuke user written RULES.
                    logWarnings $ warns (tcg_rules tcg_env1)
                    return tcg_env1 { tcg_rules = [] }
                else do
                    -- Wipe out trust required packages if the module isn't
                    -- trusted. Not doing this doesn't cause any problems
                    -- but means the hi file will say some pkgs should be
                    -- trusted when they don't need to be (since its an
                    -- untrusted module) and we don't force them to be.
                    let imps  = tcg_imports tcg_env1
                        imps' = imps { imp_trust_pkgs = [] }
                    return tcg_env1 { tcg_imports = imps' }

        else
            return tcg_env

    where
        warns rules = listToBag $ map warnRules rules
        warnRules (L loc (HsRule n _ _ _ _ _ _)) =
            mkPlainWarnMsg loc $
                text "Rule \"" <> ftext n <> text "\" ignored" $+$
                text "User defined rules are disabled under Safe Haskell"

--------------------------------------------------------------
-- Safe Haskell
--------------------------------------------------------------

-- Note [Safe Haskell API]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- XXX: We only call this in hscFileFrontend and don't expose
-- it to the GHC API. External users of GHC can't properly use
-- the GHC API and Safe Haskell.


-- Note [Safe Haskell Trust Check]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Safe Haskell checks that an import is trusted according to the following
-- rules for an import of module M that resides in Package P:
--
--   * If M is recorded as Safe and all its trust dependencies are OK
--     then M is considered safe.
--   * If M is recorded as Trustworthy and P is considered trusted and
--     all M's trust dependencies are OK then M is considered safe.
--
-- By trust dependencies we mean that the check is transitive. So if
-- a module M that is Safe relies on a module N that is trustworthy,
-- importing module M will first check (according to the second case)
-- that N is trusted before checking M is trusted.
--
-- This is a minimal description, so please refer to the user guide
-- for more details. The user guide is also considered the authoritative
-- source in this matter, not the comments or code.


-- | Validate that safe imported modules are actually safe.
-- For modules in the HomePackage (the package the module we
-- are compiling in resides) this just involves checking its
-- trust type is 'Safe' or 'Trustworthy'. For modules that
-- reside in another package we also must check that the
-- external pacakge is trusted. See the Note [Safe Haskell
-- Trust Check] above for more information.
--
-- The code for this is quite tricky as the whole algorithm
-- is done in a few distinct phases in different parts of the
-- code base. See RnNames.rnImportDecl for where package trust
-- dependencies for a module are collected and unioned.
-- Specifically see the Note [RnNames . Tracking Trust Transitively]
-- and the Note [RnNames . Trust Own Package].
checkSafeImports :: DynFlags -> HscEnv -> TcGblEnv -> Hsc TcGblEnv
checkSafeImports dflags hsc_env tcg_env
    = do
        imps <- mapM condense imports'
        pkgs <- mapM checkSafe imps
        checkPkgTrust pkg_reqs

        -- add in trusted package requirements for this module
        let new_trust = emptyImportAvails { imp_trust_pkgs = catMaybes pkgs }
        return tcg_env { tcg_imports = imp_info `plusImportAvails` new_trust }

    where
        imp_info = tcg_imports tcg_env     -- ImportAvails
        imports  = imp_mods imp_info       -- ImportedMods
        imports' = moduleEnvToList imports -- (Module, [ImportedModsVal])
        pkg_reqs = imp_trust_pkgs imp_info -- [PackageId]

        condense :: (Module, [ImportedModsVal]) -> Hsc (Module, SrcSpan, IsSafeImport)
        condense (_, [])   = panic "HscMain.condense: Pattern match failure!"
        condense (m, x:xs) = do (_,_,l,s) <- foldlM cond' x xs
                                return (m, l, s)
        
        -- ImportedModsVal = (ModuleName, Bool, SrcSpan, IsSafeImport)
        cond' :: ImportedModsVal -> ImportedModsVal -> Hsc ImportedModsVal
        cond' v1@(m1,_,l1,s1) (_,_,_,s2)
            | s1 /= s2
            = liftIO $ throwIO $ mkSrcErr $ unitBag $ mkPlainErrMsg l1
                    (text "Module" <+> ppr m1 <+> (text $ "is imported"
                        ++ " both as a safe and unsafe import!"))
            | otherwise
            = return v1

        lookup' :: Module -> Hsc (Maybe ModIface)
        lookup' m = do
            hsc_eps <- liftIO $ hscEPS hsc_env
            let pkgIfaceT = eps_PIT hsc_eps
                homePkgT = hsc_HPT hsc_env
                iface = lookupIfaceByModule dflags homePkgT pkgIfaceT m
            return iface

        isHomePkg :: Module -> Bool
        isHomePkg m
            | thisPackage dflags == modulePackageId m = True
            | otherwise                               = False

        -- | Check the package a module resides in is trusted.
        -- Safe compiled modules are trusted without requiring
        -- that their package is trusted. For trustworthy modules,
        -- modules in the home package are trusted but otherwise
        -- we check the package trust flag.
        packageTrusted :: SafeHaskellMode -> Bool -> Module -> Bool
        packageTrusted Sf_Safe False _ = True
        packageTrusted _ _ m
            | isHomePkg m = True
            | otherwise   = trusted $ getPackageDetails (pkgState dflags)
                                                        (modulePackageId m)

        -- Is a module trusted? Return Nothing if True, or a String
        -- if it isn't, containing the reason it isn't. Also return
        -- if the module trustworthy (true) or safe (false) so we know
        -- if we should check if the package itself is trusted in the
        -- future.
        isModSafe :: Module -> SrcSpan -> Hsc (Maybe SDoc, Bool)
        isModSafe m l = do
            iface <- lookup' m
            case iface of
                -- can't load iface to check trust!
                Nothing -> liftIO $ throwIO $ mkSrcErr $ unitBag $ mkPlainErrMsg l
                            $ text "Can't load the interface file for" <+> ppr m <>
                              text ", to check that it can be safely imported"

                -- got iface, check trust
                Just iface' -> do
                    let trust = getSafeMode $ mi_trust iface'
                        trust_own_pkg = mi_trust_pkg iface'
                        -- check module is trusted
                        safeM = trust `elem` [Sf_Safe, Sf_Trustworthy]
                        -- check package is trusted
                        safeP = packageTrusted trust trust_own_pkg m
                    if safeM && safeP
                        then return (Nothing, trust == Sf_Trustworthy)
                        else let err = Just $ if safeM
                                    then text "The package (" <> ppr (modulePackageId m) <>
                                         text ") the module resides in isn't trusted."
                                    else text "The module itself isn't safe."
                              in return (err, False)

        -- Here we check the transitive package trust requirements are OK still.
        checkPkgTrust :: [PackageId] -> Hsc ()
        checkPkgTrust pkgs = do
            case errors of
                [] -> return ()
                _  -> (liftIO . throwIO . mkSrcErr . listToBag) errors
            where
                errors = catMaybes $ map go pkgs
                go pkg
                    | trusted $ getPackageDetails (pkgState dflags) pkg
                    = Nothing
                    | otherwise
                    = Just $ mkPlainErrMsg noSrcSpan
                           $ text "The package (" <> ppr pkg <> text ") is required"
                          <> text " to be trusted but it isn't!"

        checkSafe :: (Module, SrcSpan, IsSafeImport) -> Hsc (Maybe PackageId)
        checkSafe (_, _, False) = return Nothing
        checkSafe (m, l, True ) = do
            (module_safe, tw) <- isModSafe m l
            case module_safe of
                Nothing -> return $ pkg tw
                Just s  -> liftIO $ throwIO $ mkSrcErr $ unitBag $ mkPlainErrMsg l
                            $ ppr m <+> text "can't be safely imported!"
                                <+> s
            where pkg False = Nothing
                  pkg True | isHomePkg m = Nothing
                           | otherwise   = Just (modulePackageId m)
                            
--------------------------------------------------------------
-- Simplifiers
--------------------------------------------------------------

hscSimplify :: HscEnv -> ModGuts -> IO ModGuts
hscSimplify hsc_env modguts = runHsc hsc_env $ hscSimplify' modguts

hscSimplify' :: ModGuts -> Hsc ModGuts
hscSimplify' ds_result
  = do hsc_env <- getHscEnv
       {-# SCC "Core2Core" #-}
         liftIO $ core2core hsc_env ds_result

--------------------------------------------------------------
-- Interface generators
--------------------------------------------------------------

hscSimpleIface :: TcGblEnv
               -> Maybe Fingerprint
               -> Hsc (ModIface, Bool, ModDetails)
hscSimpleIface tc_result mb_old_iface
  = do 
       hsc_env <- getHscEnv
       details <- liftIO $ mkBootModDetailsTc hsc_env tc_result
       (new_iface, no_change)
           <- {-# SCC "MkFinalIface" #-}
              ioMsgMaybe $ 
                mkIfaceTc hsc_env mb_old_iface details tc_result
       -- And the answer is ...
       liftIO $ dumpIfaceStats hsc_env
       return (new_iface, no_change, details)

hscNormalIface :: ModGuts
               -> Maybe Fingerprint
               -> Hsc (ModIface, Bool, ModDetails, CgGuts)
hscNormalIface simpl_result mb_old_iface
  = do 
       hsc_env <- getHscEnv
       (cg_guts, details) <- {-# SCC "CoreTidy" #-}
                             liftIO $ tidyProgram hsc_env simpl_result

	    -- BUILD THE NEW ModIface and ModDetails
	    --	and emit external core if necessary
	    -- This has to happen *after* code gen so that the back-end
	    -- info has been set.  Not yet clear if it matters waiting
	    -- until after code output
       (new_iface, no_change)
	   <- {-# SCC "MkFinalIface" #-}
	      ioMsgMaybe $ 
                   mkIface hsc_env mb_old_iface details simpl_result

       -- Emit external core
       -- This should definitely be here and not after CorePrep,
       -- because CorePrep produces unqualified constructor wrapper declarations,
       -- so its output isn't valid External Core (without some preprocessing).
       liftIO $ emitExternalCore (hsc_dflags hsc_env) cg_guts
       liftIO $ dumpIfaceStats hsc_env

 	    -- Return the prepared code.
       return (new_iface, no_change, details, cg_guts)

--------------------------------------------------------------
-- BackEnd combinators
--------------------------------------------------------------

hscWriteIface :: ModIface
              -> Bool
              -> ModSummary
              -> Hsc ()

hscWriteIface iface no_change mod_summary
    = do dflags <- getDynFlags
         unless no_change
           $ liftIO $ writeIfaceFile dflags (ms_location mod_summary) iface

-- | Compile to hard-code.
hscGenHardCode :: CgGuts -> ModSummary
               -> Hsc (Maybe FilePath) -- ^ @Just f@ <=> _stub.c is f
hscGenHardCode cgguts mod_summary
  = do
    hsc_env <- getHscEnv
    liftIO $ do
         let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                     -- From now on, we just use the bits we need.
                     cg_module   = this_mod,
                     cg_binds    = core_binds,
                     cg_tycons   = tycons,
                     cg_foreign  = foreign_stubs0,
                     cg_dep_pkgs = dependencies,
                     cg_hpc_info = hpc_info } = cgguts
             dflags = hsc_dflags hsc_env
             platform = targetPlatform dflags
             location = ms_location mod_summary
             data_tycons = filter isDataTyCon tycons
             -- cg_tycons includes newtypes, for the benefit of External Core,
             -- but we don't generate any code for newtypes

         -------------------
         -- PREPARE FOR CODE GENERATION
         -- Do saturation and convert to A-normal form
         prepd_binds <- {-# SCC "CorePrep" #-}
                        corePrepPgm dflags core_binds data_tycons ;
         -----------------  Convert to STG ------------------
         (stg_binds, cost_centre_info)
             <- {-# SCC "CoreToStg" #-}
                myCoreToStg dflags this_mod prepd_binds	

         let prof_init = profilingInitCode this_mod cost_centre_info
             foreign_stubs = foreign_stubs0 `appendStubC` prof_init

         ------------------  Code generation ------------------
         
         cmms <- if dopt Opt_TryNewCodeGen dflags
                 then do cmms <- tryNewCodeGen hsc_env this_mod data_tycons
                                 cost_centre_info
                                 stg_binds hpc_info
                         return cmms
                 else {-# SCC "CodeGen" #-}
                       codeGen dflags this_mod data_tycons
                               cost_centre_info
                               stg_binds hpc_info

         --- Optionally run experimental Cmm transformations ---
         cmms <- optionallyConvertAndOrCPS hsc_env cmms
                 -- unless certain dflags are on, the identity function
         ------------------  Code output -----------------------
         rawcmms <- cmmToRawCmm cmms
         dumpIfSet_dyn dflags Opt_D_dump_raw_cmm "Raw Cmm" (pprPlatform platform rawcmms)
         (_stub_h_exists, stub_c_exists)
             <- codeOutput dflags this_mod location foreign_stubs 
                dependencies rawcmms
         return stub_c_exists

hscInteractive :: (ModIface, ModDetails, CgGuts)
               -> ModSummary
               -> Hsc (InteractiveStatus, ModIface, ModDetails)
#ifdef GHCI
hscInteractive (iface, details, cgguts) mod_summary
    = do
         dflags <- getDynFlags
         let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                     -- From now on, we just use the bits we need.
                     cg_module   = this_mod,
                     cg_binds    = core_binds,
                     cg_tycons   = tycons,
                     cg_foreign  = foreign_stubs,
                     cg_modBreaks = mod_breaks } = cgguts

             location = ms_location mod_summary
             data_tycons = filter isDataTyCon tycons
             -- cg_tycons includes newtypes, for the benefit of External Core,
             -- but we don't generate any code for newtypes

         -------------------
         -- PREPARE FOR CODE GENERATION
         -- Do saturation and convert to A-normal form
         prepd_binds <- {-# SCC "CorePrep" #-}
                        liftIO $ corePrepPgm dflags core_binds data_tycons ;
         -----------------  Generate byte code ------------------
         comp_bc <- liftIO $ byteCodeGen dflags prepd_binds data_tycons mod_breaks
         ------------------ Create f-x-dynamic C-side stuff ---
         (_istub_h_exists, istub_c_exists) 
             <- liftIO $ outputForeignStubs dflags this_mod
                                            location foreign_stubs
         return (HscRecomp istub_c_exists (Just (comp_bc, mod_breaks))
                , iface, details)
#else
hscInteractive _ _ = panic "GHC not compiled with interpreter"
#endif

------------------------------

hscCompileCmmFile :: HscEnv -> FilePath -> IO ()
hscCompileCmmFile hsc_env filename
  = runHsc hsc_env $ do
      let dflags = hsc_dflags hsc_env
      cmm <- ioMsgMaybe $ parseCmmFile dflags filename
      liftIO $ do
        cmms <- optionallyConvertAndOrCPS hsc_env [cmm]
        rawCmms <- cmmToRawCmm cmms
        _ <- codeOutput dflags no_mod no_loc NoStubs [] rawCmms
        return ()
  where
	no_mod = panic "hscCmmFile: no_mod"
	no_loc = ModLocation{ ml_hs_file  = Just filename,
                              ml_hi_file  = panic "hscCmmFile: no hi file",
                              ml_obj_file = panic "hscCmmFile: no obj file" }

-------------------- Stuff for new code gen ---------------------

tryNewCodeGen   :: HscEnv -> Module -> [TyCon]
                -> CollectedCCs
                -> [(StgBinding,[(Id,[Id])])]
                -> HpcInfo
                -> IO [Cmm]
tryNewCodeGen hsc_env this_mod data_tycons
              cost_centre_info stg_binds hpc_info =
  do    { let dflags = hsc_dflags hsc_env
              platform = targetPlatform dflags
        ; prog <- StgCmm.codeGen dflags this_mod data_tycons
                         cost_centre_info stg_binds hpc_info
        ; dumpIfSet_dyn dflags Opt_D_dump_cmmz "Cmm produced by new codegen" 
                (pprCmms platform prog)

        -- We are building a single SRT for the entire module, so
        -- we must thread it through all the procedures as we cps-convert them.
        ; us <- mkSplitUniqSupply 'S'
        ; let initTopSRT = initUs_ us emptySRT
        ; (topSRT, prog) <- foldM (cmmPipeline hsc_env) (initTopSRT, []) prog

        ; let prog' = map cmmOfZgraph (srtToData topSRT : prog)
        ; dumpIfSet_dyn dflags Opt_D_dump_cmmz "Output Cmm" (pprPlatform platform prog')
        ; return prog' }


optionallyConvertAndOrCPS :: HscEnv -> [Cmm] -> IO [Cmm]
optionallyConvertAndOrCPS hsc_env cmms =
    do let dflags = hsc_dflags hsc_env
        --------  Optionally convert to and from zipper ------
       cmms <- if dopt Opt_ConvertToZipCfgAndBack dflags
               then mapM (testCmmConversion hsc_env) cmms
               else return cmms
       return cmms


testCmmConversion :: HscEnv -> Cmm -> IO Cmm
testCmmConversion hsc_env cmm =
    do let dflags = hsc_dflags hsc_env
           platform = targetPlatform dflags
       showPass dflags "CmmToCmm"
       dumpIfSet_dyn dflags Opt_D_dump_cvt_cmm "C-- pre-conversion" (pprPlatform platform cmm)
       --continuationC <- cmmCPS dflags abstractC >>= cmmToRawCmm
       us <- mkSplitUniqSupply 'C'
       let zgraph = initUs_ us (cmmToZgraph platform cmm)
       chosen_graph <-
        if dopt Opt_RunCPSZ dflags
            then do us <- mkSplitUniqSupply 'S'
                    let topSRT = initUs_ us emptySRT
                    (_, [zgraph]) <- cmmPipeline hsc_env (topSRT, []) zgraph
                    return zgraph
            else return (runCmmContFlowOpts zgraph)
       dumpIfSet_dyn dflags Opt_D_dump_cmmz "C-- Zipper Graph" (pprPlatform platform chosen_graph)
       showPass dflags "Convert from Z back to Cmm"
       let cvt = cmmOfZgraph chosen_graph
       dumpIfSet_dyn dflags Opt_D_dump_cvt_cmm "C-- post-conversion" (pprPlatform platform cvt)
       return cvt

myCoreToStg :: DynFlags -> Module -> [CoreBind]
            -> IO ( [(StgBinding,[(Id,[Id])])]  -- output program
	          , CollectedCCs) -- cost centre info (declared and used)

myCoreToStg dflags this_mod prepd_binds
 = do 
      stg_binds <- {-# SCC "Core2Stg" #-}
	     coreToStg (thisPackage dflags) prepd_binds

      (stg_binds2, cost_centre_info) <- {-# SCC "Stg2Stg" #-}
	     stg2stg dflags this_mod stg_binds

      return (stg_binds2, cost_centre_info)
\end{code}


%************************************************************************
%*									*
\subsection{Compiling a do-statement}
%*									*
%************************************************************************

When the UnlinkedBCOExpr is linked you get an HValue of type
	IO [HValue]
When you run it you get a list of HValues that should be 
the same length as the list of names; add them to the ClosureEnv.

A naked expression returns a singleton Name [it].

	What you type			The IO [HValue] that hscStmt returns
	-------------			------------------------------------
	let pat = expr		==> 	let pat = expr in return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	pat <- expr		==> 	expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	expr (of IO type)	==>	expr >>= \ v -> return [v]
	  [NB: result not printed]	bindings: [it]
	  

	expr (of non-IO type, 
	  result showable)	==>	let v = expr in print v >> return [v]
	  				bindings: [it]

	expr (of non-IO type, 
	  result not showable)	==>	error

\begin{code}
#ifdef GHCI
hscStmt		-- Compile a stmt all the way to an HValue, but don't run it
  :: HscEnv
  -> String			-- The statement
  -> IO (Maybe ([Id], HValue))
     -- ^ 'Nothing' <==> empty statement (or comment only), but no parse error
hscStmt hsc_env stmt = hscStmtWithLocation hsc_env stmt "<interactive>" 1

hscStmtWithLocation	-- Compile a stmt all the way to an HValue, but don't run it
  :: HscEnv
  -> String			-- The statement
  -> String                     -- the source
  -> Int                        -- ^ starting line
  -> IO (Maybe ([Id], HValue))
     -- ^ 'Nothing' <==> empty statement (or comment only), but no parse error
hscStmtWithLocation hsc_env stmt source linenumber = runHsc hsc_env $ do
    maybe_stmt <- hscParseStmtWithLocation source linenumber stmt
    case maybe_stmt of
      Nothing -> return Nothing
      Just parsed_stmt -> do  -- The real stuff

             -- Rename and typecheck it
	let icontext = hsc_IC hsc_env
	(ids, tc_expr) <- ioMsgMaybe $ 
                            tcRnStmt hsc_env icontext parsed_stmt
	    -- Desugar it
	let rdr_env  = ic_rn_gbl_env icontext
	    type_env = mkTypeEnv (map AnId (ic_tmp_ids icontext))
	ds_expr <- ioMsgMaybe $
                     deSugarExpr hsc_env iNTERACTIVE rdr_env type_env tc_expr
        handleWarnings

	-- Then desugar, code gen, and link it
	let src_span = srcLocSpan interactiveSrcLoc
        hsc_env <- getHscEnv
	hval <- liftIO $ hscCompileCoreExpr hsc_env src_span ds_expr

	return $ Just (ids, hval)

hscImport :: HscEnv -> String -> IO (ImportDecl RdrName)
hscImport hsc_env str = runHsc hsc_env $ do
    (L _ (HsModule{hsmodImports=is})) <- 
       hscParseThing parseModule str
    case is of
        [i] -> return (unLoc i)
        _ -> liftIO $ throwOneError $
                mkPlainErrMsg noSrcSpan $
                    ptext (sLit "parse error in import declaration")

hscTcExpr	-- Typecheck an expression (but don't run it)
  :: HscEnv
  -> String			-- The expression
  -> IO Type

hscTcExpr hsc_env expr = runHsc hsc_env $ do
    maybe_stmt <- hscParseStmt expr
    case maybe_stmt of
        Just (L _ (ExprStmt expr _ _ _)) ->
            ioMsgMaybe $ tcRnExpr hsc_env (hsc_IC hsc_env) expr
        _ ->
            liftIO $ throwIO $ mkSrcErr $ unitBag $ mkPlainErrMsg noSrcSpan
                (text "not an expression:" <+> quotes (text expr))

-- | Find the kind of a type
hscKcType
  :: HscEnv
  -> String			-- ^ The type
  -> IO Kind

hscKcType hsc_env str = runHsc hsc_env $ do
    ty <- hscParseType str
    ioMsgMaybe $ tcRnType hsc_env (hsc_IC hsc_env) ty

#endif
\end{code}

\begin{code}
#ifdef GHCI
hscParseStmt :: String -> Hsc (Maybe (LStmt RdrName))
hscParseStmt = hscParseThing parseStmt

hscParseStmtWithLocation :: String -> Int 
                         -> String -> Hsc (Maybe (LStmt RdrName))
hscParseStmtWithLocation source linenumber stmt = 
  hscParseThingWithLocation source linenumber parseStmt stmt

hscParseType :: String -> Hsc (LHsType RdrName)
hscParseType = hscParseThing parseType
#endif

hscParseIdentifier :: HscEnv -> String -> IO (Located RdrName)
hscParseIdentifier hsc_env str = runHsc hsc_env $ 
                                   hscParseThing parseIdentifier str

hscParseThing :: (Outputable thing)
	      => Lexer.P thing
	      -> String
	      -> Hsc thing
hscParseThing = hscParseThingWithLocation "<interactive>" 1

hscParseThingWithLocation :: (Outputable thing)
	      => String -> Int 
              -> Lexer.P thing
	      -> String
	      -> Hsc thing
hscParseThingWithLocation source linenumber parser str
 = {-# SCC "Parser" #-} do
      dflags <- getDynFlags
      liftIO $ showPass dflags "Parser"

      let buf = stringToStringBuffer str
          loc  = mkRealSrcLoc (fsLit source) linenumber 1

      case unP parser (mkPState dflags buf loc) of

        PFailed span err -> do
          let msg = mkPlainErrMsg span err
          liftIO $ throwIO (mkSrcErr (unitBag msg))

        POk pst thing -> do
          logWarningsReportErrors (getMessages pst)
          liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr thing)
          return thing
\end{code}

\begin{code}
hscCompileCore :: HscEnv
               -> Bool
               -> ModSummary
               -> [CoreBind]
               -> IO ()

hscCompileCore hsc_env simplify mod_summary binds
  = runHsc hsc_env $ do
      let maybe_simplify mod_guts | simplify = hscSimplify' mod_guts
                                  | otherwise = return mod_guts
      guts <- maybe_simplify (mkModGuts (ms_mod mod_summary) binds)
      (iface, changed, _details, cgguts) <- hscNormalIface guts Nothing
      hscWriteIface iface changed mod_summary
      _ <- hscGenHardCode cgguts mod_summary
      return ()

-- Makes a "vanilla" ModGuts.
mkModGuts :: Module -> [CoreBind] -> ModGuts
mkModGuts mod binds = ModGuts {
  mg_module = mod,
  mg_boot = False,
  mg_exports = [],
  mg_deps = noDependencies,
  mg_dir_imps = emptyModuleEnv,
  mg_used_names = emptyNameSet,
  mg_used_th = False,
  mg_rdr_env = emptyGlobalRdrEnv,
  mg_fix_env = emptyFixityEnv,
  mg_types = emptyTypeEnv,
  mg_insts = [],
  mg_fam_insts = [],
  mg_rules = [],
  mg_vect_decls = [],
  mg_binds = binds,
  mg_foreign = NoStubs,
  mg_warns = NoWarnings,
  mg_anns = [],
  mg_hpc_info = emptyHpcInfo False,
  mg_modBreaks = emptyModBreaks,
  mg_vect_info = noVectInfo,
  mg_inst_env = emptyInstEnv,
  mg_fam_inst_env = emptyFamInstEnv,
  mg_trust_pkg = False
}
\end{code}

%************************************************************************
%*									*
	Desugar, simplify, convert to bytecode, and link an expression
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
hscCompileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO HValue
hscCompileCoreExpr hsc_env srcspan ds_expr
  | rtsIsProfiled
  = throwIO (InstallationError "You can't call hscCompileCoreExpr in a profiled compiler")
    	  -- Otherwise you get a seg-fault when you run it

  | otherwise = do
    let dflags = hsc_dflags hsc_env
    let lint_on = dopt Opt_DoCoreLinting dflags

  	-- Simplify it
    simpl_expr <- simplifyExpr dflags ds_expr

  	-- Tidy it (temporary, until coreSat does cloning)
    let tidy_expr = tidyExpr emptyTidyEnv simpl_expr

  	-- Prepare for codegen
    prepd_expr <- corePrepExpr dflags tidy_expr

  	-- Lint if necessary
  	-- ToDo: improve SrcLoc
    when lint_on $
       let ictxt = hsc_IC hsc_env
           tyvars = varSetElems (tyVarsOfTypes (map idType (ic_tmp_ids ictxt)))
       in
           case lintUnfolding noSrcLoc tyvars prepd_expr of
  	      Just err -> pprPanic "hscCompileCoreExpr" err
  	      Nothing  -> return ()

          -- Convert to BCOs
    bcos <- coreExprToBCOs dflags prepd_expr

  	-- link it
    hval <- linkExpr hsc_env srcspan bcos

    return hval
#endif
\end{code}


%************************************************************************
%*									*
	Statistics on reading interfaces
%*									*
%************************************************************************

\begin{code}
dumpIfaceStats :: HscEnv -> IO ()
dumpIfaceStats hsc_env
  = do	{ eps <- readIORef (hsc_EPS hsc_env)
	; dumpIfSet (dump_if_trace || dump_rn_stats)
	      	    "Interface statistics"
	      	    (ifaceStats eps) }
  where
    dflags = hsc_dflags hsc_env
    dump_rn_stats = dopt Opt_D_dump_rn_stats dflags
    dump_if_trace = dopt Opt_D_dump_if_trace dflags
\end{code}

%************************************************************************
%*									*
	Progress Messages: Module i of n
%*									*
%************************************************************************

\begin{code}
showModuleIndex :: Maybe (Int, Int) -> String
showModuleIndex Nothing = ""
showModuleIndex (Just (i,n)) = "[" ++ padded ++ " of " ++ n_str ++ "] "
    where
        n_str = show n
        i_str = show i
        padded = replicate (length n_str - length i_str) ' ' ++ i_str
\end{code}
