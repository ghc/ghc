%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%
\begin{code}
-- | Main driver for the compiling plain Haskell source code.
--
-- This module implements compilation of a Haskell-only source file.  It is
-- /not/ concerned with preprocessing of source files; this is handled in
-- "DriverPipeline".
--
module HscMain
    ( newHscEnv, hscCmmFile
    , hscParseIdentifier
    , hscSimplify
    , hscNormalIface, hscWriteIface, hscGenHardCode
#ifdef GHCI
    , hscStmt, hscTcExpr, hscImport, hscKcType
    , compileExpr
#endif
    , HsCompiler(..)
    , hscOneShotCompiler, hscNothingCompiler
    , hscInteractiveCompiler, hscBatchCompiler
    , hscCompileOneShot     -- :: Compiler HscStatus
    , hscCompileBatch       -- :: Compiler (HscStatus, ModIface, ModDetails)
    , hscCompileNothing     -- :: Compiler (HscStatus, ModIface, ModDetails)
    , hscCompileInteractive -- :: Compiler (InteractiveStatus, ModIface, ModDetails)
    , hscCheckRecompBackend
    , HscStatus' (..)
    , InteractiveStatus, HscStatus

    -- The new interface
    , hscParse
    , hscTypecheck
    , hscTypecheckRename
    , hscDesugar
    , makeSimpleIface
    , makeSimpleDetails
    ) where

#ifdef GHCI
import CodeOutput	( outputForeignStubs )
import ByteCodeGen	( byteCodeGen, coreExprToBCOs )
import Linker		( HValue, linkExpr )
import CoreTidy		( tidyExpr )
import CorePrep		( corePrepExpr )
import Desugar          ( deSugarExpr )
import SimplCore        ( simplifyExpr )
import TcRnDriver	( tcRnStmt, tcRnExpr, tcRnType ) 
import Type		( Type )
import PrelNames	( iNTERACTIVE )
import {- Kind parts of -} Type		( Kind )
import CoreLint		( lintUnfolding )
import DsMeta		( templateHaskellNames )
import SrcLoc		( SrcSpan, noSrcLoc, interactiveSrcLoc, srcLocSpan, noSrcSpan, unLoc )
import VarSet
import VarEnv		( emptyTidyEnv )
import Panic
#endif

import Id		( Id )
import Module		( emptyModuleEnv, ModLocation(..), Module )
import RdrName
import HsSyn
import CoreSyn
import SrcLoc		( Located(..) )
import StringBuffer
import Parser
import Lexer
import SrcLoc		( mkSrcLoc )
import TcRnDriver	( tcRnModule )
import TcIface		( typecheckIface )
import TcRnMonad	( initIfaceCheck, TcGblEnv(..) )
import IfaceEnv		( initNameCache )
import LoadIface	( ifaceStats, initExternalPackageState )
import PrelInfo		( wiredInThings, basicKnownKeyNames )
import MkIface
import Desugar          ( deSugar )
import SimplCore        ( core2core )
import TidyPgm
import CorePrep		( corePrepPgm )
import CoreToStg	( coreToStg )
import qualified StgCmm	( codeGen )
import StgSyn
import CostCentre
import TyCon		( TyCon, isDataTyCon )
import Name		( Name, NamedThing(..) )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
import Cmm              ( Cmm )
import PprCmm		( pprCmms )
import CmmParse		( parseCmmFile )
import CmmBuildInfoTables
import CmmCPS
import CmmCPSZ
import CmmInfo
import OptimizationFuel ( initOptFuelState )
import CmmCvt
import CmmTx
import CmmContFlowOpt
import CodeOutput	( codeOutput )
import NameEnv          ( emptyNameEnv )
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
import Bag		( unitBag )
import Exception
-- import MonadUtils

import Control.Monad
-- import System.IO
import Data.IORef
\end{code}
#include "HsVersions.h"


%************************************************************************
%*									*
		Initialisation
%*									*
%************************************************************************

\begin{code}
newHscEnv :: GhcApiCallbacks -> DynFlags -> IO HscEnv
newHscEnv callbacks dflags
  = do 	{ eps_var <- newIORef initExternalPackageState
	; us      <- mkSplitUniqSupply 'r'
	; nc_var  <- newIORef (initNameCache us knownKeyNames)
	; fc_var  <- newIORef emptyUFM
	; mlc_var <- newIORef emptyModuleEnv
        ; optFuel <- initOptFuelState
	; return (HscEnv { hsc_dflags = dflags,
                           hsc_callbacks = callbacks,
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


knownKeyNames :: [Name]	-- Put here to avoid loops involving DsMeta,
			-- where templateHaskellNames are defined
knownKeyNames = map getName wiredInThings 
	      ++ basicKnownKeyNames
#ifdef GHCI
	      ++ templateHaskellNames
#endif
\end{code}


\begin{code}
-- | parse a file, returning the abstract syntax
hscParse :: GhcMonad m =>
            ModSummary
         -> m (Located (HsModule RdrName))
hscParse mod_summary = do
   hsc_env <- getSession
   let dflags        = hsc_dflags hsc_env
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

   let loc  = mkSrcLoc (mkFastString src_filename) 1 1

   case unP parseModule (mkPState dflags buf loc) of
     PFailed span err ->
         throwOneError (mkPlainErrMsg span err)

     POk pst rdr_module -> do
         let ms@(warns,errs) = getMessages pst
         logWarnings warns
         if errorsFound dflags ms then
           liftIO $ throwIO $ mkSrcErr errs
          else liftIO $ do
           dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;
           dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
                               (ppSourceStats False rdr_module) ;
           return rdr_module
          -- ToDo: free the string buffer later.

-- | Rename and typecheck a module
hscTypecheck :: GhcMonad m =>
                ModSummary -> Located (HsModule RdrName)
             -> m TcGblEnv
hscTypecheck mod_summary rdr_module = do
      hsc_env <- getSession
      r <- {-# SCC "Typecheck-Rename" #-}
           ioMsgMaybe $ tcRnModule hsc_env (ms_hsc_src mod_summary) False rdr_module
      return r

-- XXX: should this really be a Maybe X?  Check under which circumstances this
-- can become a Nothing and decide whether this should instead throw an
-- exception/signal an error.
type RenamedStuff = 
        (Maybe (HsGroup Name, [LImportDecl Name], Maybe [LIE Name],
                Maybe LHsDocString))

-- | Rename and typecheck a module, additionally returning the renamed syntax
hscTypecheckRename ::
       GhcMonad m =>
       ModSummary -> Located (HsModule RdrName)
    -> m (TcGblEnv, RenamedStuff)
hscTypecheckRename mod_summary rdr_module = do
    hsc_env <- getSession
    tc_result
          <- {-# SCC "Typecheck-Rename" #-}
             ioMsgMaybe $ tcRnModule hsc_env (ms_hsc_src mod_summary) True rdr_module

    let -- This 'do' is in the Maybe monad!
        rn_info = do { decl <- tcg_rn_decls tc_result
                     ; let imports = tcg_rn_imports tc_result
                           exports = tcg_rn_exports tc_result
                           doc_hdr  = tcg_doc_hdr tc_result
                     ; return (decl,imports,exports,doc_hdr) }

    return (tc_result, rn_info)

-- | Convert a typechecked module to Core
hscDesugar :: GhcMonad m => ModSummary -> TcGblEnv -> m ModGuts
hscDesugar mod_summary tc_result =
  withSession $ \hsc_env ->
    ioMsgMaybe $ deSugar hsc_env (ms_location mod_summary) tc_result

-- | Make a 'ModIface' from the results of typechecking.  Used when
-- not optimising, and the interface doesn't need to contain any
-- unfoldings or other cross-module optimisation info.
-- ToDo: the old interface is only needed to get the version numbers,
-- we should use fingerprint versions instead.
makeSimpleIface :: GhcMonad m =>
                   Maybe ModIface -> TcGblEnv -> ModDetails
                -> m (ModIface,Bool)
makeSimpleIface maybe_old_iface tc_result details =
  withSession $ \hsc_env ->
  ioMsgMaybe $ mkIfaceTc hsc_env (fmap mi_iface_hash maybe_old_iface) details tc_result

-- | Make a 'ModDetails' from the results of typechecking.  Used when
-- typechecking only, as opposed to full compilation.
makeSimpleDetails :: GhcMonad m => TcGblEnv -> m ModDetails
makeSimpleDetails tc_result =
    withSession $ \hsc_env -> liftIO $ mkBootModDetailsTc hsc_env tc_result
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
core files to either byte-code, hard-code (C, asm, Java, ect) or to
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
       Bool -- Has stub files.  This is a hack. We can't compile C files here
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
type Compiler result =  GhcMonad m =>
                        HscEnv
                     -> ModSummary
                     -> Bool                -- True <=> source unchanged
                     -> Maybe ModIface      -- Old interface, if available
                     -> Maybe (Int,Int)     -- Just (i,n) <=> module i of n (for msgs)
                     -> m result

data HsCompiler a
  = HsCompiler {
    -- | Called when no recompilation is necessary.
    hscNoRecomp :: GhcMonad m =>
                   ModIface -> m a,

    -- | Called to recompile the module.
    hscRecompile :: GhcMonad m =>
                    ModSummary -> Maybe Fingerprint -> m a,

    hscBackend :: GhcMonad m =>
                  TcGblEnv -> ModSummary -> Maybe Fingerprint -> m a,

    -- | Code generation for Boot modules.
    hscGenBootOutput :: GhcMonad m =>
                        TcGblEnv -> ModSummary -> Maybe Fingerprint -> m a,

    -- | Code generation for normal modules.
    hscGenOutput :: GhcMonad m =>
                    ModGuts  -> ModSummary -> Maybe Fingerprint -> m a
  }

genericHscCompile :: GhcMonad m =>
                     HsCompiler a
                  -> (Maybe (Int,Int) -> Bool -> ModSummary -> m ())
                  -> HscEnv -> ModSummary -> Bool
                  -> Maybe ModIface -> Maybe (Int, Int)
                  -> m a
genericHscCompile compiler hscMessage
                  hsc_env mod_summary source_unchanged
                  mb_old_iface0 mb_mod_index =
   withTempSession (\_ -> hsc_env) $ do
     (recomp_reqd, mb_checked_iface)
         <- {-# SCC "checkOldIface" #-}
            liftIO $ checkOldIface hsc_env mod_summary
                                   source_unchanged mb_old_iface0
     -- save the interface that comes back from checkOldIface.
     -- In one-shot mode we don't have the old iface until this
     -- point, when checkOldIface reads it from the disk.
     let mb_old_hash = fmap mi_iface_hash mb_checked_iface
     case mb_checked_iface of
       Just iface | not recomp_reqd
           -> do hscMessage mb_mod_index False mod_summary
                 hscNoRecomp compiler iface
       _otherwise
           -> do hscMessage mb_mod_index True mod_summary
                 hscRecompile compiler mod_summary mb_old_hash

hscCheckRecompBackend :: HsCompiler a -> TcGblEnv -> Compiler a
hscCheckRecompBackend compiler tc_result 
                   hsc_env mod_summary source_unchanged mb_old_iface _m_of_n =
   withTempSession (\_ -> hsc_env) $ do
     (recomp_reqd, mb_checked_iface)
         <- {-# SCC "checkOldIface" #-}
            liftIO $ checkOldIface hsc_env mod_summary
                                   source_unchanged mb_old_iface

     let mb_old_hash = fmap mi_iface_hash mb_checked_iface
     case mb_checked_iface of
       Just iface | not recomp_reqd
           -> hscNoRecomp compiler iface{ mi_globals = Just (tcg_rdr_env tc_result) }
       _otherwise
           -> hscBackend compiler tc_result mod_summary mb_old_hash

genericHscRecompile :: GhcMonad m =>
                       HsCompiler a
                    -> ModSummary -> Maybe Fingerprint
                    -> m a
genericHscRecompile compiler mod_summary mb_old_hash
  | ExtCoreFile <- ms_hsc_src mod_summary =
      panic "GHC does not currently support reading External Core files"
  | otherwise = do
      tc_result <- hscFileFrontEnd mod_summary
      hscBackend compiler tc_result mod_summary mb_old_hash

genericHscBackend :: GhcMonad m =>
                     HsCompiler a
                  -> TcGblEnv -> ModSummary -> Maybe Fingerprint
                  -> m a
genericHscBackend compiler tc_result mod_summary mb_old_hash
  | HsBootFile <- ms_hsc_src mod_summary =
      hscGenBootOutput compiler tc_result mod_summary mb_old_hash
  | otherwise = do
      guts <- hscDesugar mod_summary tc_result
      hscGenOutput compiler guts mod_summary mb_old_hash

--------------------------------------------------------------
-- Compilers
--------------------------------------------------------------

hscOneShotCompiler :: HsCompiler OneShotResult
hscOneShotCompiler =
  HsCompiler {

    hscNoRecomp = \_old_iface -> do
      withSession (liftIO . dumpIfaceStats)
      return HscNoRecomp

  , hscRecompile = genericHscRecompile hscOneShotCompiler

  , hscBackend = \ tc_result mod_summary mb_old_hash -> do
       hsc_env <- getSession
       case hscTarget (hsc_dflags hsc_env) of
         HscNothing -> return (HscRecomp False ())
         _otherw    -> genericHscBackend hscOneShotCompiler 
                                         tc_result mod_summary mb_old_hash

  , hscGenBootOutput = \tc_result mod_summary mb_old_iface -> do
       (iface, changed, _) <- hscSimpleIface tc_result mb_old_iface
       hscWriteIface iface changed mod_summary
       return (HscRecomp False ())

  , hscGenOutput = \guts0 mod_summary mb_old_iface -> do
       guts <- hscSimplify guts0
       (iface, changed, _details, cgguts)
           <- hscNormalIface guts mb_old_iface
       hscWriteIface iface changed mod_summary
       hasStub <- hscGenHardCode cgguts mod_summary
       return (HscRecomp hasStub ())
  }

-- Compile Haskell, boot and extCore in OneShot mode.
hscCompileOneShot :: Compiler OneShotResult
hscCompileOneShot hsc_env mod_summary src_changed mb_old_iface mb_i_of_n = do
       -- One-shot mode needs a knot-tying mutable variable for interface
       -- files.  See TcRnTypes.TcGblEnv.tcg_type_env_var.
      type_env_var <- liftIO $ newIORef emptyNameEnv
      let
         mod = ms_mod mod_summary
         hsc_env' = hsc_env{ hsc_type_env_var = Just (mod, type_env_var) }
      ---
      genericHscCompile hscOneShotCompiler
                        oneShotMsg hsc_env' mod_summary src_changed
                        mb_old_iface mb_i_of_n


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
       (iface, changed, details)
           <- hscSimpleIface tc_result mb_old_iface
       hscWriteIface iface changed mod_summary
       return (HscRecomp False (), iface, details)

  , hscGenOutput = \guts0 mod_summary mb_old_iface -> do
       guts <- hscSimplify guts0
       (iface, changed, details, cgguts)
           <- hscNormalIface guts mb_old_iface
       hscWriteIface iface changed mod_summary
       hasStub <- hscGenHardCode cgguts mod_summary
       return (HscRecomp hasStub (), iface, details)
  }

-- Compile Haskell, boot and extCore in batch mode.
hscCompileBatch :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileBatch = genericHscCompile hscBatchCompiler batchMsg

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
       return (HscRecomp False Nothing, iface, details)

  , hscGenOutput = \guts0 mod_summary mb_old_iface -> do
       guts <- hscSimplify guts0
       (iface, _changed, details, cgguts)
           <- hscNormalIface guts mb_old_iface
       hscInteractive (iface, details, cgguts) mod_summary
  }

-- Compile Haskell, extCore to bytecode.
hscCompileInteractive :: Compiler (InteractiveStatus, ModIface, ModDetails)
hscCompileInteractive = genericHscCompile hscInteractiveCompiler batchMsg

--------------------------------------------------------------

hscNothingCompiler :: HsCompiler NothingResult
hscNothingCompiler =
  HsCompiler {
    hscNoRecomp = \iface -> do
       details <- genModDetails iface
       return (HscNoRecomp, iface, details)

  , hscRecompile = genericHscRecompile hscNothingCompiler

  , hscBackend = \tc_result _mod_summary mb_old_iface -> do
       (iface, _changed, details) <- hscSimpleIface tc_result mb_old_iface
       return (HscRecomp False (), iface, details)

  , hscGenBootOutput = \_ _ _ ->
        panic "hscCompileNothing: hscGenBootOutput should not be called"

  , hscGenOutput = \_ _ _ ->
        panic "hscCompileNothing: hscGenOutput should not be called"
  }

-- Type-check Haskell and .hs-boot only (no external core)
hscCompileNothing :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileNothing = genericHscCompile hscNothingCompiler batchMsg

--------------------------------------------------------------
-- NoRecomp handlers
--------------------------------------------------------------

genModDetails :: GhcMonad m => ModIface -> m ModDetails
genModDetails old_iface =
    withSession $ \hsc_env -> liftIO $ do
      new_details <- {-# SCC "tcRnIface" #-}
                     initIfaceCheck hsc_env $
                     typecheckIface old_iface
      dumpIfaceStats hsc_env
      return new_details

--------------------------------------------------------------
-- Progress displayers.
--------------------------------------------------------------

oneShotMsg :: GhcMonad m => Maybe (Int,Int) -> Bool -> ModSummary -> m ()
oneShotMsg _mb_mod_index recomp _mod_summary
    = do hsc_env <- getSession
         liftIO $ do
         if recomp
            then return ()
            else compilationProgressMsg (hsc_dflags hsc_env) $
                     "compilation IS NOT required"

batchMsg :: GhcMonad m => Maybe (Int,Int) -> Bool -> ModSummary -> m ()
batchMsg mb_mod_index recomp mod_summary
    = do hsc_env <- getSession
         let showMsg msg = compilationProgressMsg (hsc_dflags hsc_env) $
                           (showModuleIndex mb_mod_index ++
                            msg ++ showModMsg (hscTarget (hsc_dflags hsc_env)) recomp mod_summary)
         liftIO $ do
         if recomp
            then showMsg "Compiling "
            else if verbosity (hsc_dflags hsc_env) >= 2
                    then showMsg "Skipping  "
                    else return ()

--------------------------------------------------------------
-- FrontEnds
--------------------------------------------------------------
hscFileFrontEnd :: GhcMonad m => ModSummary -> m TcGblEnv
hscFileFrontEnd mod_summary =
    do rdr_module <- hscParse mod_summary
       hscTypecheck mod_summary rdr_module

--------------------------------------------------------------
-- Simplifiers
--------------------------------------------------------------

hscSimplify :: GhcMonad m => ModGuts -> m ModGuts
hscSimplify ds_result
  = do hsc_env <- getSession
       simpl_result <- {-# SCC "Core2Core" #-}
                       liftIO $ core2core hsc_env ds_result
       return simpl_result

--------------------------------------------------------------
-- Interface generators
--------------------------------------------------------------

hscSimpleIface :: GhcMonad m =>
                  TcGblEnv
               -> Maybe Fingerprint
               -> m (ModIface, Bool, ModDetails)
hscSimpleIface tc_result mb_old_iface
  = do hsc_env <- getSession
       details <- liftIO $ mkBootModDetailsTc hsc_env tc_result
       (new_iface, no_change)
           <- {-# SCC "MkFinalIface" #-}
              ioMsgMaybe $ mkIfaceTc hsc_env mb_old_iface details tc_result
       -- And the answer is ...
       liftIO $ dumpIfaceStats hsc_env
       return (new_iface, no_change, details)

hscNormalIface :: GhcMonad m =>
                  ModGuts
               -> Maybe Fingerprint
               -> m (ModIface, Bool, ModDetails, CgGuts)
hscNormalIface simpl_result mb_old_iface
  = do hsc_env <- getSession

       (cg_guts, details) <- {-# SCC "CoreTidy" #-}
                             liftIO $ tidyProgram hsc_env simpl_result

	    -- BUILD THE NEW ModIface and ModDetails
	    --	and emit external core if necessary
	    -- This has to happen *after* code gen so that the back-end
	    -- info has been set.  Not yet clear if it matters waiting
	    -- until after code output
       (new_iface, no_change)
	   <- {-# SCC "MkFinalIface" #-}
	      ioMsgMaybe $ mkIface hsc_env mb_old_iface
                                   details simpl_result
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

hscWriteIface :: GhcMonad m =>
                 ModIface -> Bool
              -> ModSummary
              -> m ()
hscWriteIface iface no_change mod_summary
    = do hsc_env <- getSession
         let dflags = hsc_dflags hsc_env
         liftIO $ do
         unless no_change
           $ writeIfaceFile dflags (ms_location mod_summary) iface

-- | Compile to hard-code.
hscGenHardCode :: GhcMonad m =>
                  CgGuts -> ModSummary
               -> m Bool -- ^ @True@ <=> stub.c exists
hscGenHardCode cgguts mod_summary
    = withSession $ \hsc_env -> liftIO $ do
         let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                     -- From now on, we just use the bits we need.
                     cg_module   = this_mod,
                     cg_binds    = core_binds,
                     cg_tycons   = tycons,
                     cg_dir_imps = dir_imps,
                     cg_foreign  = foreign_stubs,
                     cg_dep_pkgs = dependencies,
                     cg_hpc_info = hpc_info } = cgguts
             dflags = hsc_dflags hsc_env
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

         ------------------  Code generation ------------------
         cmms <- if dopt Opt_TryNewCodeGen (hsc_dflags hsc_env)
                 then do cmms <- tryNewCodeGen hsc_env this_mod data_tycons
                                 dir_imps cost_centre_info
                                 stg_binds hpc_info
                         return cmms
                 else {-# SCC "CodeGen" #-}
                       codeGen dflags this_mod data_tycons
                               dir_imps cost_centre_info
                               stg_binds hpc_info

         --- Optionally run experimental Cmm transformations ---
         -- cmms <- optionallyConvertAndOrCPS hsc_env cmms
                 -- unless certain dflags are on, the identity function
         ------------------  Code output -----------------------
         rawcmms <- cmmToRawCmm cmms
         dumpIfSet_dyn dflags Opt_D_dump_cmmz "Raw Cmm" (ppr rawcmms)
         (_stub_h_exists, stub_c_exists)
             <- codeOutput dflags this_mod location foreign_stubs 
                dependencies rawcmms
         return stub_c_exists

hscInteractive :: GhcMonad m =>
                  (ModIface, ModDetails, CgGuts)
               -> ModSummary
               -> m (InteractiveStatus, ModIface, ModDetails)
#ifdef GHCI
hscInteractive (iface, details, cgguts) mod_summary
    = do hsc_env <- getSession
         liftIO $ do
         let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                     -- From now on, we just use the bits we need.
                     cg_module   = this_mod,
                     cg_binds    = core_binds,
                     cg_tycons   = tycons,
                     cg_foreign  = foreign_stubs,
                     cg_modBreaks = mod_breaks } = cgguts
             dflags = hsc_dflags hsc_env
             location = ms_location mod_summary
             data_tycons = filter isDataTyCon tycons
             -- cg_tycons includes newtypes, for the benefit of External Core,
             -- but we don't generate any code for newtypes

         -------------------
         -- PREPARE FOR CODE GENERATION
         -- Do saturation and convert to A-normal form
         prepd_binds <- {-# SCC "CorePrep" #-}
                        corePrepPgm dflags core_binds data_tycons ;
         -----------------  Generate byte code ------------------
         comp_bc <- byteCodeGen dflags prepd_binds data_tycons mod_breaks
         ------------------ Create f-x-dynamic C-side stuff ---
         (_istub_h_exists, istub_c_exists) 
             <- outputForeignStubs dflags this_mod location foreign_stubs
         return (HscRecomp istub_c_exists (Just (comp_bc, mod_breaks))
                , iface, details)
#else
hscInteractive _ _ = panic "GHC not compiled with interpreter"
#endif

------------------------------

hscCmmFile :: GhcMonad m => HscEnv -> FilePath -> m ()
hscCmmFile hsc_env filename = do
    dflags <- return $ hsc_dflags hsc_env
    cmm <- ioMsgMaybe $
             parseCmmFile dflags filename
    cmms <- liftIO $ optionallyConvertAndOrCPS hsc_env [cmm]
    rawCmms <- liftIO $ cmmToRawCmm cmms
    _ <- liftIO $ codeOutput dflags no_mod no_loc NoStubs [] rawCmms
    return ()
  where
	no_mod = panic "hscCmmFile: no_mod"
	no_loc = ModLocation{ ml_hs_file  = Just filename,
                              ml_hi_file  = panic "hscCmmFile: no hi file",
                              ml_obj_file = panic "hscCmmFile: no obj file" }

-------------------- Stuff for new code gen ---------------------

tryNewCodeGen	:: HscEnv -> Module -> [TyCon] -> [Module]
		-> CollectedCCs
		-> [(StgBinding,[(Id,[Id])])]
		-> HpcInfo
		-> IO [Cmm]
tryNewCodeGen hsc_env this_mod data_tycons imported_mods 
	      cost_centre_info stg_binds hpc_info =
  do	{ let dflags = hsc_dflags hsc_env
        ; prog <- StgCmm.codeGen dflags this_mod data_tycons imported_mods 
		 	 cost_centre_info stg_binds hpc_info
	; dumpIfSet_dyn dflags Opt_D_dump_cmmz "Cmm produced by new codegen" 
		(pprCmms prog)

	; prog <- return $ map (runTx $ runCmmOpts cmmCfgOptsZ) prog
		-- Control flow optimisation

        -- We are building a single SRT for the entire module, so
        -- we must thread it through all the procedures as we cps-convert them.
        ; us <- mkSplitUniqSupply 'S'
        ; let topSRT = initUs_ us emptySRT
	; (topSRT, prog) <- foldM (protoCmmCPSZ hsc_env) (topSRT, []) prog
		-- The main CPS conversion

	; prog <- return $ map (runTx $ runCmmOpts cmmCfgOptsZ) (srtToData topSRT : prog)
		-- Control flow optimisation, again

	; let prog' = map cmmOfZgraph prog
	; dumpIfSet_dyn dflags Opt_D_dump_cmmz "Output Cmm" (ppr prog')
	; return prog' }


optionallyConvertAndOrCPS :: HscEnv -> [Cmm] -> IO [Cmm]
optionallyConvertAndOrCPS hsc_env cmms =
    do let dflags = hsc_dflags hsc_env
        --------  Optionally convert to and from zipper ------
       cmms <- if dopt Opt_ConvertToZipCfgAndBack dflags
               then mapM (testCmmConversion hsc_env) cmms
               else return cmms
         ---------  Optionally convert to CPS (MDA) -----------
       cmms <- if not (dopt Opt_ConvertToZipCfgAndBack dflags) &&
                  dopt Opt_RunCPS dflags
               then cmmCPS dflags cmms
               else return cmms
       return cmms


testCmmConversion :: HscEnv -> Cmm -> IO Cmm
testCmmConversion hsc_env cmm =
    do let dflags = hsc_dflags hsc_env
       showPass dflags "CmmToCmm"
       dumpIfSet_dyn dflags Opt_D_dump_cvt_cmm "C-- pre-conversion" (ppr cmm)
       --continuationC <- cmmCPS dflags abstractC >>= cmmToRawCmm
       us <- mkSplitUniqSupply 'C'
       let cfopts = runTx $ runCmmOpts cmmCfgOptsZ
       let cvtm = do g <- cmmToZgraph cmm
                     return $ cfopts g
       let zgraph = initUs_ us cvtm
       us <- mkSplitUniqSupply 'S'
       let topSRT = initUs_ us emptySRT
       (_, [cps_zgraph]) <- protoCmmCPSZ hsc_env (topSRT, []) zgraph
       let chosen_graph = if dopt Opt_RunCPSZ dflags then cps_zgraph else zgraph
       dumpIfSet_dyn dflags Opt_D_dump_cmmz "C-- Zipper Graph" (ppr chosen_graph)
       showPass dflags "Convert from Z back to Cmm"
       let cvt = cmmOfZgraph $ cfopts $ chosen_graph
       dumpIfSet_dyn dflags Opt_D_dump_cvt_cmm "C-- post-conversion" (ppr cvt)
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
  :: GhcMonad m =>
     HscEnv
  -> String			-- The statement
  -> m (Maybe ([Id], HValue))
     -- ^ 'Nothing' <==> empty statement (or comment only), but no parse error
hscStmt hsc_env stmt = do
    maybe_stmt <- hscParseStmt (hsc_dflags hsc_env) stmt
    case maybe_stmt of
      Nothing -> return Nothing
      Just parsed_stmt -> do  -- The real stuff

             -- Rename and typecheck it
	let icontext = hsc_IC hsc_env
	(ids, tc_expr) <- ioMsgMaybe $ tcRnStmt hsc_env icontext parsed_stmt
	    -- Desugar it
	let rdr_env  = ic_rn_gbl_env icontext
	    type_env = mkTypeEnv (map AnId (ic_tmp_ids icontext))
	ds_expr <- ioMsgMaybe $
                     deSugarExpr hsc_env iNTERACTIVE rdr_env type_env tc_expr

	-- Then desugar, code gen, and link it
	let src_span = srcLocSpan interactiveSrcLoc
	hval <- liftIO $ compileExpr hsc_env src_span ds_expr

	return $ Just (ids, hval)

hscImport :: GhcMonad m => HscEnv -> String -> m (ImportDecl RdrName)
hscImport hsc_env str = do
    (L _ (HsModule{hsmodImports=is})) <- hscParseThing parseModule (hsc_dflags hsc_env) str
    case is of
        [i] -> return (unLoc i)
        _ -> throwOneError (mkPlainErrMsg noSrcSpan (ptext (sLit "parse error in import declaration")))

hscTcExpr	-- Typecheck an expression (but don't run it)
  :: GhcMonad m =>
     HscEnv
  -> String			-- The expression
  -> m Type

hscTcExpr hsc_env expr = do
    maybe_stmt <- hscParseStmt (hsc_dflags hsc_env) expr
    let icontext = hsc_IC hsc_env
    case maybe_stmt of
      Just (L _ (ExprStmt expr _ _)) -> do
          ty <- ioMsgMaybe $ tcRnExpr hsc_env icontext expr
          return ty
      _ -> do throw $ mkSrcErr $ unitBag $ mkPlainErrMsg
                        noSrcSpan
                        (text "not an expression:" <+> quotes (text expr))

-- | Find the kind of a type
hscKcType
  :: GhcMonad m =>
     HscEnv
  -> String			-- ^ The type
  -> m Kind

hscKcType hsc_env str = do
    ty <- hscParseType (hsc_dflags hsc_env) str
    let icontext = hsc_IC hsc_env
    ioMsgMaybe $ tcRnType hsc_env icontext ty

#endif
\end{code}

\begin{code}
#ifdef GHCI
hscParseStmt :: GhcMonad m => DynFlags -> String -> m (Maybe (LStmt RdrName))
hscParseStmt = hscParseThing parseStmt

hscParseType :: GhcMonad m => DynFlags -> String -> m (LHsType RdrName)
hscParseType = hscParseThing parseType
#endif

hscParseIdentifier :: GhcMonad m => DynFlags -> String -> m (Located RdrName)
hscParseIdentifier = hscParseThing parseIdentifier

hscParseThing :: (Outputable thing, GhcMonad m)
	      => Lexer.P thing
	      -> DynFlags -> String
	      -> m thing
	-- Nothing => Parse error (message already printed)
	-- Just x  => success
hscParseThing parser dflags str
 = (liftIO $ showPass dflags "Parser") >>
      {-# SCC "Parser" #-} do

      buf <- liftIO $ stringToStringBuffer str

      let loc  = mkSrcLoc (fsLit "<interactive>") 1 1

      case unP parser (mkPState dflags buf loc) of

	PFailed span err -> do
          let msg = mkPlainErrMsg span err
          throw (mkSrcErr (unitBag msg))

	POk pst thing -> do

          let ms@(warns, errs) = getMessages pst
          logWarnings warns
          when (errorsFound dflags ms) $ -- handle -Werror
            throw (mkSrcErr errs)

          --ToDo: can't free the string buffer until we've finished this
          -- compilation sweep and all the identifiers have gone away.
          liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr thing)
          return thing
\end{code}

%************************************************************************
%*									*
	Desugar, simplify, convert to bytecode, and link an expression
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
compileExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO HValue

compileExpr hsc_env srcspan ds_expr
  | rtsIsProfiled
  = throwIO (InstallationError "You can't call compileExpr in a profiled compiler")
    	  -- Otherwise you get a seg-fault when you run it

  | otherwise
  = do	{ let { dflags  = hsc_dflags hsc_env ;
		lint_on = dopt Opt_DoCoreLinting dflags }
	      
		-- Simplify it
	; simpl_expr <- simplifyExpr dflags ds_expr

		-- Tidy it (temporary, until coreSat does cloning)
	; let tidy_expr = tidyExpr emptyTidyEnv simpl_expr

		-- Prepare for codegen
	; prepd_expr <- corePrepExpr dflags tidy_expr

		-- Lint if necessary
		-- ToDo: improve SrcLoc
	; if lint_on then 
                let ictxt = hsc_IC hsc_env
                    tyvars = varSetElems (ic_tyvars ictxt)
                in
		case lintUnfolding noSrcLoc tyvars prepd_expr of
		   Just err -> pprPanic "compileExpr" err
		   Nothing  -> return ()
	  else
		return ()

		-- Convert to BCOs
	; bcos <- coreExprToBCOs dflags prepd_expr

		-- link it
	; hval <- linkExpr hsc_env srcspan bcos

	; return hval
     }
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
