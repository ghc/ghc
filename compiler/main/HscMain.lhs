%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%

\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module HscMain
    ( newHscEnv, hscCmmFile
    , hscParseIdentifier
    , hscSimplify
    , evalComp
    , hscNormalIface, hscWriteIface, hscOneShot
    , CompState (..)
#ifdef GHCI
    , hscStmt, hscTcExpr, hscKcType
    , compileExpr
#endif
    , hscCompileOneShot     -- :: Compiler HscStatus
    , hscCompileBatch       -- :: Compiler (HscStatus, ModIface, ModDetails)
    , hscCompileNothing     -- :: Compiler (HscStatus, ModIface, ModDetails)
    , hscCompileInteractive -- :: Compiler (InteractiveStatus, ModIface, ModDetails)
    , HscStatus (..)
    , InteractiveStatus (..)

    -- The new interface
    , parseFile
    , typecheckModule'
    , typecheckRenameModule
    , deSugarModule
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
import SrcLoc		( SrcSpan, noSrcLoc, interactiveSrcLoc, srcLocSpan, noSrcSpan )
import VarSet
import VarEnv		( emptyTidyEnv )
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
import StgSyn
import CostCentre
import TyCon		( isDataTyCon )
import Name		( Name, NamedThing(..) )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
import Cmm              ( Cmm )
import CmmParse		( parseCmmFile )
import CmmCPS
import CmmCPSZ
import CmmInfo
import OptimizationFuel ( initOptFuelState )
import CmmCvt
import CmmTx
import CmmContFlowOpt
import CodeOutput	( codeOutput )
import NameEnv          ( emptyNameEnv )

import DynFlags
import ErrUtils
import UniqSupply	( mkSplitUniqSupply )

import Outputable
import HscStats		( ppSourceStats )
import HscTypes
import MkExternalCore	( emitExternalCore )
import FastString
import LazyUniqFM		( emptyUFM )
import UniqSupply       ( initUs_ )
import Bag		( unitBag, emptyBag, unionBags )
import Exception
import MonadUtils

import Control.Monad
import System.Exit
import System.IO
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
                           hsc_type_env_var = Nothing,
                           hsc_global_rdr_env = emptyGlobalRdrEnv,
                           hsc_global_type_env = emptyNameEnv } ) }
			

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
parseFile :: GhcMonad m => HscEnv -> ModSummary -> m (Located (HsModule RdrName))
parseFile hsc_env mod_summary = do
    maybe_parsed <- liftIO $ myParseModule dflags hspp_file hspp_buf
    case maybe_parsed of
      Left err -> do throw (mkSrcErr (unitBag err))
      Right rdr_module
               -> return rdr_module
  where
           dflags    = hsc_dflags hsc_env
           hspp_file = ms_hspp_file mod_summary
           hspp_buf  = ms_hspp_buf  mod_summary

-- | Rename and typecheck a module
typecheckModule' :: GhcMonad m =>
                   HscEnv -> ModSummary -> Located (HsModule RdrName)
                -> m TcGblEnv
typecheckModule' hsc_env mod_summary rdr_module = do
      r <- {-# SCC "Typecheck-Rename" #-}
           ioMsgMaybe $ tcRnModule hsc_env (ms_hsc_src mod_summary) False rdr_module
      return r

-- XXX: should this really be a Maybe X?  Check under which circumstances this
-- can become a Nothing and decide whether this should instead throw an
-- exception/signal an error.
type RenamedStuff = 
        (Maybe (HsGroup Name, [LImportDecl Name], Maybe [LIE Name],
                Maybe (HsDoc Name), HaddockModInfo Name))

-- | Rename and typecheck a module, additionally returning the renamed syntax
typecheckRenameModule
    :: GhcMonad m =>
       HscEnv -> ModSummary -> Located (HsModule RdrName)
    -> m (TcGblEnv, RenamedStuff)
typecheckRenameModule hsc_env mod_summary rdr_module = do
    tc_result
          <- {-# SCC "Typecheck-Rename" #-}
             ioMsgMaybe $ tcRnModule hsc_env (ms_hsc_src mod_summary) True rdr_module

    let rn_info = do decl <- tcg_rn_decls tc_result
                     imports <- tcg_rn_imports tc_result
                     let exports = tcg_rn_exports tc_result
                     let doc = tcg_doc tc_result
        	     let hmi = tcg_hmi tc_result
                     return (decl,imports,exports,doc,hmi)

    return (tc_result, rn_info)

-- | Convert a typechecked module to Core
deSugarModule :: GhcMonad m => HscEnv -> ModSummary -> TcGblEnv -> m ModGuts
deSugarModule hsc_env mod_summary tc_result = do
    ioMsgMaybe $ deSugar hsc_env (ms_location mod_summary) tc_result

-- | Make a 'ModIface' from the results of typechecking.  Used when
-- not optimising, and the interface doesn't need to contain any
-- unfoldings or other cross-module optimisation info.
-- ToDo: the old interface is only needed to get the version numbers,
-- we should use fingerprint versions instead.
makeSimpleIface :: HscEnv -> Maybe ModIface -> TcGblEnv -> ModDetails
                -> IO (ModIface,Bool)
makeSimpleIface hsc_env maybe_old_iface tc_result details = do
  mkIfaceTc hsc_env (fmap mi_iface_hash maybe_old_iface) details tc_result

-- | Make a 'ModDetails' from the results of typechecking.  Used when
-- typechecking only, as opposed to full compilation.
makeSimpleDetails :: HscEnv -> TcGblEnv -> IO ModDetails
makeSimpleDetails hsc_env tc_result = mkBootModDetailsTc hsc_env tc_result

-- deSugarModule :: HscEnv -> TcGblEnv -> IO Core
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
data HscStatus
    = HscNoRecomp
    | HscRecomp  Bool -- Has stub files.
                      -- This is a hack. We can't compile C files here
                      -- since it's done in DriverPipeline. For now we
                      -- just return True if we want the caller to compile
                      -- them for us.

-- Status of a compilation to byte-code.
data InteractiveStatus
    = InteractiveNoRecomp
    | InteractiveRecomp Bool     -- Same as HscStatus
                        CompiledByteCode
                        ModBreaks


-- I want Control.Monad.State! --Lemmih 03/07/2006
newtype Comp a = Comp {runComp :: CompState -> IORef Messages -> IO (a, CompState)}

instance Monad Comp where
    g >>= fn = Comp $ \s r -> runComp g s r >>= \(a,s') -> runComp (fn a) s' r
    return a = Comp $ \s _ -> return (a,s)
    fail = error

evalComp :: Comp a -> CompState -> IO (Messages, a)
evalComp comp st = do r <- newIORef emptyMessages
                      (val,_st') <- runComp comp st r
                      msgs <- readIORef r
                      return (msgs, val)

logMsgs :: Messages -> Comp ()
logMsgs (warns', errs') = Comp $ \s r -> do
                           (warns, errs) <- readIORef r
                           writeIORef r $! ( warns' `unionBags` warns
                                           , errs' `unionBags` errs )
                           return ((), s)

data CompState
    = CompState
    { compHscEnv     :: HscEnv
    , compModSummary :: ModSummary
    , compOldIface   :: Maybe ModIface
    }

get :: Comp CompState
get = Comp $ \s _ -> return (s,s)

modify :: (CompState -> CompState) -> Comp ()
modify f = Comp $ \s _ -> return ((), f s)

gets :: (CompState -> a) -> Comp a
gets getter = do st <- get
                 return (getter st)

instance MonadIO Comp where
  liftIO ioA = Comp $ \s _ -> do a <- ioA; return (a,s)

type NoRecomp result = ModIface -> Comp result

-- FIXME: The old interface and module index are only using in 'batch' and
--        'interactive' mode. They should be removed from 'oneshot' mode.
type Compiler result =  GhcMonad m =>
                        HscEnv
                     -> ModSummary
                     -> Bool                -- True <=> source unchanged
                     -> Maybe ModIface      -- Old interface, if available
                     -> Maybe (Int,Int)     -- Just (i,n) <=> module i of n (for msgs)
                     -> m result

--------------------------------------------------------------
-- Compilers
--------------------------------------------------------------

-- Compile Haskell, boot and extCore in OneShot mode.
hscCompileOneShot :: Compiler HscStatus
hscCompileOneShot hsc_env mod_summary src_changed mb_old_iface mb_i_of_n
  = do
     -- One-shot mode needs a knot-tying mutable variable for interface files.
     -- See TcRnTypes.TcGblEnv.tcg_type_env_var.
    type_env_var <- liftIO $ newIORef emptyNameEnv
    let 
       mod = ms_mod mod_summary
       hsc_env' = hsc_env{ hsc_type_env_var = Just (mod, type_env_var) }
    ---
    hscCompilerOneShot' hsc_env' mod_summary src_changed mb_old_iface mb_i_of_n

hscCompilerOneShot' :: Compiler HscStatus
hscCompilerOneShot'
   = hscCompiler norecompOneShot oneShotMsg (genComp backend boot_backend)
   where
     backend inp  = hscSimplify inp >>= hscNormalIface >>= hscWriteIface >>= hscOneShot
     boot_backend inp = hscSimpleIface inp >>= hscWriteIface >> return (Just (HscRecomp False))

-- Compile Haskell, boot and extCore in batch mode.
hscCompileBatch :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileBatch
   = hscCompiler norecompBatch batchMsg (genComp backend boot_backend)
   where
     backend inp  = hscSimplify inp >>= hscNormalIface >>= hscWriteIface >>= hscBatch
     boot_backend inp = hscSimpleIface inp >>= hscWriteIface >>= hscNothing

-- Compile Haskell, extCore to bytecode.
hscCompileInteractive :: Compiler (InteractiveStatus, ModIface, ModDetails)
hscCompileInteractive
   = hscCompiler norecompInteractive batchMsg (genComp backend boot_backend)
   where
     backend inp = hscSimplify inp >>= hscNormalIface >>= hscIgnoreIface >>= hscInteractive
     boot_backend _ = panic "hscCompileInteractive: HsBootFile"

-- Type-check Haskell and .hs-boot only (no external core)
hscCompileNothing :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileNothing
   = hscCompiler norecompBatch batchMsg comp
   where
     backend tc = hscSimpleIface tc >>= hscIgnoreIface >>= hscNothing

     comp = do   -- genComp doesn't fit here, because we want to omit
                 -- desugaring and for the backend to take a TcGblEnv
        mod_summary <- gets compModSummary
        case ms_hsc_src mod_summary of
           ExtCoreFile -> panic "hscCompileNothing: cannot do external core"
           _other -> do
                mb_tc <- hscFileFrontEnd
                case mb_tc of
                  Nothing -> return Nothing
                  Just tc_result -> backend tc_result
        
hscCompiler
        :: NoRecomp result                       -- No recomp necessary
        -> (Maybe (Int,Int) -> Bool -> Comp ())  -- Message callback
        -> Comp (Maybe result)
        -> Compiler result
hscCompiler norecomp messenger recomp hsc_env mod_summary 
            source_unchanged mbOldIface mbModIndex
   = ioMsgMaybe $
      flip evalComp (CompState hsc_env mod_summary mbOldIface) $
      do (recomp_reqd, mbCheckedIface)
             <- {-# SCC "checkOldIface" #-}
                liftIO $ checkOldIface hsc_env mod_summary
                              source_unchanged mbOldIface
	 -- save the interface that comes back from checkOldIface.
	 -- In one-shot mode we don't have the old iface until this
	 -- point, when checkOldIface reads it from the disk.
	 modify (\s -> s{ compOldIface = mbCheckedIface })
         case mbCheckedIface of 
           Just iface | not recomp_reqd
               -> do messenger mbModIndex False
                     result <- norecomp iface
                     return (Just result)
           _otherwise
               -> do messenger mbModIndex True
                     recomp

-- the usual way to build the Comp (Maybe result) to pass to hscCompiler
genComp :: (ModGuts  -> Comp (Maybe a))
        -> (TcGblEnv -> Comp (Maybe a))
        -> Comp (Maybe a)
genComp backend boot_backend = do
    mod_summary <- gets compModSummary
    case ms_hsc_src mod_summary of
       ExtCoreFile -> do
          panic "GHC does not currently support reading External Core files"
       _not_core -> do
          mb_tc <- hscFileFrontEnd
          case mb_tc of
            Nothing -> return Nothing
            Just tc_result -> 
              case ms_hsc_src mod_summary of
                HsBootFile -> boot_backend tc_result
                _other     -> do
                  mb_guts <- hscDesugar tc_result
                  case mb_guts of
                    Nothing -> return Nothing
                    Just guts -> backend guts

--------------------------------------------------------------
-- NoRecomp handlers
--------------------------------------------------------------

norecompOneShot :: NoRecomp HscStatus
norecompOneShot _old_iface
    = do hsc_env <- gets compHscEnv
         liftIO $ do
         dumpIfaceStats hsc_env
         return HscNoRecomp

norecompBatch :: NoRecomp (HscStatus, ModIface, ModDetails)
norecompBatch = norecompWorker HscNoRecomp False

norecompInteractive :: NoRecomp (InteractiveStatus, ModIface, ModDetails)
norecompInteractive = norecompWorker InteractiveNoRecomp True

norecompWorker :: a -> Bool -> NoRecomp (a, ModIface, ModDetails)
norecompWorker a _isInterp old_iface
    = do hsc_env <- gets compHscEnv
         liftIO $ do
         new_details <- {-# SCC "tcRnIface" #-}
                        initIfaceCheck hsc_env $
                        typecheckIface old_iface
         dumpIfaceStats hsc_env
         return (a, old_iface, new_details)

--------------------------------------------------------------
-- Progress displayers.
--------------------------------------------------------------

oneShotMsg :: Maybe (Int,Int) -> Bool -> Comp ()
oneShotMsg _mb_mod_index recomp
    = do hsc_env <- gets compHscEnv
         liftIO $ do
         if recomp
            then return ()
            else compilationProgressMsg (hsc_dflags hsc_env) $
                     "compilation IS NOT required"

batchMsg :: Maybe (Int,Int) -> Bool -> Comp ()
batchMsg mb_mod_index recomp
    = do hsc_env <- gets compHscEnv
         mod_summary <- gets compModSummary
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
hscFileFrontEnd :: Comp (Maybe TcGblEnv)
hscFileFrontEnd =
    do hsc_env <- gets compHscEnv
       mod_summary <- gets compModSummary

             -------------------
             -- PARSE
             -------------------
       let dflags = hsc_dflags hsc_env
           hspp_file = ms_hspp_file mod_summary
           hspp_buf  = ms_hspp_buf  mod_summary
       maybe_parsed <- liftIO $ myParseModule dflags hspp_file hspp_buf
       case maybe_parsed of
         Left err
             -> do logMsgs (emptyBag, unitBag err)
                   return Nothing
         Right rdr_module
             -------------------
             -- RENAME and TYPECHECK
             -------------------
             -> do (tc_msgs, maybe_tc_result) 
                       <- {-# SCC "Typecheck-Rename" #-}
                          liftIO $ tcRnModule hsc_env (ms_hsc_src mod_summary)
                                              False rdr_module
                   logMsgs tc_msgs
                   return maybe_tc_result

--------------------------------------------------------------
-- Desugaring
--------------------------------------------------------------

hscDesugar :: TcGblEnv -> Comp (Maybe ModGuts)
hscDesugar tc_result
  = do mod_summary <- gets compModSummary
       hsc_env <- gets compHscEnv

          -------------------
          -- DESUGAR
          -------------------
       (msgs, ds_result)
           <- {-# SCC "DeSugar" #-}
              liftIO $ deSugar hsc_env (ms_location mod_summary) tc_result
       logMsgs msgs
       return ds_result

--------------------------------------------------------------
-- Simplifiers
--------------------------------------------------------------

hscSimplify :: ModGuts -> Comp ModGuts
hscSimplify ds_result
  = do hsc_env <- gets compHscEnv
       liftIO $ do
           -------------------
           -- SIMPLIFY
           -------------------
       simpl_result <- {-# SCC "Core2Core" #-}
                       core2core hsc_env ds_result
       return simpl_result

--------------------------------------------------------------
-- Interface generators
--------------------------------------------------------------

-- HACK: we return ModGuts even though we know it's not gonna be used.
--       We do this because the type signature needs to be identical
--       in structure to the type of 'hscNormalIface'.
hscSimpleIface :: TcGblEnv -> Comp (ModIface, Bool, ModDetails, TcGblEnv)
hscSimpleIface tc_result
  = do hsc_env <- gets compHscEnv
       maybe_old_iface <- gets compOldIface
       liftIO $ do
       details <- mkBootModDetailsTc hsc_env tc_result
       (new_iface, no_change)
           <- {-# SCC "MkFinalIface" #-}
              mkIfaceTc hsc_env (fmap mi_iface_hash maybe_old_iface) details tc_result
       -- And the answer is ...
       dumpIfaceStats hsc_env
       return (new_iface, no_change, details, tc_result)

hscNormalIface :: ModGuts -> Comp (ModIface, Bool, ModDetails, CgGuts)
hscNormalIface simpl_result
  = do hsc_env <- gets compHscEnv
       _mod_summary <- gets compModSummary
       maybe_old_iface <- gets compOldIface
       liftIO $ do
 	    -------------------
 	    -- TIDY
 	    -------------------
       (cg_guts, details) <- {-# SCC "CoreTidy" #-}
                             tidyProgram hsc_env simpl_result

 	    -------------------
	    -- BUILD THE NEW ModIface and ModDetails
	    --	and emit external core if necessary
	    -- This has to happen *after* code gen so that the back-end
	    -- info has been set.  Not yet clear if it matters waiting
	    -- until after code output
       (new_iface, no_change)
		<- {-# SCC "MkFinalIface" #-}
		   mkIface hsc_env (fmap mi_iface_hash maybe_old_iface)
                         details simpl_result
	-- Emit external core
       -- This should definitely be here and not after CorePrep,
       -- because CorePrep produces unqualified constructor wrapper declarations,
       -- so its output isn't valid External Core (without some preprocessing).
       emitExternalCore (hsc_dflags hsc_env) cg_guts 
       dumpIfaceStats hsc_env

 	    -------------------
 	    -- Return the prepared code.
       return (new_iface, no_change, details, cg_guts)

--------------------------------------------------------------
-- BackEnd combinators
--------------------------------------------------------------

hscWriteIface :: (ModIface, Bool, ModDetails, a) -> Comp (ModIface, ModDetails, a)
hscWriteIface (iface, no_change, details, a)
    = do mod_summary <- gets compModSummary
         hsc_env <- gets compHscEnv
         let dflags = hsc_dflags hsc_env
         liftIO $ do
         unless no_change
           $ writeIfaceFile dflags (ms_location mod_summary) iface
         return (iface, details, a)

hscIgnoreIface :: (ModIface, Bool, ModDetails, a) -> Comp (ModIface, ModDetails, a)
hscIgnoreIface (iface, _no_change, details, a)
    = return (iface, details, a)

-- Don't output any code.
hscNothing :: (ModIface, ModDetails, a) -> Comp (Maybe (HscStatus, ModIface, ModDetails))
hscNothing (iface, details, _)
    = return (Just (HscRecomp False, iface, details))

-- Generate code and return both the new ModIface and the ModDetails.
hscBatch :: (ModIface, ModDetails, CgGuts) -> Comp (Maybe (HscStatus, ModIface, ModDetails))
hscBatch (iface, details, cgguts)
    = do hasStub <- hscCompile cgguts
         return (Just (HscRecomp hasStub, iface, details))

-- Here we don't need the ModIface and ModDetails anymore.
hscOneShot :: (ModIface, ModDetails, CgGuts) -> Comp (Maybe HscStatus)
hscOneShot (_, _, cgguts)
    = do hasStub <- hscCompile cgguts
         return (Just (HscRecomp hasStub))

-- Compile to hard-code.
hscCompile :: CgGuts -> Comp Bool
hscCompile cgguts
    = do hsc_env <- gets compHscEnv
         mod_summary <- gets compModSummary
         liftIO $ do
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
         cmms <- {-# SCC "CodeGen" #-}
                      codeGen dflags this_mod data_tycons
                              dir_imps cost_centre_info
                              stg_binds hpc_info
         --- Optionally run experimental Cmm transformations ---
         cmms <- optionallyConvertAndOrCPS hsc_env cmms
                 -- unless certain dflags are on, the identity function
         ------------------  Code output -----------------------
         rawcmms <- cmmToRawCmm cmms
         (_stub_h_exists, stub_c_exists)
             <- codeOutput dflags this_mod location foreign_stubs 
                dependencies rawcmms
         return stub_c_exists

hscInteractive :: (ModIface, ModDetails, CgGuts)
               -> Comp (Maybe (InteractiveStatus, ModIface, ModDetails))
#ifdef GHCI
hscInteractive (iface, details, cgguts)
    = do hsc_env <- gets compHscEnv
         mod_summary <- gets compModSummary
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
         return (Just (InteractiveRecomp istub_c_exists comp_bc mod_breaks, iface, details))
#else
hscInteractive _ = panic "GHC not compiled with interpreter"
#endif

------------------------------

hscCmmFile :: GhcMonad m => HscEnv -> FilePath -> m ()
hscCmmFile hsc_env filename = do
    dflags <- return $ hsc_dflags hsc_env
    cmm <- ioMsgMaybe $
             parseCmmFile dflags filename
    cmms <- liftIO $ optionallyConvertAndOrCPS hsc_env [cmm]
    rawCmms <- liftIO $ cmmToRawCmm cmms
    liftIO $ codeOutput dflags no_mod no_loc NoStubs [] rawCmms
    return ()
  where
	no_mod = panic "hscCmmFile: no_mod"
	no_loc = ModLocation{ ml_hs_file  = Just filename,
                              ml_hi_file  = panic "hscCmmFile: no hi file",
                              ml_obj_file = panic "hscCmmFile: no obj file" }

optionallyConvertAndOrCPS :: HscEnv -> [Cmm] -> IO [Cmm]
optionallyConvertAndOrCPS hsc_env cmms =
    do let dflags = hsc_dflags hsc_env
        --------  Optionally convert to and from zipper ------
       cmms <- if dopt Opt_ConvertToZipCfgAndBack dflags
               then mapM (testCmmConversion hsc_env) cmms
               else return cmms
         ---------  Optionally convert to CPS (MDA) -----------
       cmms <- if not (dopt Opt_ConvertToZipCfgAndBack dflags) &&
                  dopt Opt_RunCPSZ dflags
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
       cps_zgraph <- protoCmmCPSZ hsc_env zgraph
       let chosen_graph = if dopt Opt_RunCPSZ dflags then cps_zgraph else zgraph
       dumpIfSet_dyn dflags Opt_D_dump_cmmz "C-- Zipper Graph" (ppr chosen_graph)
       showPass dflags "Convert from Z back to Cmm"
       let cvt = cmmOfZgraph $ cfopts $ chosen_graph
       dumpIfSet_dyn dflags Opt_D_dump_cvt_cmm "C-- post-conversion" (ppr cvt)
       return cvt
       -- return cmm -- don't use the conversion

myParseModule :: DynFlags -> FilePath -> Maybe StringBuffer
              -> IO (Either ErrMsg (Located (HsModule RdrName)))
myParseModule dflags src_filename maybe_src_buf
 =    --------------------------  Parser  ----------------
      showPass dflags "Parser" >>
      {-# SCC "Parser" #-} do

	-- sometimes we already have the buffer in memory, perhaps
	-- because we needed to parse the imports out of it, or get the 
	-- module name.
      buf <- case maybe_src_buf of
		Just b  -> return b
		Nothing -> hGetStringBuffer src_filename

      let loc  = mkSrcLoc (mkFastString src_filename) 1 0

      case unP parseModule (mkPState buf loc dflags) of {

	PFailed span err -> return (Left (mkPlainErrMsg span err));

	POk pst rdr_module -> do {

      let {ms = getMessages pst};
      printErrorsAndWarnings dflags ms; -- XXX
      when (errorsFound dflags ms) $ exitWith (ExitFailure 1);
      
      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;
      
      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;
      
      return (Right rdr_module)
	-- ToDo: free the string buffer later.
      }}


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

      let loc  = mkSrcLoc (fsLit "<interactive>") 1 0

      case unP parser (mkPState buf loc dflags) of

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

