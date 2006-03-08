%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%

\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module HscMain
    ( newHscEnv, hscCmmFile
    , hscFileCheck
    , hscParseIdentifier
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
    , HscChecked (..)
    ) where

#include "HsVersions.h"

#ifdef GHCI
import HsSyn		( Stmt(..), LHsExpr, LStmt, LHsType )
import Module		( Module )
import CodeOutput	( outputForeignStubs )
import ByteCodeGen	( byteCodeGen, coreExprToBCOs )
import Linker		( HValue, linkExpr )
import CoreTidy		( tidyExpr )
import CorePrep		( corePrepExpr )
import Flattening	( flattenExpr )
import Desugar          ( deSugarExpr )
import SimplCore        ( simplifyExpr )
import TcRnDriver	( tcRnStmt, tcRnExpr, tcRnType ) 
import Type		( Type )
import PrelNames	( iNTERACTIVE )
import Kind		( Kind )
import CoreLint		( lintUnfolding )
import DsMeta		( templateHaskellNames )
import SrcLoc		( noSrcLoc )
import VarEnv		( emptyTidyEnv )
#endif

import Var		( Id )
import Module		( emptyModuleEnv, ModLocation(..) )
import RdrName		( GlobalRdrEnv, RdrName )
import HsSyn		( HsModule, LHsBinds, HsGroup, LIE, LImportDecl )
import SrcLoc		( Located(..) )
import StringBuffer	( hGetStringBuffer, stringToStringBuffer )
import Parser
import Lexer		( P(..), ParseResult(..), mkPState )
import SrcLoc		( mkSrcLoc )
import TcRnDriver	( tcRnModule, tcRnExtCore )
import TcIface		( typecheckIface )
import TcRnMonad	( initIfaceCheck, TcGblEnv(..) )
import IfaceEnv		( initNameCache )
import LoadIface	( ifaceStats, initExternalPackageState )
import PrelInfo		( wiredInThings, basicKnownKeyNames )
import MkIface		( checkOldIface, mkIface, writeIfaceFile )
import Desugar          ( deSugar )
import Flattening       ( flatten )
import SimplCore        ( core2core )
import TidyPgm		( tidyProgram, mkBootModDetails )
import CorePrep		( corePrepPgm )
import CoreToStg	( coreToStg )
import TyCon		( isDataTyCon )
import Packages		( mkHomeModules )
import Name		( Name, NamedThing(..) )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
import CmmParse		( parseCmmFile )
import CodeOutput	( codeOutput )

import DynFlags
import ErrUtils
import UniqSupply	( mkSplitUniqSupply )

import Outputable
import HscStats		( ppSourceStats )
import HscTypes
import MkExternalCore	( emitExternalCore )
import ParserCore
import ParserCoreUtils
import FastString
import Maybes		( expectJust )
import Bag		( unitBag )
import Monad		( unless )
import IO
import DATA_IOREF	( newIORef, readIORef )
\end{code}


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
	; fc_var  <- newIORef emptyModuleEnv
	; return (HscEnv { hsc_dflags = dflags,
			   hsc_targets = [],
			   hsc_mod_graph = [],
			   hsc_IC     = emptyInteractiveContext,
			   hsc_HPT    = emptyHomePackageTable,
			   hsc_EPS    = eps_var,
			   hsc_NC     = nc_var,
			   hsc_FC     = fc_var } ) }
			

knownKeyNames :: [Name]	-- Put here to avoid loops involving DsMeta,
			-- where templateHaskellNames are defined
knownKeyNames = map getName wiredInThings 
	      ++ basicKnownKeyNames
#ifdef GHCI
	      ++ templateHaskellNames
#endif
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

data HscChecked
    = HscChecked
        -- parsed
        (Located (HsModule RdrName))
        -- renamed
        (Maybe (HsGroup Name,[LImportDecl Name],Maybe [LIE Name]))
        -- typechecked
        (Maybe (LHsBinds Id, GlobalRdrEnv, ModDetails))


-- Status of a compilation to hard-code or nothing.
data HscStatus
    = HscNoRecomp
    | HscRecomp  Bool -- Has stub files.
                      -- This is a hack. We can't compile C files here
                      -- since it's done in DriverPipeline. For now we
                      -- just return True if we want the caller to compile
                      -- it for us.

-- Status of a compilation to byte-code.
data InteractiveStatus
    = InteractiveNoRecomp
    | InteractiveRecomp Bool     -- Same as HscStatus
                        CompiledByteCode


-- I want Control.Monad.State! --Lemmih 03/07/2006
newtype Comp a = Comp {runComp :: CompState -> IO (a, CompState)}

instance Monad Comp where
    g >>= fn = Comp $ \s -> runComp g s >>= \(a,s') -> runComp (fn a) s'
    return a = Comp $ \s -> return (a,s)
    fail = error

evalComp :: Comp a -> CompState -> IO a
evalComp comp st = do (val,_st') <- runComp comp st
                      return val

data CompState
    = CompState
    { compHscEnv     :: HscEnv
    , compModSummary :: ModSummary
    , compOldIface   :: Maybe ModIface
    }

get :: Comp CompState
get = Comp $ \s -> return (s,s)

gets :: (CompState -> a) -> Comp a
gets getter = do st <- get
                 return (getter st)

liftIO :: IO a -> Comp a
liftIO ioA = Comp $ \s -> do a <- ioA
                             return (a,s)

type NoRecomp result = ModIface -> Comp result
type FrontEnd core = Comp (Maybe core)

-- FIXME: The old interface and module index are only using in 'batch' and
--        'interactive' mode. They should be removed from 'oneshot' mode.
type Compiler result =  HscEnv
                     -> ModSummary
                     -> Bool                -- True <=> source unchanged
                     -> Maybe ModIface      -- Old interface, if available
                     -> Maybe (Int,Int)     -- Just (i,n) <=> module i of n (for msgs)
                     -> IO (Maybe result)


-- This functions checks if recompilation is necessary and
-- then combines the FrontEnd and BackEnd to a working compiler.
hscMkCompiler :: NoRecomp result         -- What to do when recompilation isn't required.
              -> (Maybe (Int,Int) -> Bool -> Comp ())
              -> FrontEnd core
              -> (core -> Comp result)   -- Backend.
              -> Compiler result
hscMkCompiler norecomp messenger frontend backend
              hsc_env mod_summary source_unchanged
              mbOldIface mbModIndex
    = flip evalComp (CompState hsc_env mod_summary mbOldIface) $
      do (recomp_reqd, mbCheckedIface)
             <- {-# SCC "checkOldIface" #-}
                liftIO $ checkOldIface hsc_env mod_summary
                              source_unchanged mbOldIface
         case mbCheckedIface of 
           Just iface | not recomp_reqd
               -> do messenger mbModIndex False
                     result <- norecomp iface
                     return (Just result)
           _otherwise
               -> do messenger mbModIndex True
                     mbCore <- frontend
                     case mbCore of
                       Nothing
                           -> return Nothing
                       Just core
                           -> do result <- backend core
                                 return (Just result)

--------------------------------------------------------------
-- Compilers
--------------------------------------------------------------

--        1         2         3         4         5         6         7         8          9
-- Compile Haskell, boot and extCore in OneShot mode.
hscCompileOneShot :: Compiler HscStatus
hscCompileOneShot hsc_env mod_summary =
    compiler hsc_env mod_summary
    where mkComp = hscMkCompiler (norecompOneShot HscNoRecomp) oneShotMsg
          -- How to compile nonBoot files.
          nonBootComp inp = hscSimplify inp >>= hscNormalIface >>=
                            hscWriteIface >>= hscOneShot
          -- How to compile boot files.
          bootComp inp = hscSimpleIface inp >>= hscWriteIface >>= hscConst (HscRecomp False)
          compiler
              = case ms_hsc_src mod_summary of
                ExtCoreFile
                    -> mkComp hscCoreFrontEnd nonBootComp
                HsSrcFile
                    -> mkComp hscFileFrontEnd nonBootComp
                HsBootFile
                    -> mkComp hscFileFrontEnd bootComp

-- Compile Haskell, boot and extCore in batch mode.
hscCompileBatch :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileBatch hsc_env mod_summary
    = compiler hsc_env mod_summary
    where mkComp = hscMkCompiler norecompBatch (batchMsg False)
          nonBootComp inp = hscSimplify inp >>= hscNormalIface >>=
                            hscWriteIface >>= hscBatch
          bootComp inp = hscSimpleIface inp >>= hscWriteIface >>= hscNothing
          compiler
              = case ms_hsc_src mod_summary of
                ExtCoreFile
                    -> mkComp hscCoreFrontEnd nonBootComp
                HsSrcFile
                    -> mkComp hscFileFrontEnd nonBootComp
                HsBootFile
                    -> mkComp hscFileFrontEnd bootComp

-- Type-check Haskell, boot and extCore.
hscCompileNothing :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileNothing hsc_env mod_summary
    = compiler hsc_env mod_summary
    where mkComp = hscMkCompiler norecompBatch (batchMsg False)
          pipeline inp = hscSimpleIface inp >>= hscIgnoreIface >>= hscNothing
          compiler
              = case ms_hsc_src mod_summary of
                ExtCoreFile
                    -> mkComp hscCoreFrontEnd pipeline
                HsSrcFile
                    -> mkComp hscFileFrontEnd pipeline
                HsBootFile
                    -> mkComp hscFileFrontEnd pipeline

-- Compile Haskell, extCore to bytecode.
hscCompileInteractive :: Compiler (InteractiveStatus, ModIface, ModDetails)
hscCompileInteractive hsc_env mod_summary =
    hscMkCompiler norecompInteractive (batchMsg True)
                  frontend backend
                  hsc_env mod_summary
    where backend inp = hscSimplify inp >>= hscNormalIface >>= hscIgnoreIface >>= hscInteractive
          frontend = case ms_hsc_src mod_summary of
                       ExtCoreFile -> hscCoreFrontEnd
                       HsSrcFile   -> hscFileFrontEnd
                       HsBootFile  -> panic bootErrorMsg
          bootErrorMsg = "Compiling a HsBootFile to bytecode doesn't make sense. " ++
                         "Use 'hscCompileBatch' instead."

--------------------------------------------------------------
-- NoRecomp handlers
--------------------------------------------------------------

norecompOneShot :: a -> NoRecomp a
norecompOneShot a old_iface
    = do hsc_env <- gets compHscEnv
         liftIO $ do
         dumpIfaceStats hsc_env
         return a

norecompBatch :: NoRecomp (HscStatus, ModIface, ModDetails)
norecompBatch = norecompWorker HscNoRecomp False

norecompInteractive :: NoRecomp (InteractiveStatus, ModIface, ModDetails)
norecompInteractive = norecompWorker InteractiveNoRecomp True

norecompWorker :: a -> Bool -> NoRecomp (a, ModIface, ModDetails)
norecompWorker a isInterp old_iface
    = do hsc_env <- gets compHscEnv
         mod_summary <- gets compModSummary
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

batchMsg :: Bool -> Maybe (Int,Int) -> Bool -> Comp ()
batchMsg toInterp mb_mod_index recomp
    = do hsc_env <- gets compHscEnv
         mod_summary <- gets compModSummary
         let showMsg msg = compilationProgressMsg (hsc_dflags hsc_env) $
                           (showModuleIndex mb_mod_index ++
                            msg ++ showModMsg (not toInterp) mod_summary)
         liftIO $ do
         if recomp
            then showMsg "Compiling "
            else showMsg "Skipping  "



--------------------------------------------------------------
-- FrontEnds
--------------------------------------------------------------

hscCoreFrontEnd :: FrontEnd ModGuts
hscCoreFrontEnd =
    do hsc_env <- gets compHscEnv
       mod_summary <- gets compModSummary
       liftIO $ do
            -------------------
            -- PARSE
            -------------------
       inp <- readFile (expectJust "hscCoreFrontEnd" (ms_hspp_file mod_summary))
       case parseCore inp 1 of
         FailP s
             -> do errorMsg (hsc_dflags hsc_env) (text s{-ToDo: wrong-})
                   return Nothing
         OkP rdr_module
             -------------------
             -- RENAME and TYPECHECK
             -------------------
             -> do (tc_msgs, maybe_tc_result) <- {-# SCC "TypeCheck" #-}
                                                 tcRnExtCore hsc_env rdr_module
                   printErrorsAndWarnings (hsc_dflags hsc_env) tc_msgs
                   case maybe_tc_result of
                     Nothing       -> return Nothing
                     Just mod_guts -> return (Just mod_guts)         -- No desugaring to do!

	 
hscFileFrontEnd :: FrontEnd ModGuts
hscFileFrontEnd =
    do hsc_env <- gets compHscEnv
       mod_summary <- gets compModSummary
       liftIO $ do
             -------------------
             -- PARSE
             -------------------
       let dflags = hsc_dflags hsc_env
           hspp_file = expectJust "hscFileFrontEnd" (ms_hspp_file mod_summary)
           hspp_buf  = ms_hspp_buf  mod_summary
       maybe_parsed <- myParseModule dflags hspp_file hspp_buf
       case maybe_parsed of
         Left err
             -> do printBagOfErrors dflags (unitBag err)
                   return Nothing
         Right rdr_module
             -------------------
             -- RENAME and TYPECHECK
             -------------------
             -> do (tc_msgs, maybe_tc_result) 
                       <- {-# SCC "Typecheck-Rename" #-}
                          tcRnModule hsc_env (ms_hsc_src mod_summary) False rdr_module
                   printErrorsAndWarnings dflags tc_msgs
                   case maybe_tc_result of
                     Nothing
                         -> return Nothing
                     Just tc_result
                         -------------------
                         -- DESUGAR
                         -------------------
                         -> do (warns, maybe_ds_result) <- {-# SCC "DeSugar" #-}
                                                           deSugar hsc_env tc_result
                               printBagOfWarnings dflags warns
                               return maybe_ds_result

--------------------------------------------------------------
-- Simplifiers
--------------------------------------------------------------

hscSimplify :: ModGuts -> Comp ModGuts
hscSimplify ds_result
  = do hsc_env <- gets compHscEnv
       liftIO $ do
       flat_result <- {-# SCC "Flattening" #-}
                      flatten hsc_env ds_result
           -------------------
           -- SIMPLIFY
           -------------------
       simpl_result <- {-# SCC "Core2Core" #-}
                       core2core hsc_env flat_result
       return simpl_result

--------------------------------------------------------------
-- Interface generators
--------------------------------------------------------------

-- HACK: we return ModGuts even though we know it's not gonna be used.
--       We do this because the type signature needs to be identical
--       in structure to the type of 'hscNormalIface'.
hscSimpleIface :: ModGuts -> Comp (ModIface, Bool, ModDetails, ModGuts)
hscSimpleIface ds_result
  = do hsc_env <- gets compHscEnv
       mod_summary <- gets compModSummary
       maybe_old_iface <- gets compOldIface
       liftIO $ do
       details <- mkBootModDetails hsc_env ds_result
       (new_iface, no_change) 
           <- {-# SCC "MkFinalIface" #-}
              mkIface hsc_env maybe_old_iface ds_result details
       -- And the answer is ...
       dumpIfaceStats hsc_env
       return (new_iface, no_change, details, ds_result)

hscNormalIface :: ModGuts -> Comp (ModIface, Bool, ModDetails, CgGuts)
hscNormalIface simpl_result
  = do hsc_env <- gets compHscEnv
       mod_summary <- gets compModSummary
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
		   mkIface hsc_env maybe_old_iface simpl_result details
	-- Emit external core
       emitExternalCore (hsc_dflags hsc_env) cg_guts -- Move this? --Lemmih 03/07/2006

 	    -------------------
 	    -- Return the prepared code.
       return (new_iface, no_change, details, cg_guts)

--------------------------------------------------------------
-- BackEnd combinators
--------------------------------------------------------------

hscWriteIface :: (ModIface, Bool, ModDetails, a) -> Comp (ModIface, ModDetails, a)
hscWriteIface (iface, no_change, details, a)
    = do mod_summary <- gets compModSummary
         liftIO $ do
         unless no_change
           $ writeIfaceFile (ms_location mod_summary) iface
         return (iface, details, a)

hscIgnoreIface :: (ModIface, Bool, ModDetails, a) -> Comp (ModIface, ModDetails, a)
hscIgnoreIface (iface, no_change, details, a)
    = return (iface, details, a)

-- Don't output any code.
hscNothing :: (ModIface, ModDetails, a) -> Comp (HscStatus, ModIface, ModDetails)
hscNothing (iface, details, a)
    = return (HscRecomp False, iface, details)

-- Generate code and return both the new ModIface and the ModDetails.
hscBatch :: (ModIface, ModDetails, CgGuts) -> Comp (HscStatus, ModIface, ModDetails)
hscBatch (iface, details, cgguts)
    = do hasStub <- hscCompile cgguts
         return (HscRecomp hasStub, iface, details)

-- Here we don't need the ModIface and ModDetails anymore.
hscOneShot :: (ModIface, ModDetails, CgGuts) -> Comp HscStatus
hscOneShot (_, _, cgguts)
    = do hasStub <- hscCompile cgguts
         return (HscRecomp hasStub)

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
                     cg_home_mods = home_mods,
                     cg_dep_pkgs = dependencies } = cgguts
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
                myCoreToStg dflags home_mods this_mod prepd_binds	
         ------------------  Code generation ------------------
         abstractC <- {-# SCC "CodeGen" #-}
                      codeGen dflags home_mods this_mod data_tycons
                              foreign_stubs dir_imps cost_centre_info
                              stg_binds
         ------------------  Code output -----------------------
         (stub_h_exists,stub_c_exists)
             <- codeOutput dflags this_mod location foreign_stubs 
                dependencies abstractC
         return stub_c_exists

hscConst :: b -> a -> Comp b
hscConst b a = return b

hscInteractive :: (ModIface, ModDetails, CgGuts)
               -> Comp (InteractiveStatus, ModIface, ModDetails)
hscInteractive (iface, details, cgguts)
#ifdef GHCI
    = do hsc_env <- gets compHscEnv
         mod_summary <- gets compModSummary
         liftIO $ do
         let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                     -- From now on, we just use the bits we need.
                     cg_module   = this_mod,
                     cg_binds    = core_binds,
                     cg_tycons   = tycons,
                     cg_foreign  = foreign_stubs } = cgguts
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
         comp_bc <- byteCodeGen dflags prepd_binds data_tycons
         ------------------ Create f-x-dynamic C-side stuff ---
         (istub_h_exists, istub_c_exists) 
             <- outputForeignStubs dflags this_mod location foreign_stubs
         return (InteractiveRecomp istub_c_exists comp_bc, iface, details)
#else
    = panic "GHC not compiled with interpreter"
#endif

------------------------------

hscFileCheck :: HscEnv -> ModSummary -> IO (Maybe HscChecked)
hscFileCheck hsc_env mod_summary = do {
 	    -------------------
 	    -- PARSE
 	    -------------------
	; let dflags    = hsc_dflags hsc_env
	      hspp_file = expectJust "hscFileFrontEnd" (ms_hspp_file mod_summary)
	      hspp_buf  = ms_hspp_buf  mod_summary

	; maybe_parsed <- myParseModule dflags hspp_file hspp_buf

	; case maybe_parsed of {
      	     Left err -> do { printBagOfErrors dflags (unitBag err)
			    ; return Nothing } ;
      	     Right rdr_module -> do {

 	    -------------------
 	    -- RENAME and TYPECHECK
 	    -------------------
	  (tc_msgs, maybe_tc_result) 
		<- _scc_ "Typecheck-Rename" 
		   tcRnModule hsc_env (ms_hsc_src mod_summary) 
			True{-save renamed syntax-}
			rdr_module

	; printErrorsAndWarnings dflags tc_msgs
	; case maybe_tc_result of {
      	     Nothing -> return (Just (HscChecked rdr_module Nothing Nothing));
      	     Just tc_result -> do
		let md = ModDetails { 
				md_types   = tcg_type_env tc_result,
				md_exports = tcg_exports  tc_result,
				md_insts   = tcg_insts    tc_result,
				md_rules   = [panic "no rules"] }
				   -- Rules are CoreRules, not the
				   -- RuleDecls we get out of the typechecker
                    rnInfo = do decl <- tcg_rn_decls tc_result
                                imports <- tcg_rn_imports tc_result
                                let exports = tcg_rn_exports tc_result
                                return (decl,imports,exports)
		return (Just (HscChecked rdr_module 
                                   rnInfo
				   (Just (tcg_binds tc_result,
					  tcg_rdr_env tc_result,
					  md))))
	}}}}


hscCmmFile :: DynFlags -> FilePath -> IO Bool
hscCmmFile dflags filename = do
  maybe_cmm <- parseCmmFile dflags (mkHomeModules []) filename
  case maybe_cmm of
    Nothing -> return False
    Just cmm -> do
	codeOutput dflags no_mod no_loc NoStubs [] [cmm]
	return True
  where
	no_mod = panic "hscCmmFile: no_mod"
	no_loc = ModLocation{ ml_hs_file  = Just filename,
                              ml_hi_file  = panic "hscCmmFile: no hi file",
                              ml_obj_file = panic "hscCmmFile: no obj file" }


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

	POk _ rdr_module -> do {

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;
      
      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;
      
      return (Right rdr_module)
	-- ToDo: free the string buffer later.
      }}


myCoreToStg dflags home_mods this_mod prepd_binds
 = do 
      stg_binds <- {-# SCC "Core2Stg" #-}
	     coreToStg home_mods prepd_binds

      (stg_binds2, cost_centre_info) <- {-# SCC "Stg2Stg" #-}
	     stg2stg dflags home_mods this_mod stg_binds

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
  -> IO (Maybe (HscEnv, [Name], HValue))

hscStmt hsc_env stmt
  = do	{ maybe_stmt <- hscParseStmt (hsc_dflags hsc_env) stmt
	; case maybe_stmt of {
      	     Nothing	  -> return Nothing ;	-- Parse error
      	     Just Nothing -> return Nothing ;	-- Empty line
      	     Just (Just parsed_stmt) -> do {	-- The real stuff

		-- Rename and typecheck it
	  let icontext = hsc_IC hsc_env
	; maybe_tc_result <- tcRnStmt hsc_env icontext parsed_stmt

	; case maybe_tc_result of {
		Nothing -> return Nothing ;
		Just (new_ic, bound_names, tc_expr) -> do {

		-- Then desugar, code gen, and link it
	; hval <- compileExpr hsc_env iNTERACTIVE 
			      (ic_rn_gbl_env new_ic) 
			      (ic_type_env new_ic)
			      tc_expr

	; return (Just (hsc_env{ hsc_IC=new_ic }, bound_names, hval))
	}}}}}

hscTcExpr	-- Typecheck an expression (but don't run it)
  :: HscEnv
  -> String			-- The expression
  -> IO (Maybe Type)

hscTcExpr hsc_env expr
  = do	{ maybe_stmt <- hscParseStmt (hsc_dflags hsc_env) expr
	; let icontext = hsc_IC hsc_env
	; case maybe_stmt of {
	     Nothing      -> return Nothing ;	-- Parse error
	     Just (Just (L _ (ExprStmt expr _ _)))
			-> tcRnExpr hsc_env icontext expr ;
	     Just other -> do { errorMsg (hsc_dflags hsc_env) (text "not an expression:" <+> quotes (text expr)) ;
			        return Nothing } ;
      	     } }

hscKcType	-- Find the kind of a type
  :: HscEnv
  -> String			-- The type
  -> IO (Maybe Kind)

hscKcType hsc_env str
  = do	{ maybe_type <- hscParseType (hsc_dflags hsc_env) str
	; let icontext = hsc_IC hsc_env
	; case maybe_type of {
	     Just ty	-> tcRnType hsc_env icontext ty ;
	     Just other -> do { errorMsg (hsc_dflags hsc_env) (text "not an type:" <+> quotes (text str)) ;
			        return Nothing } ;
      	     Nothing    -> return Nothing } }
#endif
\end{code}

\begin{code}
#ifdef GHCI
hscParseStmt :: DynFlags -> String -> IO (Maybe (Maybe (LStmt RdrName)))
hscParseStmt = hscParseThing parseStmt

hscParseType :: DynFlags -> String -> IO (Maybe (LHsType RdrName))
hscParseType = hscParseThing parseType
#endif

hscParseIdentifier :: DynFlags -> String -> IO (Maybe (Located RdrName))
hscParseIdentifier = hscParseThing parseIdentifier

hscParseThing :: Outputable thing
	      => Lexer.P thing
	      -> DynFlags -> String
	      -> IO (Maybe thing)
	-- Nothing => Parse error (message already printed)
	-- Just x  => success
hscParseThing parser dflags str
 = showPass dflags "Parser" >>
      {-# SCC "Parser" #-} do

      buf <- stringToStringBuffer str

      let loc  = mkSrcLoc FSLIT("<interactive>") 1 0

      case unP parser (mkPState buf loc dflags) of {

	PFailed span err -> do { printError span err;
                                 return Nothing };

	POk _ thing -> do {

      --ToDo: can't free the string buffer until we've finished this
      -- compilation sweep and all the identifiers have gone away.
      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr thing);
      return (Just thing)
      }}
\end{code}

%************************************************************************
%*									*
	Desugar, simplify, convert to bytecode, and link an expression
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
compileExpr :: HscEnv 
	    -> Module -> GlobalRdrEnv -> TypeEnv
	    -> LHsExpr Id
	    -> IO HValue

compileExpr hsc_env this_mod rdr_env type_env tc_expr
  = do	{ let { dflags  = hsc_dflags hsc_env ;
		lint_on = dopt Opt_DoCoreLinting dflags }
	      
	 	-- Desugar it
	; ds_expr <- deSugarExpr hsc_env this_mod rdr_env type_env tc_expr
	
		-- Flatten it
	; flat_expr <- flattenExpr hsc_env ds_expr

		-- Simplify it
	; simpl_expr <- simplifyExpr dflags flat_expr

		-- Tidy it (temporary, until coreSat does cloning)
	; let tidy_expr = tidyExpr emptyTidyEnv simpl_expr

		-- Prepare for codegen
	; prepd_expr <- corePrepExpr dflags tidy_expr

		-- Lint if necessary
		-- ToDo: improve SrcLoc
	; if lint_on then 
		case lintUnfolding noSrcLoc [] prepd_expr of
		   Just err -> pprPanic "compileExpr" err
		   Nothing  -> return ()
	  else
		return ()

		-- Convert to BCOs
	; bcos <- coreExprToBCOs dflags prepd_expr

		-- link it
	; hval <- linkExpr hsc_env bcos

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
showModuleIndex Nothing = ""
showModuleIndex (Just (i,n)) = "[" ++ padded ++ " of " ++ n_str ++ "] "
    where
        n_str = show n
        i_str = show i
        padded = replicate (length n_str - length i_str) ' ' ++ i_str
\end{code}

