%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%
\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module HscMain ( hscMain ) where

#include "HsVersions.h"

import Monad		( when )
import IO		( hPutStr, hClose, stderr, openFile, IOMode(..) )
import HsSyn

import RdrHsSyn		( RdrNameHsModule )
import FastString	( unpackFS )
import StringBuffer	( hGetStringBuffer )
import Parser		( parse )
import Lex		( PState(..), ParseResult(..) )
import SrcLoc		( mkSrcLoc )

import Rename		( renameModule, checkOldIface )

import PrelInfo		( wiredInThings )
import PrelRules	( builtinRules )
import MkIface		( completeIface, mkModDetailsFromIface )
import TcModule		( TcResults(..), typecheckModule )
import Desugar		( deSugar )
import SimplCore	( core2core )
import OccurAnal	( occurAnalyseBinds )
import CoreUtils	( coreBindsSize )
import CoreTidy		( tidyCorePgm )
import CoreToStg	( topCoreBindsToStg )
import StgSyn		( collectFinalStgBinders )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
import CodeOutput	( codeOutput )

import Module		( ModuleName, moduleNameUserString, 
			  moduleUserString, moduleName )
import CmdLineOpts
import ErrUtils		( ghcExit, doIfSet, dumpIfSet )
import UniqSupply	( mkSplitUniqSupply )

import Bag		( emptyBag )
import Outputable
import Char		( isSpace )
import StgInterp	( stgToInterpSyn )
import HscStats		( ppSourceStats )
import HscTypes		( ModDetails, ModIface, PersistentCompilerState(..),
			  PersistentRenamerState(..), WhatsImported(..),
			  HomeSymbolTable, PackageSymbolTable, ImportVersion, 
			  GenAvailInfo(..), RdrAvailInfo, OrigNameEnv(..),
			  PackageRuleBase )
import RnMonad		( ExportItem, ParsedIface(..) )
import CmSummarise	( ModSummary )
import InterpSyn	( UnlinkedIBind )
import StgInterp	( ItblEnv )
import FiniteMap	( FiniteMap, plusFM, emptyFM, addToFM )
import OccName		( OccName, pprOccName )
import Name		( Name, nameModule )
\end{code}


%************************************************************************
%*									*
\subsection{The main compiler pipeline}
%*									*
%************************************************************************

\begin{code}
data HscResult
   = HscOK   ModDetails  	     -- new details (HomeSymbolTable additions)
	     (Maybe ModIface)	     -- new iface (if any compilation was done)
	     (Maybe String)  	     -- generated stub_h filename (in /tmp)
	     (Maybe String)  	     -- generated stub_c filename (in /tmp)
	     (Maybe ([UnlinkedIBind],ItblEnv)) -- interpreted code, if any
             PersistentCompilerState -- updated PCS

   | HscFail PersistentCompilerState -- updated PCS
	-- no errors or warnings; the individual passes
	-- (parse/rename/typecheck) print messages themselves

hscMain
  :: DynFlags	
  -> ModSummary       -- summary, including source filename
  -> Maybe ModIface   -- old interface, if available
  -> String	      -- file in which to put the output (.s, .hc, .java etc.)
  -> HomeSymbolTable		-- for home module ModDetails
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> IO HscResult

hscMain dflags core_cmds stg_cmds summary maybe_old_iface
	output_filename mod_details pcs
 = do {
      -- ????? source_unchanged :: Bool -- extracted from summary?

      (ch_pcs, check_errs, (recomp_reqd, maybe_checked_iface))
         <- checkOldIface dflags finder hit hst pcs mod source_unchanged
                          maybe_old_iface;
      if check_errs then
         return (HscFail ch_pcs)
      else do {

      let no_old_iface = not (isJust maybe_checked_iface)
          what_next | recomp_reqd || no_old_iface = hscRecomp 
                    | otherwise                   = hscNoRecomp
      ;
      return (what_next dflags finder core_cmds stg_cmds summary hit hst 
                        pcs2 maybe_checked_iface)
      }}


hscNoRecomp dflags finder core_cmds stg_cmds summary hit hst pcs maybe_old_iface
 = do {
      -- we definitely expect to have the old interface available
      let old_iface = case maybe_old_iface of 
                         Just old_if -> old_if
                         Nothing -> panic "hscNoRecomp:old_iface"
      ;
      -- CLOSURE
      (pcs_cl, closure_errs, cl_hs_decls) 
         <- closeIfaceDecls dflags finder hit hst pcs old_iface ;
      if closure_errs then 
         return (HscFail cl_pcs) 
      else do {

      -- TYPECHECK
      maybe_tc_result
         <- typecheckModule dflags mod pcs_cl hst hit pit cl_hs_decls;
      case maybe_tc_result of {
         Nothing -> return (HscFail cl_pcs);
         Just tc_result -> do {

      let pcs_tc        = tc_pcs tc_result
          env_tc        = tc_env tc_result
          binds_tc      = tc_binds tc_result
          local_insts   = tc_insts tc_result
          local_rules   = tc_rules tc_result
      ;
      -- create a new details from the closed, typechecked, old iface
      let new_details = mkModDetailsFromIface env_tc local_insts local_rules
      ;
      return (HscOK final_details
		    Nothing -- tells CM to use old iface and linkables
		    Nothing Nothing -- foreign export stuff
                    Nothing -- ibinds
		    pcs_tc)
      }}}}


hscRecomp dflags finder core_cmds stg_cmds summary hit hst pcs maybe_old_iface
 = do {
      -- what target are we shooting for?
      let toInterp = dopt_HscLang dflags == HscInterpreted
      ;
      -- PARSE
      maybe_parsed <- myParseModule dflags summary;
      case maybe_parsed of {
         Nothing -> return (HscFail pcs);
         Just rdr_module -> do {

      -- RENAME
      (pcs_rn, maybe_rn_result) 
         <- renameModule dflags finder hit hst pcs mod rdr_module;
      case maybe_rn_result of {
         Nothing -> return (HscFail pcs_rn);
         Just (new_iface, rn_hs_decls) -> do {

      -- TYPECHECK
      maybe_tc_result
         <- typecheckModule dflags mod pcs_rn hst hit pit rn_hs_decls;
      case maybe_tc_result of {
         Nothing -> return (HscFail pcs_rn);
         Just tc_result -> do {

      let pcs_tc        = tc_pcs tc_result
          env_tc        = tc_env tc_result
          binds_tc      = tc_binds tc_result
          local_insts   = tc_insts tc_result
      ;
      -- DESUGAR, SIMPLIFY, TIDY-CORE
      -- We grab the the unfoldings at this point.
      (tidy_binds, orphan_rules, foreign_stuff)
         <- dsThenSimplThenTidy dflags mod tc_result ds_uniqs
      ;
      -- CONVERT TO STG
      (stg_binds, cost_centre_info, top_level_ids) 
         <- myCoreToStg finder c2s_uniqs st_uniqs this_mod tidy_binds
      ;
      -- cook up a new ModDetails now we (finally) have all the bits
      let new_details = mkModDetails tc_env local_insts tidy_binds 
			             top_level_ids orphan_rules
      ;
      -- and possibly create a new ModIface
      let maybe_final_iface = completeIface maybe_old_iface new_iface new_details 
      ;

      -- Write the interface file
      writeIface finder maybe_final_iface
      ;

      -- do the rest of code generation/emission
      (maybe_ibinds, maybe_stub_h_filename, maybe_stub_c_filename) 
         <- restOfCodeGeneration toInterp
                                 this_mod imported_modules cost_centre_info 
                                 fe_binders tc_env stg_binds
      ;
      -- and the answer is ...
      return (HscOK new_details maybe_final_iface 
		    maybe_stub_h_filename maybe_stub_c_filename
                    maybe_ibinds pcs_tc)
      }}}}}}}


myParseModule dflags summary
 = do --------------------------  Reader  ----------------
      show_pass "Parser"
      -- _scc_     "Parser"

      let src_filename -- name of the preprocessed source file
            = case ms_ppsource summary of
                 Just (filename, fingerprint) -> filename
                 Nothing -> pprPanic 
                               "myParseModule:summary is not of a source module"
                               (ppr summary)

      buf <- hGetStringBuffer True{-expand tabs-} src_filename

      let glaexts | dopt Opt_GlasgowExts dflags = 1#
	          | otherwise 		      = 0#

      case parse buf PState{ bol = 0#, atbol = 1#,
	 		     context = [], glasgow_exts = glaexts,
			     loc = mkSrcLoc src_filename 1 } of {

	PFailed err -> do { hPutStrLn stderr (showSDoc err);
                            return Nothing };
	POk _ rdr_module@(HsModule mod_name _ _ _ _ _ _) ->

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module)
      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module)

      return (Just rdr_module)
      }


restOfCodeGeneration toInterp this_mod imported_modules cost_centre_info 
                     foreign_stuff tc_env stg_binds
 | toInterp
 = return (Nothing, Nothing, 
	   Just (stgToInterpSyn stg_binds local_tycons local_classes))
 | otherwise
 = do --------------------------  Code generation -------------------------------
      show_pass "CodeGen"
      -- _scc_     "CodeGen"
      abstractC <- codeGen this_mod imported_modules
                           cost_centre_info fe_binders
                           local_tycons local_classes stg_binds

      --------------------------  Code output -------------------------------
      show_pass "CodeOutput"
      -- _scc_     "CodeOutput"
      let (fe_binders, h_code, c_code) = foreign_stuff
      (maybe_stub_h_name, maybe_stub_c_name)
         <- codeOutput this_mod local_tycons local_classes
                       occ_anal_tidy_binds stg_binds2
                       c_code h_code abstractC ncg_uniqs

      return (maybe_stub_h_name, maybe_stub_c_name, Nothing)
 where
    local_tycons  = tcEnvTyCons tc_env
    local_classes = tcEnvClasses tc_env


dsThenSimplThenTidy dflags mod tc_result
-- make up ds_uniqs here
 = do --------------------------  Desugaring ----------------
      -- _scc_     "DeSugar"
      (desugared, rules, h_code, c_code, fe_binders) 
         <- deSugar this_mod ds_uniqs tc_result

      --------------------------  Main Core-language transformations ----------------
      -- _scc_     "Core2Core"
      (simplified, orphan_rules)  <- core2core core_cmds desugared rules

      -- Do the final tidy-up
      (tidy_binds, tidy_orphan_rules) 
         <- tidyCorePgm this_mod simplified orphan_rules
      
      return (tidy_binds, tidy_orphan_rules, (fe_binders,h_code,c_code))


myCoreToStg c2s_uniqs st_uniqs this_mod tidy_binds
 = do let occ_anal_tidy_binds = occurAnalyseBinds tidy_binds

      () <- coreBindsSize occ_anal_tidy_binds `seq` return ()
      -- TEMP: the above call zaps some space usage allocated by the
      -- simplifier, which for reasons I don't understand, persists
      -- thoroughout code generation

      show_pass "Core2Stg"
      -- _scc_     "Core2Stg"
      let stg_binds   = topCoreBindsToStg c2s_uniqs occ_anal_tidy_binds

      show_pass "Stg2Stg"
      -- _scc_     "Stg2Stg"
      (stg_binds2, cost_centre_info) <- stg2stg stg_cmds this_mod st_uniqs stg_binds
      let final_ids = collectFinalStgBinders (map fst stg_binds2)

      return (stg_binds2, cost_centre_info, final_ids)
\end{code}


%************************************************************************
%*									*
\subsection{Initial persistent state}
%*									*
%************************************************************************

\begin{code}
initPersistentCompilerState :: IO PersistentCompilerState
initPersistentCompilerState 
  = do prs <- initPersistentRenamerState
       return (
        PCS { pcs_PST   = initPackageDetails,
	      pcs_insts = emptyInstEnv,
	      pcs_rules = emptyRuleEnv,
	      pcs_PRS   = prs
            }
        )

initPackageDetails :: PackageSymbolTable
initPackageDetails = extendTypeEnv emptyModuleEnv wiredInThings

initPersistentRenamerState :: IO PersistentRenamerState
  = do ns <- mkSplitUniqSupply 'r'
       return (
        PRS { prsOrig  = Orig { origNames  = initOrigNames,
			        origIParam = emptyFM },
	      prsDecls = emptyNameEnv,
	      prsInsts = emptyBag,
	      prsRules = emptyBag,
	      prsNS    = ns
            }
        )

initOrigNames :: FiniteMap (ModuleName,OccName) Name
initOrigNames = grab knownKeyNames `plusFM` grab (map getName wiredInThings)
	      where
		grab names   = foldl add emptyFM names
		add env name = addToFM env (moduleName (nameModule name), nameOccName name) name


initRules :: PackageRuleBase
initRules = foldl add emptyVarEnv builtinRules
	  where
	    add env (name,rule) = extendNameEnv_C add1 env name [rule]
	    add1 rules _	= rule : rules
\end{code}
