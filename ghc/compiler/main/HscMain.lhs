%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%
\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module HscMain ( HscResult(..), hscMain, 
		 initPersistentCompilerState ) where

#include "HsVersions.h"

import Maybe		( isJust )
import Monad		( when )
import IO		( hPutStr, hPutStrLn, hClose, stderr, 
			  openFile, IOMode(..) )
import HsSyn

import RdrHsSyn		( RdrNameHsModule )
import FastString	( unpackFS )
import StringBuffer	( hGetStringBuffer )
import Parser		( parse )
import Lex		( PState(..), ParseResult(..) )
import SrcLoc		( mkSrcLoc )

import Rename		( renameModule, checkOldIface, closeIfaceDecls )

import Rules		( emptyRuleBase )
import PrelInfo		( wiredInThings )
import PrelNames	( knownKeyNames )
import PrelRules	( builtinRules )
import MkIface		( completeIface, mkModDetailsFromIface, mkModDetails,
			  writeIface )
import TcModule		( TcResults(..), typecheckModule )
import TcEnv		( tcEnvTyCons, tcEnvClasses )
import InstEnv		( emptyInstEnv )
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
			  moduleUserString, moduleName, emptyModuleEnv,
			  extendModuleEnv )
import CmdLineOpts
import ErrUtils		( ghcExit, doIfSet, dumpIfSet_dyn )
import UniqSupply	( mkSplitUniqSupply )

import Bag		( emptyBag )
import Outputable
import Char		( isSpace )
import StgInterp	( stgToInterpSyn )
import HscStats		( ppSourceStats )
import HscTypes		( ModDetails, ModIface(..), PersistentCompilerState(..),
			  PersistentRenamerState(..), WhatsImported(..),
			  HomeSymbolTable, PackageSymbolTable, ImportVersion, 
			  GenAvailInfo(..), RdrAvailInfo, OrigNameEnv(..),
			  PackageRuleBase, HomeIfaceTable, PackageIfaceTable,
			  extendTypeEnv, groupTyThings, TypeEnv, TyThing,
			  typeEnvClasses, typeEnvTyCons )
import RnMonad		( ExportItem, ParsedIface(..) )
import CmSummarise	( ModSummary(..), name_of_summary, ms_get_imports,
			  mimp_name )
import Finder		( Finder )
import InterpSyn	( UnlinkedIBind )
import StgInterp	( ItblEnv )
import FiniteMap	( FiniteMap, plusFM, emptyFM, addToFM )
import OccName		( OccName, pprOccName )
import Name		( Name, nameModule, emptyNameEnv, nameOccName, 
			  getName, extendNameEnv_C, nameEnvElts )
import VarEnv		( emptyVarEnv )
import Module		( Module, mkModuleName,	lookupModuleEnvByName )

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
  -> Finder
  -> ModSummary       -- summary, including source filename
  -> Maybe ModIface   -- old interface, if available
  -> HomeSymbolTable		-- for home module ModDetails
  -> HomeIfaceTable
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> IO HscResult

hscMain dflags finder summary maybe_old_iface hst hit pcs
 = do {
      -- ????? source_unchanged :: Bool -- extracted from summary?
      let source_unchanged = trace "WARNING: source_unchanged?!" False
      ;
      (pcs_ch, check_errs, (recomp_reqd, maybe_checked_iface))
         <- checkOldIface dflags finder hit hst pcs (ms_mod summary)
			  source_unchanged maybe_old_iface;
      if check_errs then
         return (HscFail pcs_ch)
      else do {

      let no_old_iface = not (isJust maybe_checked_iface)
          what_next | recomp_reqd || no_old_iface = hscRecomp 
                    | otherwise                   = hscNoRecomp
      ;
      what_next dflags finder summary maybe_checked_iface
                hst hit pcs_ch
      }}


hscNoRecomp dflags finder summary maybe_checked_iface hst hit pcs_ch
 = do {
      -- we definitely expect to have the old interface available
      let old_iface = case maybe_checked_iface of 
                         Just old_if -> old_if
                         Nothing -> panic "hscNoRecomp:old_iface"
      ;
      -- CLOSURE
      (pcs_cl, closure_errs, cl_hs_decls) 
         <- closeIfaceDecls dflags finder hit hst pcs_ch old_iface ;
      if closure_errs then 
         return (HscFail pcs_cl) 
      else do {

      -- TYPECHECK
      maybe_tc_result
         <- typecheckModule dflags (ms_mod summary) pcs_cl hst hit cl_hs_decls;
      case maybe_tc_result of {
         Nothing -> return (HscFail pcs_cl);
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
      return (HscOK new_details
		    Nothing -- tells CM to use old iface and linkables
		    Nothing Nothing -- foreign export stuff
                    Nothing -- ibinds
		    pcs_tc)
      }}}}


hscRecomp dflags finder summary maybe_checked_iface hst hit pcs_ch
 = do {
      -- what target are we shooting for?
      let toInterp = dopt_HscLang dflags == HscInterpreted
          this_mod = ms_mod summary
      ;
      -- PARSE
      maybe_parsed <- myParseModule dflags summary;
      case maybe_parsed of {
         Nothing -> return (HscFail pcs_ch);
         Just rdr_module -> do {

      -- RENAME
      show_pass dflags "Renamer";
      (pcs_rn, maybe_rn_result) 
         <- renameModule dflags finder hit hst pcs_ch this_mod rdr_module;
      case maybe_rn_result of {
         Nothing -> return (HscFail pcs_rn);
         Just (new_iface, rn_hs_decls) -> do {

      -- TYPECHECK
      show_pass dflags "Typechecker";
      maybe_tc_result
         <- typecheckModule dflags this_mod pcs_rn hst hit rn_hs_decls;
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
         <- dsThenSimplThenTidy dflags (pcs_rules pcs_tc) this_mod tc_result hst
      ;
      -- CONVERT TO STG
      (stg_binds, oa_tidy_binds, cost_centre_info, top_level_ids) 
         <- myCoreToStg dflags this_mod tidy_binds
      ;
      -- cook up a new ModDetails now we (finally) have all the bits
      let new_details = mkModDetails env_tc local_insts tidy_binds 
			             top_level_ids orphan_rules
      ;
      -- and possibly create a new ModIface
      let maybe_final_iface_and_sdoc 
             = completeIface maybe_checked_iface new_iface new_details 
          maybe_final_iface
             = case maybe_final_iface_and_sdoc of 
                  Just (fif, sdoc) -> Just fif; Nothing -> Nothing
      ;
      -- Write the interface file
      writeIface finder maybe_final_iface
      ;
      -- do the rest of code generation/emission
      (maybe_stub_h_filename, maybe_stub_c_filename, maybe_ibinds)
         <- restOfCodeGeneration dflags toInterp summary
               cost_centre_info foreign_stuff env_tc stg_binds oa_tidy_binds
               hit (pcs_PIT pcs_tc)       
      ;
      -- and the answer is ...
      return (HscOK new_details maybe_final_iface 
		    maybe_stub_h_filename maybe_stub_c_filename
                    maybe_ibinds pcs_tc)
      }}}}}}}


myParseModule dflags summary
 = do --------------------------  Parser  ----------------
      show_pass dflags "Parser"
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
			     loc = mkSrcLoc (_PK_ src_filename) 1 } of {

	PFailed err -> do { hPutStrLn stderr (showSDoc err);
                            return Nothing };
	POk _ rdr_module@(HsModule mod_name _ _ _ _ _ _) -> do {

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;
      
      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;
      
      return (Just rdr_module)
      }}


restOfCodeGeneration dflags toInterp summary cost_centre_info 
                     foreign_stuff env_tc stg_binds oa_tidy_binds
                     hit pit -- these last two for mapping ModNames to Modules
 | toInterp
 = do (ibinds,itbl_env) 
         <- stgToInterpSyn (map fst stg_binds) local_tycons local_classes
      return (Nothing, Nothing, Just (ibinds,itbl_env))
 | otherwise
 = do --------------------------  Code generation -------------------------------
      show_pass dflags "CodeGen"
      -- _scc_     "CodeGen"
      abstractC <- codeGen dflags this_mod imported_modules
                           cost_centre_info fe_binders
                           local_tycons local_classes stg_binds

      --------------------------  Code output -------------------------------
      show_pass dflags "CodeOutput"
      -- _scc_     "CodeOutput"
      ncg_uniqs <- mkSplitUniqSupply 'n'
      (maybe_stub_h_name, maybe_stub_c_name)
         <- codeOutput dflags this_mod local_tycons local_classes
                       oa_tidy_binds stg_binds
                       c_code h_code abstractC ncg_uniqs

      return (maybe_stub_h_name, maybe_stub_c_name, Nothing)
 where
    local_tycons     = typeEnvTyCons env_tc
    local_classes    = typeEnvClasses env_tc
    this_mod         = ms_mod summary
    imported_modules = map (mod_name_to_Module.mimp_name) 
	                   (ms_get_imports summary)
    (fe_binders,h_code,c_code) = foreign_stuff

    mod_name_to_Module :: ModuleName -> Module
    mod_name_to_Module nm
       = let str_mi = case lookupModuleEnvByName hit nm of
                          Just mi -> mi
                          Nothing -> case lookupModuleEnvByName pit nm of
                                        Just mi -> mi
                                        Nothing -> barf nm
         in  mi_module str_mi
    barf nm = pprPanic "mod_name_to_Module: no hst or pst mapping for" 
                       (ppr nm)


dsThenSimplThenTidy dflags rule_base this_mod tc_result hst
 = do --------------------------  Desugaring ----------------
      -- _scc_     "DeSugar"
      show_pass dflags "DeSugar"
      ds_uniqs <- mkSplitUniqSupply 'd'
      (desugared, rules, h_code, c_code, fe_binders) 
         <- deSugar dflags this_mod ds_uniqs hst tc_result

      --------------------------  Main Core-language transformations ----------------
      -- _scc_     "Core2Core"
      show_pass dflags "Core2Core"
      (simplified, orphan_rules) 
         <- core2core dflags rule_base hst desugared rules

      -- Do the final tidy-up
      show_pass dflags "CoreTidy"
      (tidy_binds, tidy_orphan_rules) 
         <- tidyCorePgm dflags this_mod simplified orphan_rules
      
      return (tidy_binds, tidy_orphan_rules, (fe_binders,h_code,c_code))


myCoreToStg dflags this_mod tidy_binds
 = do 
      c2s_uniqs <- mkSplitUniqSupply 'c'
      st_uniqs  <- mkSplitUniqSupply 'g'
      let occ_anal_tidy_binds = occurAnalyseBinds tidy_binds

      () <- coreBindsSize occ_anal_tidy_binds `seq` return ()
      -- TEMP: the above call zaps some space usage allocated by the
      -- simplifier, which for reasons I don't understand, persists
      -- thoroughout code generation

      show_pass dflags "Core2Stg"
      -- _scc_     "Core2Stg"
      let stg_binds   = topCoreBindsToStg c2s_uniqs occ_anal_tidy_binds

      show_pass dflags "Stg2Stg"
      -- _scc_     "Stg2Stg"
      (stg_binds2, cost_centre_info) <- stg2stg dflags this_mod st_uniqs stg_binds
      let final_ids = collectFinalStgBinders (map fst stg_binds2)

      return (stg_binds2, occ_anal_tidy_binds, cost_centre_info, final_ids)


show_pass dflags what
  = if   dopt Opt_D_show_passes dflags
    then hPutStr stderr ("*** "++what++":\n")
    else return ()
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
	      pcs_rules = emptyRuleBase,
	      pcs_PRS   = prs
            }
        )

initPackageDetails :: PackageSymbolTable
initPackageDetails = extendTypeEnv emptyModuleEnv (groupTyThings wiredInThings)

--initPackageDetails = panic "initPackageDetails"

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
initOrigNames 
   = grab knownKeyNames `plusFM` grab (map getName wiredInThings)
     where
        grab names = foldl add emptyFM names
        add env name 
           = addToFM env (moduleName (nameModule name), nameOccName name) name


initRules :: PackageRuleBase
initRules = emptyRuleBase
{- SHOULD BE (ish)
            foldl add emptyVarEnv builtinRules
	  where
	    add env (name,rule) 
              = extendRuleBase env name rule
-}
\end{code}
