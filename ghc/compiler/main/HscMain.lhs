%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%
\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module HscMain ( HscResult(..), hscMain, 
		 initPersistentCompilerState ) where

#include "HsVersions.h"

import Maybe		( isJust )
import IO		( hPutStrLn, stderr )
import HsSyn

import StringBuffer	( hGetStringBuffer )
import Parser
import Lex		( PState(..), ParseResult(..) )
import SrcLoc		( mkSrcLoc )

import Rename		( renameModule, checkOldIface, closeIfaceDecls )
import Rules		( emptyRuleBase )
import PrelInfo		( wiredInThingEnv, wiredInThings )
import PrelNames	( knownKeyNames )
import MkIface		( completeIface, mkModDetailsFromIface, mkModDetails,
			  writeIface, pprIface )
import TcModule		( TcResults(..), typecheckModule )
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

import Module		( ModuleName, moduleName, mkModuleInThisPackage )
import CmdLineOpts
import ErrUtils		( dumpIfSet_dyn, showPass )
import Util		( unJust )
import UniqSupply	( mkSplitUniqSupply )

import Bag		( emptyBag )
import Outputable
import Interpreter	( UnlinkedIBind, ItblEnv, stgToInterpSyn )
import HscStats		( ppSourceStats )
import HscTypes		( ModDetails, ModIface(..), PersistentCompilerState(..),
			  PersistentRenamerState(..), ModuleLocation(..),
			  HomeSymbolTable, 
			  OrigNameEnv(..), PackageRuleBase, HomeIfaceTable, 
			  typeEnvClasses, typeEnvTyCons, emptyIfaceTable )
import FiniteMap	( FiniteMap, plusFM, emptyFM, addToFM )
import OccName		( OccName )
import Name		( Name, nameModule, nameOccName, getName  )
import Name		( emptyNameEnv )
import Module		( Module, lookupModuleEnvByName )

import Monad		( when )
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
  -> Bool			-- source unchanged?
  -> ModuleLocation		-- location info
  -> Maybe ModIface		-- old interface, if available
  -> HomeSymbolTable		-- for home module ModDetails
  -> HomeIfaceTable
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> IO HscResult

hscMain dflags source_unchanged location maybe_old_iface hst hit pcs
 = do {
      putStrLn ("CHECKING OLD IFACE for hs = " ++ show (ml_hs_file location)
                ++ ", hspp = " ++ show (ml_hspp_file location));

      (pcs_ch, errs_found, (recomp_reqd, maybe_checked_iface))
         <- checkOldIface dflags hit hst pcs (unJust (ml_hi_file location) "hscMain")
			  source_unchanged maybe_old_iface;

      if errs_found then
         return (HscFail pcs_ch)
      else do {

      let no_old_iface = not (isJust maybe_checked_iface)
          what_next | recomp_reqd || no_old_iface = hscRecomp 
                    | otherwise                   = hscNoRecomp
      ;
      what_next dflags location maybe_checked_iface
                hst hit pcs_ch
      }}


hscNoRecomp dflags location maybe_checked_iface hst hit pcs_ch
 = do {
      hPutStrLn stderr "COMPILATION NOT REQUIRED";
      -- we definitely expect to have the old interface available
      let old_iface = case maybe_checked_iface of 
                         Just old_if -> old_if
                         Nothing -> panic "hscNoRecomp:old_iface"
          this_mod = mi_module old_iface
      ;
      -- CLOSURE
      (pcs_cl, closure_errs, cl_hs_decls) 
         <- closeIfaceDecls dflags hit hst pcs_ch old_iface ;
      if closure_errs then 
         return (HscFail pcs_cl) 
      else do {

      -- TYPECHECK
      maybe_tc_result <- typecheckModule dflags this_mod pcs_cl hst 
					 old_iface alwaysQualify cl_hs_decls;
      case maybe_tc_result of {
         Nothing -> return (HscFail pcs_cl);
         Just tc_result -> do {

      let pcs_tc      = tc_pcs tc_result
          env_tc      = tc_env tc_result
          local_insts = tc_insts tc_result
          local_rules = tc_rules tc_result
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


hscRecomp dflags location maybe_checked_iface hst hit pcs_ch
 = do	{
      	; hPutStrLn stderr "COMPILATION IS REQUIRED";

      	  -- what target are we shooting for?
      	; let toInterp = dopt_HscLang dflags == HscInterpreted

 	    -------------------
 	    -- PARSE
 	    -------------------
	; maybe_parsed <- myParseModule dflags (unJust (ml_hspp_file location) "hscRecomp:hspp")
	; case maybe_parsed of {
      	     Nothing -> return (HscFail pcs_ch);
      	     Just rdr_module -> do {
	; let this_mod = mkModuleInThisPackage (hsModuleName rdr_module)
    
 	    -------------------
 	    -- RENAME
 	    -------------------
	; (pcs_rn, maybe_rn_result) 
      	     <- renameModule dflags hit hst pcs_ch this_mod rdr_module
      	; case maybe_rn_result of {
      	     Nothing -> return (HscFail pcs_rn);
      	     Just (print_unqualified, is_exported, new_iface, rn_hs_decls) -> do {
    
 	    -------------------
 	    -- TYPECHECK
 	    -------------------
	; maybe_tc_result <- typecheckModule dflags this_mod pcs_rn hst new_iface 
					     print_unqualified rn_hs_decls
	; case maybe_tc_result of {
      	     Nothing -> do { hPutStrLn stderr "Typecheck failed" 
 			   ; return (HscFail pcs_rn) } ;
      	     Just tc_result -> do {
    
	; let pcs_tc        = tc_pcs tc_result
      	      env_tc        = tc_env tc_result
      	      local_insts   = tc_insts tc_result

 	    -------------------
 	    -- DESUGAR, SIMPLIFY, TIDY-CORE
 	    -------------------
      	  -- We grab the the unfoldings at this point.
	; simpl_result <- dsThenSimplThenTidy dflags (pcs_rules pcs_tc) this_mod 
 					      print_unqualified is_exported tc_result hst
	; let (tidy_binds, orphan_rules, foreign_stuff) = simpl_result
      	    
 	    -------------------
 	    -- CONVERT TO STG
 	    -------------------
	; (stg_binds, oa_tidy_binds, cost_centre_info, top_level_ids) 
      	     <- myCoreToStg dflags this_mod tidy_binds


 	    -------------------
 	    -- BUILD THE NEW ModDetails AND ModIface
 	    -------------------
	; let new_details = mkModDetails env_tc local_insts tidy_binds 
 					 top_level_ids orphan_rules
	; final_iface <- mkFinalIface dflags location maybe_checked_iface 
				      new_iface new_details

 	    -------------------
 	    -- COMPLETE CODE GENERATION
 	    -------------------
	; (maybe_stub_h_filename, maybe_stub_c_filename, maybe_ibinds)
      	     <- restOfCodeGeneration dflags toInterp this_mod
 		   (map ideclName (hsModuleImports rdr_module))
      		   cost_centre_info foreign_stuff env_tc stg_binds oa_tidy_binds
      		   hit (pcs_PIT pcs_tc)       

      	  -- and the answer is ...
	; return (HscOK new_details (Just final_iface)
 			maybe_stub_h_filename maybe_stub_c_filename
      			maybe_ibinds pcs_tc)
      	  }}}}}}}



mkFinalIface dflags location maybe_old_iface new_iface new_details
 = case completeIface maybe_old_iface new_iface new_details of
      (new_iface, Nothing) -- no change in the interfacfe
         -> do when (dopt Opt_D_dump_hi_diffs dflags)
                    (printDump (text "INTERFACE UNCHANGED"))
               dumpIfSet_dyn dflags Opt_D_dump_hi
                             "UNCHANGED FINAL INTERFACE" (pprIface new_iface)
	       return new_iface
      (new_iface, Just sdoc_diffs)
         -> do dumpIfSet_dyn dflags Opt_D_dump_hi_diffs "INTERFACE HAS CHANGED" sdoc_diffs
               dumpIfSet_dyn dflags Opt_D_dump_hi "NEW FINAL INTERFACE" (pprIface new_iface)
               -- Write the interface file
               writeIface (unJust (ml_hi_file location) "hscRecomp:hi") new_iface
               return new_iface


myParseModule dflags src_filename
 = do --------------------------  Parser  ----------------
      showPass dflags "Parser"
      -- _scc_     "Parser"

      buf <- hGetStringBuffer True{-expand tabs-} src_filename

      let glaexts | dopt Opt_GlasgowExts dflags = 1#
	          | otherwise 		      = 0#

      case parse buf PState{ bol = 0#, atbol = 1#,
	 		     context = [], glasgow_exts = glaexts,
			     loc = mkSrcLoc (_PK_ src_filename) 1 } of {

	PFailed err -> do { hPutStrLn stderr (showSDoc err);
                            return Nothing };

	POk _ (PModule rdr_module@(HsModule mod_name _ _ _ _ _ _)) -> do {

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;
      
      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;
      
      return (Just rdr_module)
      }}


restOfCodeGeneration dflags toInterp this_mod imported_module_names cost_centre_info 
                     foreign_stuff env_tc stg_binds oa_tidy_binds
                     hit pit -- these last two for mapping ModNames to Modules
 | toInterp
 = do (ibinds,itbl_env) 
         <- stgToInterpSyn (map fst stg_binds) local_tycons local_classes
      return (Nothing, Nothing, Just (ibinds,itbl_env))

 | otherwise
 = do --------------------------  Code generation -------------------------------
      -- _scc_     "CodeGen"
      abstractC <- codeGen dflags this_mod imported_modules
                           cost_centre_info fe_binders
                           local_tycons stg_binds

      --------------------------  Code output -------------------------------
      -- _scc_     "CodeOutput"
      (maybe_stub_h_name, maybe_stub_c_name)
         <- codeOutput dflags this_mod local_tycons
                       oa_tidy_binds stg_binds
                       c_code h_code abstractC

      return (maybe_stub_h_name, maybe_stub_c_name, Nothing)
 where
    local_tycons     = typeEnvTyCons env_tc
    local_classes    = typeEnvClasses env_tc
    imported_modules = map mod_name_to_Module imported_module_names
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


dsThenSimplThenTidy dflags rule_base this_mod print_unqual is_exported tc_result hst
 = do --------------------------  Desugaring ----------------
      -- _scc_     "DeSugar"
      (desugared, rules, h_code, c_code, fe_binders) 
         <- deSugar dflags this_mod print_unqual hst tc_result

      --------------------------  Main Core-language transformations ----------------
      -- _scc_     "Core2Core"
      (simplified, orphan_rules) 
         <- core2core dflags rule_base hst is_exported desugared rules

      -- Do the final tidy-up
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

      showPass dflags "Core2Stg"
      -- _scc_     "Core2Stg"
      let stg_binds   = topCoreBindsToStg c2s_uniqs occ_anal_tidy_binds

      showPass dflags "Stg2Stg"
      -- _scc_     "Stg2Stg"
      (stg_binds2, cost_centre_info) <- stg2stg dflags this_mod st_uniqs stg_binds
      let final_ids = collectFinalStgBinders (map fst stg_binds2)

      return (stg_binds2, occ_anal_tidy_binds, cost_centre_info, final_ids)
\end{code}


%************************************************************************
%*									*
\subsection{Compiling an expression}
%*									*
%************************************************************************

hscExpr
  :: DynFlags
  -> HomeSymbolTable	
  -> HomeIfaceTable
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> Module			-- Context for compiling
  -> String			-- The expression
  -> IO HscResult

hscExpr dflags hst hit pcs this_module expr
  = do	{ 	-- Parse it
	; maybe_parsed <- myParseExpr dflags expr
	; case maybe_parsed of {
      	     Nothing -> return (HscFail pcs_ch);
      	     Just parsed_expr -> do {

		-- Rename it
	  (new_pcs, maybe_renamed_expr) <- renameExpr dflags hit hst pcs this_module parsed_expr ;
	; case maybe_renamed_expr of {
		Nothing -> FAIL
		Just renamed_expr -> 

		-- Typecheck it
	  maybe_tc_expr <- typecheckExpr dflags pcs hst unqual renamed_expr 
	; case maybe_tc_expr of
		Nothing -> FAIL
		Just typechecked_expr ->

	


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
        PCS { pcs_PIT   = emptyIfaceTable,
              pcs_PTE   = wiredInThingEnv,
	      pcs_insts = emptyInstEnv,
	      pcs_rules = emptyRuleBase,
	      pcs_PRS   = prs
            }
        )

initPersistentRenamerState :: IO PersistentRenamerState
  = do ns <- mkSplitUniqSupply 'r'
       return (
        PRS { prsOrig  = Orig { origNames  = initOrigNames,
			        origIParam = emptyFM },
	      prsDecls = (emptyNameEnv, 0),
	      prsInsts = (emptyBag, 0),
	      prsRules = (emptyBag, 0),
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
