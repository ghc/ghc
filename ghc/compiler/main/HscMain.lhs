%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%
\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module HscMain ( HscResult(..), hscMain, 
#ifdef GHCI
		 hscExpr,
#endif
		 initPersistentCompilerState ) where

#include "HsVersions.h"

#ifdef GHCI
import RdrHsSyn		( RdrNameHsExpr )
import Rename		( renameExpr )
import StringBuffer	( stringToStringBuffer, freeStringBuffer )
import Unique		( Uniquable(..) )
import Type		( Type, splitTyConApp_maybe )
import PrelNames	( ioTyConKey )
import ByteCodeGen	( byteCodeGen )
#endif

import HsSyn

import StringBuffer	( hGetStringBuffer )
import Parser
import Lex		( PState(..), ParseResult(..) )
import SrcLoc		( mkSrcLoc )
import Rename		( checkOldIface, renameModule, closeIfaceDecls )
import Rules		( emptyRuleBase )
import PrelInfo		( wiredInThingEnv, wiredInThings )
import PrelNames	( knownKeyNames )
import MkIface		( completeIface, mkModDetailsFromIface, mkModDetails,
			  writeIface, pprIface )
import TcModule
import InstEnv		( emptyInstEnv )
import Desugar
import SimplCore
import CoreUtils	( coreBindsSize )
import CoreTidy		( tidyCorePgm )
import CoreSat
import CoreToStg	( coreToStg )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
import CodeOutput	( codeOutput )

import Module		( ModuleName, moduleName, mkHomeModule )
import CmdLineOpts
import ErrUtils		( dumpIfSet_dyn, showPass )
import Util		( unJust )
import UniqSupply	( mkSplitUniqSupply )

import Bag		( emptyBag )
import Outputable
import Interpreter
import CmStaticInfo	( GhciMode(..) )
import HscStats		( ppSourceStats )
import HscTypes		( ModDetails, ModIface(..), PersistentCompilerState(..),
			  PersistentRenamerState(..), ModuleLocation(..),
			  HomeSymbolTable, 
			  NameSupply(..), PackageRuleBase, HomeIfaceTable, 
			  typeEnvClasses, typeEnvTyCons, emptyIfaceTable )
import FiniteMap	( FiniteMap, plusFM, emptyFM, addToFM )
import OccName		( OccName )
import Name		( Name, nameModule, nameOccName, getName, isGlobalName,
			  emptyNameEnv )
import Module		( Module, lookupModuleEnvByName )

import Monad		( when )
import Maybe		( isJust )
import IO
\end{code}


%************************************************************************
%*									*
\subsection{The main compiler pipeline}
%*									*
%************************************************************************

\begin{code}
data HscResult
   -- compilation failed
   = HscFail     PersistentCompilerState -- updated PCS
   -- concluded that it wasn't necessary
   | HscNoRecomp PersistentCompilerState -- updated PCS
                 ModDetails  	         -- new details (HomeSymbolTable additions)
	         ModIface	         -- new iface (if any compilation was done)
   -- did recompilation
   | HscRecomp   PersistentCompilerState -- updated PCS
                 ModDetails  		 -- new details (HomeSymbolTable additions)
                 ModIface		 -- new iface (if any compilation was done)
	         (Maybe String) 	 -- generated stub_h filename (in /tmp)
	         (Maybe String)  	 -- generated stub_c filename (in /tmp)
	         (Maybe ([UnlinkedBCO],ItblEnv)) -- interpreted code, if any
             

	-- no errors or warnings; the individual passes
	-- (parse/rename/typecheck) print messages themselves

hscMain
  :: GhciMode
  -> DynFlags
  -> Bool			-- source unchanged?
  -> ModuleLocation		-- location info
  -> Maybe ModIface		-- old interface, if available
  -> HomeSymbolTable		-- for home module ModDetails
  -> HomeIfaceTable
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> IO HscResult

hscMain ghci_mode dflags source_unchanged location maybe_old_iface hst hit pcs
 = do {
      showPass dflags ("Checking old interface for hs = " 
			++ show (ml_hs_file location)
                	++ ", hspp = " ++ show (ml_hspp_file location));

      (pcs_ch, errs_found, (recomp_reqd, maybe_checked_iface))
         <- checkOldIface ghci_mode dflags hit hst pcs 
		(unJust "hscMain" (ml_hi_file location))
		source_unchanged maybe_old_iface;

      if errs_found then
         return (HscFail pcs_ch)
      else do {

      let no_old_iface = not (isJust maybe_checked_iface)
          what_next | recomp_reqd || no_old_iface = hscRecomp 
                    | otherwise                   = hscNoRecomp
      ;
      what_next ghci_mode dflags location maybe_checked_iface
                hst hit pcs_ch
      }}


-- we definitely expect to have the old interface available
hscNoRecomp ghci_mode dflags location (Just old_iface) hst hit pcs_ch
 | ghci_mode == OneShot
 = do {
      hPutStrLn stderr "compilation IS NOT required";
      let { bomb = panic "hscNoRecomp:OneShot" };
      return (HscNoRecomp pcs_ch bomb bomb)
      }
 | otherwise
 = do {
      hPutStrLn stderr "compilation IS NOT required";

      -- CLOSURE
      (pcs_cl, closure_errs, cl_hs_decls) 
         <- closeIfaceDecls dflags hit hst pcs_ch old_iface ;
      if closure_errs then 
         return (HscFail pcs_cl) 
      else do {

      -- TYPECHECK
      maybe_tc_result <- typecheckModule dflags pcs_cl hst 
					 old_iface alwaysQualify cl_hs_decls
					 False{-don't check for Main.main-};
      case maybe_tc_result of {
         Nothing -> return (HscFail pcs_cl);
         Just (pcs_tc, tc_result) -> do {

      let env_tc      = tc_env tc_result
          local_rules = tc_rules tc_result
      ;
      -- create a new details from the closed, typechecked, old iface
      let new_details = mkModDetailsFromIface env_tc local_rules
      ;
      return (HscNoRecomp pcs_tc new_details old_iface)
      }}}}


hscRecomp ghci_mode dflags location maybe_checked_iface hst hit pcs_ch
 = do	{
      	; when (verbosity dflags >= 1) $
		hPutStrLn stderr "compilation IS required";

      	  -- what target are we shooting for?
      	; let toInterp = dopt_HscLang dflags == HscInterpreted

 	    -------------------
 	    -- PARSE
 	    -------------------
	; maybe_parsed <- myParseModule dflags 
                             (unJust "hscRecomp:hspp" (ml_hspp_file location))
	; case maybe_parsed of {
      	     Nothing -> return (HscFail pcs_ch);
      	     Just rdr_module -> do {
	; let this_mod = mkHomeModule (hsModuleName rdr_module)
    
 	    -------------------
 	    -- RENAME
 	    -------------------
	; (pcs_rn, maybe_rn_result) 
      	     <- _scc_ "Rename" 
		 renameModule dflags hit hst pcs_ch this_mod rdr_module
      	; case maybe_rn_result of {
      	     Nothing -> return (HscFail pcs_ch{-was: pcs_rn-});
      	     Just (print_unqualified, (is_exported, new_iface, rn_hs_decls)) -> do {
    
 	    -- In interactive mode, we don't want to discard any top-level entities at
	    -- all (eg. do not inline them away during simplification), and retain them
	    -- all in the TypeEnv so they are available from the command line.
	    --
	    -- isGlobalName separates the user-defined top-level names from those
	    -- introduced by the type checker.
	; let dont_discard | ghci_mode == Interactive = isGlobalName
			   | otherwise = is_exported

 	    -------------------
 	    -- TYPECHECK
 	    -------------------
	; maybe_tc_result 
	    <- _scc_ "TypeCheck" typecheckModule dflags pcs_rn hst new_iface 
					     print_unqualified rn_hs_decls 
					     True{-check for Main.main if necessary-}
	; case maybe_tc_result of {
      	     Nothing -> return (HscFail pcs_ch{-was: pcs_rn-});
      	     Just (pcs_tc, tc_result) -> do {
    
	; let env_tc = tc_env tc_result

 	    -------------------
 	    -- DESUGAR
 	    -------------------
	; (ds_binds, ds_rules, foreign_stuff) 
             <- _scc_ "DeSugar" 
		deSugar dflags pcs_tc hst this_mod print_unqualified tc_result

 	    -------------------
 	    -- SIMPLIFY, TIDY-CORE
 	    -------------------
      	  -- We grab the the unfoldings at this point.
	; (pcs_simpl, tidy_binds, orphan_rules)
	      <- simplThenTidy dflags pcs_tc hst this_mod dont_discard ds_binds ds_rules
      	    
 	    -------------------
 	    -- BUILD THE NEW ModDetails AND ModIface
 	    -------------------
	; let new_details = mkModDetails env_tc tidy_binds orphan_rules
	; final_iface <- _scc_ "MkFinalIface" 
			  mkFinalIface ghci_mode dflags location 
                                      maybe_checked_iface new_iface new_details

 	    -------------------
 	    -- CONVERT TO STG and COMPLETE CODE GENERATION
 	    -------------------
	; (maybe_stub_h_filename, maybe_stub_c_filename, maybe_bcos)
      	     <- restOfCodeGeneration dflags toInterp this_mod
 		   (map ideclName (hsModuleImports rdr_module))
      		   foreign_stuff env_tc tidy_binds
      		   hit (pcs_PIT pcs_simpl)       

      	  -- and the answer is ...
	; return (HscRecomp pcs_simpl new_details final_iface
                            maybe_stub_h_filename maybe_stub_c_filename
      			    maybe_bcos)
      	  }}}}}}}



mkFinalIface ghci_mode dflags location maybe_old_iface new_iface new_details
 = case completeIface maybe_old_iface new_iface new_details of
      (new_iface, Nothing) -- no change in the interfacfe
         -> do when (dopt Opt_D_dump_hi_diffs dflags)
                    (printDump (text "INTERFACE UNCHANGED"))
               dumpIfSet_dyn dflags Opt_D_dump_hi
                             "UNCHANGED FINAL INTERFACE" (pprIface new_iface)
	       return new_iface
      (new_iface, Just sdoc_diffs)
         -> do dumpIfSet_dyn dflags Opt_D_dump_hi_diffs "INTERFACE HAS CHANGED" 
                                    sdoc_diffs
               dumpIfSet_dyn dflags Opt_D_dump_hi "NEW FINAL INTERFACE" 
                                    (pprIface new_iface)
               -- Write the interface file
               when (ghci_mode /= Interactive) 
                    (writeIface (unJust "hscRecomp:hi" (ml_hi_file location))
                                new_iface)
               return new_iface


myParseModule dflags src_filename
 = do --------------------------  Parser  ----------------
      showPass dflags "Parser"
      _scc_  "Parser" do

      buf <- hGetStringBuffer True{-expand tabs-} src_filename

      let glaexts | dopt Opt_GlasgowExts dflags = 1#
	          | otherwise 		        = 0#

      case parseModule buf PState{ bol = 0#, atbol = 1#,
	 		           context = [], glasgow_exts = glaexts,
  			           loc = mkSrcLoc (_PK_ src_filename) 1 } of {

	PFailed err -> do { hPutStrLn stderr (showSDoc err);
--	Not yet implemented in <4.11		    freeStringBuffer buf;
                            return Nothing };

	POk _ rdr_module@(HsModule mod_name _ _ _ _ _ _) -> do {

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;
      
      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;
      
      return (Just rdr_module)
	-- ToDo: free the string buffer later.
      }}


simplThenTidy dflags pcs hst this_mod dont_discard binds rules
 = do -- Do main Core-language transformations ---------
      -- _scc_     "Core2Core"
      (simplified, orphan_rules) 
         <- core2core dflags pcs hst dont_discard binds rules

      -- Do saturation and convert to A-normal form
      -- NOTE: subsequent passes may not transform the syntax, only annotate it
      saturated <- coreSatPgm dflags simplified

      -- Do the final tidy-up
      (pcs', tidy_binds, tidy_orphan_rules) 
         <- tidyCorePgm dflags this_mod pcs saturated orphan_rules
      
      return (pcs', tidy_binds, tidy_orphan_rules)


restOfCodeGeneration dflags toInterp this_mod imported_module_names
                     foreign_stuff env_tc tidy_binds
                     hit pit -- these last two for mapping ModNames to Modules
 | toInterp
 = do (bcos,itbl_env) 
         <- byteCodeGen dflags tidy_binds local_tycons local_classes
      return (Nothing, Nothing, Just (bcos,itbl_env))

 | otherwise
 = do
      --------------------------  Convert to STG -------------------------------
      (stg_binds, cost_centre_info) 
		<- _scc_ "CoreToStg"
		    myCoreToStg dflags this_mod tidy_binds env_tc

      --------------------------  Code generation ------------------------------
      abstractC <- _scc_ "CodeGen"
		    codeGen dflags this_mod imported_modules
                           cost_centre_info fe_binders
                           local_tycons stg_binds

      --------------------------  Code output -------------------------------
      (maybe_stub_h_name, maybe_stub_c_name)
         <- codeOutput dflags this_mod local_tycons
                       tidy_binds stg_binds
                       c_code h_code abstractC

      return (maybe_stub_h_name, maybe_stub_c_name, Nothing)
 where
    local_tycons     = typeEnvTyCons env_tc
    local_classes    = typeEnvClasses env_tc
    imported_modules = map mod_name_to_Module imported_module_names
    (h_code,c_code,fe_binders) = foreign_stuff

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


myCoreToStg dflags this_mod tidy_binds env_tc
 = do 
      () <- coreBindsSize tidy_binds `seq` return ()
      -- TEMP: the above call zaps some space usage allocated by the
      -- simplifier, which for reasons I don't understand, persists
      -- thoroughout code generation

      --let bcos = byteCodeGen dflags tidy_binds local_tycons local_classes

      
      stg_binds <- _scc_ "Core2Stg" coreToStg dflags this_mod tidy_binds

      (stg_binds2, cost_centre_info)
	   <- _scc_ "Core2Stg" stg2stg dflags this_mod stg_binds

      return (stg_binds2, cost_centre_info)
   where
      local_tycons  = typeEnvTyCons env_tc
      local_classes = typeEnvClasses env_tc
\end{code}


%************************************************************************
%*									*
\subsection{Compiling an expression}
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
hscExpr
  :: DynFlags
  -> Bool			-- True <=> wrap in 'print' to get a result of IO type
  -> HomeSymbolTable	
  -> HomeIfaceTable
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> Module			-- Context for compiling
  -> String			-- The expression
  -> IO ( PersistentCompilerState, 
	  Maybe (UnlinkedBCOExpr, PrintUnqualified, Type) )

hscExpr dflags wrap_io hst hit pcs0 this_module expr
   = do {
	maybe_parsed <- hscParseExpr dflags expr;
	case maybe_parsed of
      	     Nothing -> return (pcs0, Nothing)
      	     Just parsed_expr -> do {

		-- Rename it
	(pcs1, maybe_renamed_expr) <- 
		renameExpr dflags hit hst pcs0 this_module parsed_expr;
	case maybe_renamed_expr of
		Nothing -> return ({-WAS:pcs1-} pcs0, Nothing)
		Just (print_unqual, rn_expr) -> do {

		-- Typecheck it
	maybe_tc_return
	   <- typecheckExpr dflags wrap_io pcs1 hst print_unqual this_module rn_expr;
	case maybe_tc_return of {
		Nothing -> return ({-WAS:pcs1-} pcs0, Nothing);
		Just (pcs2, tc_expr, ty) -> do

		-- Desugar it
	ds_expr <- deSugarExpr dflags pcs2 hst this_module
			print_unqual tc_expr;
	
		-- Simplify it
	simpl_expr <- simplifyExpr dflags pcs2 hst ds_expr;

		-- Saturate it
	sat_expr <- coreSatExpr dflags simpl_expr;

		-- ToDo: need to do SRTs?

		-- Convert to BCOs
	bcos <- coreExprToBCOs dflags sat_expr

	return (pcs2, Just (bcos, print_unqual, ty));
     }}}}

hscParseExpr :: DynFlags -> String -> IO (Maybe RdrNameHsExpr)
hscParseExpr dflags str
 = do --------------------------  Parser  ----------------
      showPass dflags "Parser"
      _scc_ "Parser" do

      buf <- stringToStringBuffer str

      let glaexts | dopt Opt_GlasgowExts dflags = 1#
       	          | otherwise  	          = 0#

      case parseExpr buf PState{ bol = 0#, atbol = 1#,
	 		         context = [], glasgow_exts = glaexts,
			         loc = mkSrcLoc SLIT("<no file>") 0 } of {

	PFailed err -> do { hPutStrLn stderr (showSDoc err);
--	Not yet implemented in <4.11		    freeStringBuffer buf;
                            return Nothing };

	POk _ rdr_expr -> do {

      --ToDo: can't free the string buffer until we've finished this
      -- compilation sweep and all the identifiers have gone away.
      --freeStringBuffer buf;
      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_expr);
      return (Just rdr_expr)
      }}
#endif
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
        PCS { pcs_PIT   = emptyIfaceTable,
              pcs_PTE   = wiredInThingEnv,
	      pcs_insts = emptyInstEnv,
	      pcs_rules = emptyRuleBase,
	      pcs_PRS   = prs
            }
        )

initPersistentRenamerState :: IO PersistentRenamerState
  = do us <- mkSplitUniqSupply 'r'
       return (
        PRS { prsOrig  = NameSupply { nsUniqs = us,
				      nsNames = initOrigNames,
			      	      nsIPs   = emptyFM },
	      prsDecls 	 = (emptyNameEnv, 0),
	      prsInsts 	 = (emptyBag, 0),
	      prsRules 	 = (emptyBag, 0),
	      prsImpMods = emptyFM
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
