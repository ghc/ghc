%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%
\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module HscMain ( HscResult(..), hscMain, 
#ifdef GHCI
		 hscStmt,
#endif
		 initPersistentCompilerState ) where

#include "HsVersions.h"

#ifdef GHCI
import RdrHsSyn		( RdrNameStmt )
import Rename		( renameStmt )
import ByteCodeGen	( byteCodeGen )
import Id		( Id, idName, idFlavour, modifyIdInfo )
import IdInfo		( setFlavourInfo, makeConstantFlavour )
import HscTypes		( InteractiveContext(..), TyThing(..) )
#endif

import HsSyn

import StringBuffer	( hGetStringBuffer, 
                          stringToStringBuffer, freeStringBuffer )
import Parser
import Lex		( PState(..), ParseResult(..) )
import SrcLoc		( mkSrcLoc )
import Rename		( checkOldIface, renameModule, closeIfaceDecls )
import Rules		( emptyRuleBase )
import PrelInfo		( wiredInThingEnv, wiredInThings )
import PrelNames	( vanillaSyntaxMap, knownKeyNames )
import MkIface		( completeIface, mkModDetailsFromIface, mkModDetails,
			  writeIface, pprIface )
import TcModule
import InstEnv		( emptyInstEnv )
import Desugar
import SimplCore
import CoreUtils	( coreBindsSize )
import CoreTidy		( tidyCorePgm )
import CoreSat
import CoreTidy		( tidyCoreExpr )
import CoreToStg	( coreToStg )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
import CodeOutput	( codeOutput )

import Module		( ModuleName, moduleName, mkHomeModule, 
			  moduleUserString )
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
			  typeEnvClasses, typeEnvTyCons, emptyIfaceTable
			)
import FiniteMap	( FiniteMap, plusFM, emptyFM, addToFM )
import OccName		( OccName )
import Name		( Name, nameModule, nameOccName, getName, isGlobalName,
			  emptyNameEnv
			)
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
  -> Module
  -> ModuleLocation		-- location info
  -> Bool			-- source unchanged?
  -> Maybe ModIface		-- old interface, if available
  -> HomeSymbolTable		-- for home module ModDetails
  -> HomeIfaceTable
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> IO HscResult

hscMain ghci_mode dflags mod location source_unchanged maybe_old_iface hst hit pcs
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
      what_next ghci_mode dflags mod location maybe_checked_iface
                hst hit pcs_ch
      }}


-- we definitely expect to have the old interface available
hscNoRecomp ghci_mode dflags mod location (Just old_iface) hst hit pcs_ch
 | ghci_mode == OneShot
 = do {
      hPutStrLn stderr "compilation IS NOT required";
      let { bomb = panic "hscNoRecomp:OneShot" };
      return (HscNoRecomp pcs_ch bomb bomb)
      }
 | otherwise
 = do {
      when (verbosity dflags >= 1) $
		hPutStrLn stderr ("Skipping  " ++ compMsg mod location);

      -- CLOSURE
      (pcs_cl, closure_errs, cl_hs_decls) 
         <- closeIfaceDecls dflags hit hst pcs_ch old_iface ;
      if closure_errs then 
         return (HscFail pcs_cl) 
      else do {

      -- TYPECHECK
      maybe_tc_result 
	<- typecheckIface dflags pcs_cl hst old_iface (vanillaSyntaxMap, cl_hs_decls);

      case maybe_tc_result of {
         Nothing -> return (HscFail pcs_cl);
         Just (pcs_tc, env_tc, local_rules) -> do {

      -- create a new details from the closed, typechecked, old iface
      let new_details = mkModDetailsFromIface env_tc local_rules
      ;
      return (HscNoRecomp pcs_tc new_details old_iface)
      }}}}

compMsg mod location =
    mod_str ++ take (12 - length mod_str) (repeat ' ')
    ++ " (" ++ unJust "hscRecomp" (ml_hs_file location) ++ ")"
 where mod_str = moduleUserString mod


hscRecomp ghci_mode dflags mod location maybe_checked_iface hst hit pcs_ch
 = do	{
      	; when (verbosity dflags >= 1) $
		hPutStrLn stderr ("Compiling " ++ compMsg mod location);

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
	; (pcs_rn, print_unqualified, maybe_rn_result) 
      	     <- _scc_ "Rename" 
		 renameModule dflags hit hst pcs_ch this_mod rdr_module
      	; case maybe_rn_result of {
      	     Nothing -> return (HscFail pcs_ch{-was: pcs_rn-});
      	     Just (is_exported, new_iface, rn_hs_decls) -> do {
    
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
	      -- Do saturation and convert to A-normal form
	; saturated <- coreSatPgm dflags tidy_binds

	; (maybe_stub_h_filename, maybe_stub_c_filename, maybe_bcos)
      	     <- restOfCodeGeneration dflags toInterp this_mod
 		   (map ideclName (hsModuleImports rdr_module))
      		   foreign_stuff env_tc saturated
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
                            freeStringBuffer buf;
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

      -- Do the final tidy-up
      (pcs', tidy_binds, tidy_orphan_rules) 
         <- tidyCorePgm dflags this_mod pcs simplified orphan_rules
      
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
\subsection{Compiling a do-statement}
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
hscStmt
  :: DynFlags
  -> HomeSymbolTable	
  -> HomeIfaceTable
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> InteractiveContext		-- Context for compiling
  -> String			-- The statement
  -> IO ( PersistentCompilerState, 
	  Maybe ( [Id], 
		 UnlinkedBCOExpr) )
\end{code}

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
hscStmt dflags hst hit pcs0 icontext stmt
   = let 
	InteractiveContext { 
	     ic_rn_env = rn_env, 
	     ic_type_env = type_env,
	     ic_module   = this_mod } = icontext
     in
     do { maybe_stmt <- hscParseStmt dflags stmt
	; case maybe_stmt of
      	     Nothing -> return (pcs0, Nothing)
      	     Just parsed_stmt -> do {

		-- Rename it
	  (pcs1, print_unqual, maybe_renamed_stmt)
		 <- renameStmt dflags hit hst pcs0 this_mod rn_env parsed_stmt
	; case maybe_renamed_stmt of
		Nothing -> return (pcs0, Nothing)
		Just (bound_names, rn_stmt) -> do {

		-- Typecheck it
	  maybe_tc_return <- typecheckStmt dflags pcs1 hst type_env
					   print_unqual this_mod bound_names rn_stmt
	; case maybe_tc_return of {
		Nothing -> return (pcs0, Nothing) ;
		Just (pcs2, tc_expr, bound_ids) -> do {

		-- Desugar it
	  ds_expr <- deSugarExpr dflags pcs2 hst this_mod print_unqual tc_expr
	
		-- Simplify it
	; simpl_expr <- simplifyExpr dflags pcs2 hst ds_expr

		-- Tidy it (temporary, until coreSat does cloning)
	; tidy_expr <- tidyCoreExpr simpl_expr

		-- Saturate it
	; sat_expr <- coreSatExpr dflags tidy_expr

		-- Convert to BCOs
	; bcos <- coreExprToBCOs dflags sat_expr

	; let
		-- make all the bound ids "constant" ids, now that
		-- they're notionally top-level bindings.  This is
		-- important: otherwise when we come to compile an expression
		-- using these ids later, the byte code generator will consider
		-- the occurrences to be free rather than global.
	     constant_bound_ids = map constantizeId bound_ids;

	     constantizeId id
		 = modifyIdInfo (`setFlavourInfo` makeConstantFlavour 
					(idFlavour id)) id

	; return (pcs2, Just (constant_bound_ids, bcos))
     }}}}}

hscParseStmt :: DynFlags -> String -> IO (Maybe RdrNameStmt)
hscParseStmt dflags str
 = do --------------------------  Parser  ----------------
      showPass dflags "Parser"
      _scc_ "Parser" do

      buf <- stringToStringBuffer str

      let glaexts | dopt Opt_GlasgowExts dflags = 1#
       	          | otherwise  	                = 0#

      case parseStmt buf PState{ bol = 0#, atbol = 1#,
	 		         context = [], glasgow_exts = glaexts,
			         loc = mkSrcLoc SLIT("<no file>") 0 } of {

	PFailed err -> do { hPutStrLn stderr (showSDoc err);
--	Not yet implemented in <4.11    freeStringBuffer buf;
                            return Nothing };

	-- no stmt: the line consisted of just space or comments
	POk _ Nothing -> return Nothing;

	POk _ (Just rdr_stmt) -> do {

      --ToDo: can't free the string buffer until we've finished this
      -- compilation sweep and all the identifiers have gone away.
      --freeStringBuffer buf;
      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_stmt);
      return (Just rdr_stmt)
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
