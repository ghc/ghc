%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%

\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module HscMain ( 
	HscResult(..), hscMain, initPersistentCompilerState
#ifdef GHCI
	, hscStmt, hscTcExpr, hscThing, 
	, compileExpr
#endif
	) where

#include "HsVersions.h"

#ifdef GHCI
import TcHsSyn		( TypecheckedHsExpr )
import CodeOutput	( outputForeignStubs )
import ByteCodeGen	( byteCodeGen, coreExprToBCOs )
import Linker		( HValue, linkExpr )
import TidyPgm		( tidyCoreExpr )
import CorePrep		( corePrepExpr )
import Flattening	( flattenExpr )
import TcRnDriver	( tcRnStmt, tcRnExpr, tcRnThing ) 
import RdrHsSyn		( RdrNameStmt )
import Type		( Type )
import PrelNames	( iNTERACTIVE )
import StringBuffer	( stringToStringBuffer )
import SrcLoc		( noSrcLoc )
import Name		( Name )
import CoreLint		( lintUnfolding )
#endif

import HsSyn

import RdrName		( nameRdrName )
import StringBuffer	( hGetStringBuffer, freeStringBuffer )
import Parser
import Lex		( ParseResult(..), ExtFlags(..), mkPState )
import SrcLoc		( mkSrcLoc )
import TcRnDriver	( checkOldIface, tcRnModule, tcRnExtCore, tcRnIface )
import RnEnv		( extendOrigNameCache )
import PrelInfo		( wiredInThingEnv, knownKeyNames )
import PrelRules	( builtinRules )
import MkIface		( mkIface )
import Desugar
import Flattening       ( flatten )
import SimplCore
import TidyPgm		( tidyCorePgm )
import CorePrep		( corePrepPgm )
import CoreToStg	( coreToStg )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
import CodeOutput	( codeOutput )

import Module		( emptyModuleEnv )
import CmdLineOpts
import DriverPhases     ( isExtCore_file )
import ErrUtils		( dumpIfSet_dyn, showPass )
import UniqSupply	( mkSplitUniqSupply )

import Bag		( consBag, emptyBag )
import Outputable
import HscStats		( ppSourceStats )
import HscTypes
import MkExternalCore	( emitExternalCore )
import ParserCore
import ParserCoreUtils
import FiniteMap	( emptyFM )
import Name		( nameModule )
import Module		( Module, ModLocation(..), showModMsg )
import FastString
import Maybes		( expectJust )

import Monad		( when )
import Maybe		( isJust, fromJust )
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
	         Bool	 	 	-- stub_h exists
	         Bool  		 	-- stub_c exists
	         (Maybe CompiledByteCode)

	-- no errors or warnings; the individual passes
	-- (parse/rename/typecheck) print messages themselves

hscMain
  :: HscEnv
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> Module
  -> ModLocation		-- location info
  -> Bool			-- True <=> source unchanged
  -> Bool			-- True <=> have an object file (for msgs only)
  -> Maybe ModIface		-- old interface, if available
  -> IO HscResult

hscMain hsc_env pcs mod location 
	source_unchanged have_object maybe_old_iface
 = do {
      (pcs_ch, maybe_chk_result) <- _scc_ "checkOldIface" 
				    checkOldIface hsc_env pcs mod 
						  (ml_hi_file location)
						  source_unchanged maybe_old_iface;
      case maybe_chk_result of {
	Nothing -> return (HscFail pcs_ch) ;
	Just (recomp_reqd, maybe_checked_iface) -> do {

      let no_old_iface = not (isJust maybe_checked_iface)
          what_next | recomp_reqd || no_old_iface = hscRecomp 
                    | otherwise                   = hscNoRecomp

      ; what_next hsc_env pcs_ch have_object 
		  mod location maybe_checked_iface
      }}}


-- hscNoRecomp definitely expects to have the old interface available
hscNoRecomp hsc_env pcs_ch have_object 
	    mod location (Just old_iface)
 | hsc_mode hsc_env == OneShot
 = do {
      when (verbosity (hsc_dflags hsc_env) > 0) $
	  hPutStrLn stderr "compilation IS NOT required";
      let { bomb = panic "hscNoRecomp:OneShot" };
      return (HscNoRecomp pcs_ch bomb bomb)
      }
 | otherwise
 = do {
      when (verbosity (hsc_dflags hsc_env) >= 1) $
		hPutStrLn stderr ("Skipping  " ++ 
			showModMsg have_object mod location);

      -- Typecheck 
      (pcs_tc, maybe_tc_result) <- tcRnIface hsc_env pcs_ch old_iface ;

      case maybe_tc_result of {
         Nothing -> return (HscFail pcs_tc);
         Just new_details ->

      return (HscNoRecomp pcs_tc new_details old_iface)
      }}

hscRecomp hsc_env pcs_ch have_object 
	  mod location maybe_checked_iface
 = do	{
      	  -- what target are we shooting for?
	; let one_shot  = hsc_mode hsc_env == OneShot
      	; let dflags    = hsc_dflags hsc_env
	; let toInterp  = dopt_HscLang dflags == HscInterpreted
	; let toCore    = isJust (ml_hs_file location) &&
			  isExtCore_file (fromJust (ml_hs_file location))

      	; when (not one_shot && verbosity dflags >= 1) $
		hPutStrLn stderr ("Compiling " ++ 
			showModMsg (not toInterp) mod location);
			
	; front_res <- if toCore then 
			  hscCoreFrontEnd hsc_env pcs_ch location
		       else 
			  hscFrontEnd hsc_env pcs_ch location

	; case front_res of
	    Left flure -> return flure;
	    Right (pcs_tc, ds_result) -> do {


	-- OMITTED: 
	-- ; seqList imported_modules (return ())

 	    -------------------
 	    -- FLATTENING
 	    -------------------
	; flat_result <- _scc_ "Flattening"
 			 flatten hsc_env pcs_tc ds_result


	; let 	-- Rule-base accumulated from imported packages
	     pkg_rule_base = eps_rule_base (pcs_EPS pcs_tc)

		-- In one-shot mode, ZAP the external package state at
		-- this point, because we aren't going to need it from
		-- now on.  We keep the name cache, however, because
		-- tidyCore needs it.
	     pcs_middle 
		 | one_shot  = pcs_tc{ pcs_EPS = error "pcs_EPS missing" }
		 | otherwise = pcs_tc

	; pkg_rule_base `seq` pcs_middle `seq` return ()

	-- alive at this point:  
	--	pcs_middle
	--	flat_result
	--      pkg_rule_base

 	    -------------------
 	    -- SIMPLIFY
 	    -------------------
	; simpl_result <- _scc_     "Core2Core"
			  core2core hsc_env pkg_rule_base flat_result

 	    -------------------
 	    -- TIDY
 	    -------------------
	; (pcs_simpl, tidy_result) 
	     <- _scc_ "CoreTidy"
	        tidyCorePgm dflags pcs_middle simpl_result

	-- ZAP the persistent compiler state altogether now if we're
	-- in one-shot mode, to save space.
	; pcs_final <- if one_shot then return (error "pcs_final missing")
				   else return pcs_simpl

	; emitExternalCore dflags tidy_result

	-- Alive at this point:  
	--	tidy_result, pcs_final
	--      hsc_env

 	    -------------------
	    -- BUILD THE NEW ModIface and ModDetails
	    --	and emit external core if necessary
	    -- This has to happen *after* code gen so that the back-end
	    -- info has been set.  Not yet clear if it matters waiting
	    -- until after code output
	; new_iface <- _scc_ "MkFinalIface" 
			mkIface hsc_env location 
                         	maybe_checked_iface tidy_result


	    -- Space leak reduction: throw away the new interface if
	    -- we're in one-shot mode; we won't be needing it any
	    -- more.
	; final_iface <-
	     if one_shot then return (error "no final iface")
			 else return new_iface

	    -- Build the final ModDetails (except in one-shot mode, where
	    -- we won't need this information after compilation).
	; final_details <- 
	     if one_shot then return (error "no final details")
		 	 else return $! ModDetails { 
					   md_types = mg_types tidy_result,
					   md_insts = mg_insts tidy_result,
					   md_rules = mg_rules tidy_result }

 	    -------------------
 	    -- CONVERT TO STG and COMPLETE CODE GENERATION
	; (stub_h_exists, stub_c_exists, maybe_bcos)
		<- hscBackEnd dflags tidy_result

      	  -- and the answer is ...
	; return (HscRecomp pcs_final
			    final_details
			    final_iface
                            stub_h_exists stub_c_exists
      			    maybe_bcos)
      	 }}

hscCoreFrontEnd hsc_env pcs_ch location = do {
 	    -------------------
 	    -- PARSE
 	    -------------------
	; inp <- readFile (expectJust "hscCoreFrontEnd:hspp" (ml_hspp_file location))
	; case parseCore inp 1 of
	    FailP s        -> hPutStrLn stderr s >> return (Left (HscFail pcs_ch));
	    OkP rdr_module -> do {
    
 	    -------------------
 	    -- RENAME and TYPECHECK
 	    -------------------
	; (pcs_tc, maybe_tc_result) <- _scc_ "TypeCheck" 
			     	       tcRnExtCore hsc_env pcs_ch rdr_module
	; case maybe_tc_result of {
      	     Nothing       -> return (Left  (HscFail pcs_tc));
      	     Just mod_guts -> return (Right (pcs_tc, mod_guts))
					-- No desugaring to do!
	}}}
	 

hscFrontEnd hsc_env pcs_ch location = do {
 	    -------------------
 	    -- PARSE
 	    -------------------
	; maybe_parsed <- myParseModule (hsc_dflags hsc_env) 
                             (expectJust "hscFrontEnd:hspp" (ml_hspp_file location))

	; case maybe_parsed of {
      	     Nothing -> return (Left (HscFail pcs_ch));
      	     Just rdr_module -> do {
    
 	    -------------------
 	    -- RENAME and TYPECHECK
 	    -------------------
	; (pcs_tc, maybe_tc_result) <- _scc_ "Typecheck-Rename" 
				        tcRnModule hsc_env pcs_ch rdr_module
	; case maybe_tc_result of {
      	     Nothing -> return (Left (HscFail pcs_ch));
      	     Just tc_result -> do {

 	    -------------------
 	    -- DESUGAR
 	    -------------------
	; ds_result <- _scc_ "DeSugar" 
		       deSugar hsc_env pcs_tc tc_result
	; return (Right (pcs_tc, ds_result))
	}}}}}


hscBackEnd dflags 
    ModGuts{  -- This is the last use of the ModGuts in a compilation.
	      -- From now on, we just use the bits we need.
        mg_module   = this_mod,
	mg_binds    = core_binds,
	mg_types    = type_env,
	mg_dir_imps = dir_imps,
	mg_foreign  = foreign_stubs,
	mg_deps     = dependencies     }  = do {

 	    -------------------
 	    -- PREPARE FOR CODE GENERATION
	    -- Do saturation and convert to A-normal form
  prepd_binds <- _scc_ "CorePrep"
		 corePrepPgm dflags core_binds type_env;

  case dopt_HscLang dflags of
      HscNothing -> return (False, False, Nothing)

      HscInterpreted ->
#ifdef GHCI
	do  -----------------  Generate byte code ------------------
	    comp_bc <- byteCodeGen dflags prepd_binds type_env
	
	    ------------------ Create f-x-dynamic C-side stuff ---
	    (istub_h_exists, istub_c_exists) 
	       <- outputForeignStubs dflags foreign_stubs
	    
	    return ( istub_h_exists, istub_c_exists, Just comp_bc )
#else
	panic "GHC not compiled with interpreter"
#endif

      other ->
	do
	    -----------------  Convert to STG ------------------
	    (stg_binds, cost_centre_info) <- _scc_ "CoreToStg"
	    		 myCoreToStg dflags this_mod prepd_binds	

            ------------------  Code generation ------------------
	    abstractC <- _scc_ "CodeGen"
		         codeGen dflags this_mod type_env foreign_stubs
				 dir_imps cost_centre_info stg_binds

	    ------------------  Code output -----------------------
	    (stub_h_exists, stub_c_exists)
		     <- codeOutput dflags this_mod foreign_stubs 
				dependencies abstractC

	    return (stub_h_exists, stub_c_exists, Nothing)
   }


myParseModule dflags src_filename
 = do --------------------------  Parser  ----------------
      showPass dflags "Parser"
      _scc_  "Parser" do
      buf <- hGetStringBuffer src_filename

      let exts = mkExtFlags dflags
	  loc  = mkSrcLoc (mkFastString src_filename) 1

      case parseModule buf (mkPState loc exts) of {

	PFailed err -> do { hPutStrLn stderr (showSDoc err);
                            freeStringBuffer buf;
                            return Nothing };

	POk _ rdr_module -> do {

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;
      
      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;
      
      return (Just rdr_module)
	-- ToDo: free the string buffer later.
      }}


myCoreToStg dflags this_mod prepd_binds
 = do 
      stg_binds <- _scc_ "Core2Stg" 
	     coreToStg dflags prepd_binds

      (stg_binds2, cost_centre_info) <- _scc_ "Core2Stg" 
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
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> InteractiveContext		-- Context for compiling
  -> String			-- The statement
  -> IO ( PersistentCompilerState, 
	  Maybe (InteractiveContext, [Name], HValue) )

hscStmt hsc_env pcs icontext stmt
  = do	{ maybe_stmt <- hscParseStmt (hsc_dflags hsc_env) stmt
	; case maybe_stmt of {
      	     Nothing -> return (pcs, Nothing) ;
      	     Just parsed_stmt -> do {

		-- Rename and typecheck it
	  (pcs1, maybe_tc_result)
		 <- tcRnStmt hsc_env pcs icontext parsed_stmt

	; case maybe_tc_result of {
		Nothing -> return (pcs1, Nothing) ;
		Just (new_ic, bound_names, tc_expr) -> do {

		-- Then desugar, code gen, and link it
	; hval <- compileExpr hsc_env pcs1 iNTERACTIVE 
			      (ic_rn_gbl_env new_ic) 
			      (ic_type_env new_ic)
			      tc_expr

	; return (pcs1, Just (new_ic, bound_names, hval))
	}}}}}

hscTcExpr	-- Typecheck an expression (but don't run it)
  :: HscEnv
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> InteractiveContext		-- Context for compiling
  -> String			-- The expression
  -> IO (PersistentCompilerState, Maybe Type)

hscTcExpr hsc_env pcs icontext expr
  = do	{ maybe_stmt <- hscParseStmt (hsc_dflags hsc_env) expr
	; case maybe_stmt of {
	     Just (ExprStmt expr _ _) 
			-> tcRnExpr hsc_env pcs icontext expr ;
	     Just other -> do { hPutStrLn stderr ("not an expression: `" ++ expr ++ "'") ;
			        return (pcs, Nothing) } ;
      	     Nothing    -> return (pcs, Nothing) } }
\end{code}

\begin{code}
hscParseStmt :: DynFlags -> String -> IO (Maybe RdrNameStmt)
hscParseStmt dflags str
 = do showPass dflags "Parser"
      _scc_ "Parser"  do

      buf <- stringToStringBuffer str

      let exts = mkExtFlags dflags 
	  loc  = mkSrcLoc FSLIT("<interactive>") 1

      case parseStmt buf (mkPState loc exts) of {

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
\subsection{Getting information about an identifer}
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
hscThing -- like hscStmt, but deals with a single identifier
  :: HscEnv
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> InteractiveContext		-- Context for compiling
  -> String			-- The identifier
  -> IO ( PersistentCompilerState,
	  [TyThing] )

hscThing hsc_env pcs0 ic str
   = do let dflags 	   = hsc_dflags hsc_env

	maybe_rdr_name <- myParseIdentifier dflags str
	case maybe_rdr_name of {
	  Nothing -> return (pcs0, []);
	  Just rdr_name -> do

	(pcs1, maybe_tc_result) <- 
	   tcRnThing hsc_env pcs0 ic rdr_name

	case maybe_tc_result of {
	     Nothing     -> return (pcs1, []) ;
	     Just things -> return (pcs1, things)
 	}}

myParseIdentifier dflags str
  = do buf <- stringToStringBuffer str
 
       let exts = mkExtFlags dflags
	   loc  = mkSrcLoc FSLIT("<interactive>") 1

       case parseIdentifier buf (mkPState loc exts) of

	  PFailed err -> do { hPutStrLn stderr (showSDoc err);
           		      freeStringBuffer buf;
                              return Nothing }

	  POk _ rdr_name -> do { --should, but can't: freeStringBuffer buf;
			         return (Just rdr_name) }
#endif
\end{code}

%************************************************************************
%*									*
	Desugar, simplify, convert to bytecode, and link an expression
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
compileExpr :: HscEnv 
	    -> PersistentCompilerState
	    -> Module -> GlobalRdrEnv -> TypeEnv
	    -> TypecheckedHsExpr
	    -> IO HValue

compileExpr hsc_env pcs this_mod rdr_env type_env tc_expr
  = do	{ let { dflags  = hsc_dflags hsc_env ;
		lint_on = dopt Opt_DoCoreLinting dflags }
	      
	 	-- Desugar it
	; ds_expr <- deSugarExpr hsc_env pcs this_mod rdr_env type_env tc_expr
	
		-- Flatten it
	; flat_expr <- flattenExpr hsc_env pcs ds_expr

		-- Simplify it
	; simpl_expr <- simplifyExpr dflags flat_expr

		-- Tidy it (temporary, until coreSat does cloning)
	; tidy_expr <- tidyCoreExpr simpl_expr

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
	; hval <- linkExpr hsc_env pcs bcos

	; return hval
     }
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
  = do nc <- initNameCache
       return (
        PCS { pcs_EPS = initExternalPackageState,
	      pcs_nc  = nc })

initNameCache :: IO NameCache
  = do us <- mkSplitUniqSupply 'r'
       return (NameCache { nsUniqs = us,
			   nsNames = initOrigNames,
			   nsIPs   = emptyFM })

initExternalPackageState :: ExternalPackageState
initExternalPackageState
  = emptyExternalPackageState { 
      eps_rules  = foldr add_rule (emptyBag, 0) builtinRules,
      eps_PTE    = wiredInThingEnv,
    }
  where
    add_rule (name,rule) (rules, n_slurped)
	 = (gated_decl `consBag` rules, n_slurped)
	where
	   gated_decl = (gate_fn, (mod, IfaceRuleOut rdr_name rule))
	   mod	      = nameModule name
	   rdr_name   = nameRdrName name	-- Seems a bit of a hack to go back
						-- to the RdrName
	   gate_fn vis_fn = vis_fn name		-- Load the rule whenever name is visible

initOrigNames :: OrigNameCache
initOrigNames = foldl extendOrigNameCache emptyModuleEnv knownKeyNames 

mkExtFlags dflags
  = ExtFlags { glasgowExtsEF = dopt Opt_GlasgowExts dflags,
	       ffiEF	     = dopt Opt_FFI	 dflags,
	       withEF	     = dopt Opt_With	 dflags,
	       arrowsEF	     = dopt Opt_Arrows	 dflags,
	       parrEF	     = dopt Opt_PArr	 dflags}
\end{code}
