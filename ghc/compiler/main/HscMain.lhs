%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
%
\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module HscMain ( hscMain ) where

#include "HsVersions.h"

import IO		( hPutStr, stderr )
import HsSyn

import RdrHsSyn		( RdrNameHsModule )
import FastString	( unpackFS )
import StringBuffer	( hGetStringBuffer )
import Parser		( parse )
import Lex		( PState(..), ParseResult(..) )
import SrcLoc		( mkSrcLoc )

import Rename		( renameModule )

import PrelInfo		( wiredInThings )
import MkIface		( writeIface )
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

import Module		( ModuleName, moduleNameUserString )
import CmdLineOpts
import ErrUtils		( ghcExit, doIfSet, dumpIfSet )
import UniqSupply	( mkSplitUniqSupply )

import Outputable
import Char		( isSpace )
import StgInterp	( runStgI )
import HscStats		( ppSourceStats )
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
	     (Maybe [UnlinkedIBind]) -- interpreted code, if any
             PersistentCompilerState -- updated PCS
             (Bag WarnMsg) 		-- warnings

   | HscErrs PersistentCompilerState -- updated PCS
             (Bag ErrMsg)		-- errors
             (Bag WarnMsg)             -- warnings

hscMain
  :: DynFlags	
  -> ModSummary       -- summary, including source filename
  -> Maybe ModIFace   -- old interface, if available
  -> String	      -- file in which to put the output (.s, .hc, .java etc.)
  -> HomeSymbolTable		-- for home module ModDetails
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> IO HscResult

hscMain flags core_cmds stg_cmds summary maybe_old_iface
	output_filename mod_details pcs1 =

	--------------------------  Reader  ----------------
    show_pass "Parser"	>>
    _scc_     "Parser"

    let src_filename -- name of the preprocessed source file
       = case ms_ppsource summary of
            Just (filename, fingerprint) -> filename
            Nothing -> pprPanic "hscMain:summary is not of a source module"
                                (ppr summary)

    buf <- hGetStringBuffer True{-expand tabs-} src_filename

    let glaexts | opt_GlasgowExts = 1#
		| otherwise       = 0#

    case parse buf PState{ bol = 0#, atbol = 1#,
		           context = [], glasgow_exts = glaexts,
		           loc = mkSrcLoc src_filename 1 } of {

	PFailed err -> return (CompErrs pcs err)

	POk _ rdr_module@(HsModule mod_name _ _ _ _ _ _) ->

    dumpIfSet (dopt_D_dump_parsed flags) "Parser" (ppr rdr_module) >>

    dumpIfSet (dopt_D_source_stats flags) "Source Statistics"
	(ppSourceStats False rdr_module)	 	>>

    -- UniqueSupplies for later use (these are the only lower case uniques)
    mkSplitUniqSupply 'd'	>>= \ ds_uniqs 	-> -- desugarer
    mkSplitUniqSupply 'r'	>>= \ ru_uniqs 	-> -- rules
    mkSplitUniqSupply 'c'	>>= \ c2s_uniqs -> -- core-to-stg
    mkSplitUniqSupply 'u'	>>= \ tidy_uniqs -> -- tidy up
    mkSplitUniqSupply 'g'	>>= \ st_uniqs  -> -- stg-to-stg passes
    mkSplitUniqSupply 'n'	>>= \ ncg_uniqs -> -- native-code generator

	--------------------------  Rename  ----------------
    show_pass "Renamer" 			>>
    _scc_     "Renamer"

    renameModule rn_uniqs rdr_module		>>= \ maybe_rn_stuff ->
    case maybe_rn_stuff of {
	Nothing -> 	-- Hurrah!  Renamer reckons that there's no need to
			-- go any further
			reportCompile mod_name "Compilation NOT required!" >>
			return ();
	
	Just (this_mod, rn_mod, 
	      old_iface, new_iface,
	      rn_name_supply, fixity_env,
	      imported_modules) ->
			-- Oh well, we've got to recompile for real


	--------------------------  Typechecking ----------------
    show_pass "TypeCheck" 				>>
    _scc_     "TypeCheck"
    typecheckModule dflags mod pcs hst hit pit rn_mod
    --                tc_uniqs rn_name_supply
    --		    fixity_env rn_mod	        
						>>= \ maybe_tc_stuff ->
    case maybe_tc_stuff of {
	Nothing -> ghcExit 1;	-- Type checker failed

	Just (tc_results@(TcResults {tc_tycons  = local_tycons, 
		   	 	     tc_classes = local_classes, 
		   	 	     tc_insts   = inst_info })) ->


	--------------------------  Desugaring ----------------
    _scc_     "DeSugar"
    deSugar this_mod ds_uniqs tc_results	>>= \ (desugared, rules, h_code, c_code, fe_binders) ->


	--------------------------  Main Core-language transformations ----------------
    _scc_     "Core2Core"
    core2core core_cmds desugared rules		>>= \ (simplified, orphan_rules) ->

	-- Do the final tidy-up
    tidyCorePgm tidy_uniqs this_mod
		simplified orphan_rules		>>= \ (tidy_binds, tidy_orphan_rules) -> 

	-- Run the occurrence analyser one last time, so that
	-- dead binders get dead-binder info.  This is exploited by
	-- code generators to avoid spitting out redundant bindings.
	-- The occurrence-zapping in Simplify.simplCaseBinder means
	-- that the Simplifier nukes useful dead-var stuff especially
	-- in case patterns.
    let occ_anal_tidy_binds = occurAnalyseBinds tidy_binds in

    coreBindsSize occ_anal_tidy_binds `seq`
--	TEMP: the above call zaps some space usage allocated by the
--	simplifier, which for reasons I don't understand, persists
--	thoroughout code generation



	--------------------------  Convert to STG code -------------------------------
    show_pass "Core2Stg" 			>>
    _scc_     "Core2Stg"
    let
	stg_binds   = topCoreBindsToStg c2s_uniqs occ_anal_tidy_binds
    in

	--------------------------  Simplify STG code -------------------------------
    show_pass "Stg2Stg" 			 >>
    _scc_     "Stg2Stg"
    stg2stg stg_cmds this_mod st_uniqs stg_binds >>= \ (stg_binds2, cost_centre_info) ->

#ifdef GHCI
    runStgI local_tycons local_classes 
                         (map fst stg_binds2)    >>= \ i_result ->
    putStr ("\nANSWER = " ++ show i_result ++ "\n\n")
    >>

#else
	--------------------------  Interface file -------------------------------
	-- Dump instance decls and type signatures into the interface file
    _scc_     "Interface"
    let
	final_ids = collectFinalStgBinders (map fst stg_binds2)
    in
    writeIface this_mod old_iface new_iface
	       local_tycons local_classes inst_info
	       final_ids occ_anal_tidy_binds tidy_orphan_rules 		>>


	--------------------------  Code generation -------------------------------
    show_pass "CodeGen" 			>>
    _scc_     "CodeGen"
    codeGen this_mod imported_modules
	    cost_centre_info
	    fe_binders
	    local_tycons local_classes 
	    stg_binds2				>>= \ abstractC ->


	--------------------------  Code output -------------------------------
    show_pass "CodeOutput" 				>>
    _scc_     "CodeOutput"
    codeOutput this_mod local_tycons local_classes
	       occ_anal_tidy_binds stg_binds2
	       c_code h_code abstractC 
	       ncg_uniqs				>>


	--------------------------  Final report -------------------------------
    reportCompile mod_name (showSDoc (ppSourceStats True rdr_module)) >>

#endif


    ghcExit 0
    } }
  where
    -------------------------------------------------------------
    -- ****** help functions:

    show_pass
      = if opt_D_show_passes
	then \ what -> hPutStr stderr ("*** "++what++":\n")
	else \ what -> return ()
\end{code}


%************************************************************************
%*									*
\subsection{Initial persistent state}
%*									*
%************************************************************************

\begin{code}
initPersistentCompilerState :: PersistentCompilerState
initPersistentCompilerState 
  = PCS { pcs_PST   = initPackageDetails,
	  pcs_insts = emptyInstEnv,
	  pcs_rules = emptyRuleEnv,
	  pcs_PRS   = initPersistentRenamerState }

initPackageDetails :: PackageSymbolTable
initPackageDetails = extendTypeEnv emptyModuleEnv wiredInThings

initPersistentRenamerState :: PersistentRenamerState
  = PRS { prsOrig  = Orig { origNames  = initOrigNames,
			    origIParam = emptyFM },
	  prsDecls = emptyNameEnv,
	  prsInsts = emptyBag,
	  prsRules = emptyBag
    }

initOrigNames :: FiniteMap (ModuleName,OccName) Name
initOrigNames = grab knownKeyNames `plusFM` grab (map getName wiredInThings)
	      where
		grab names   = foldl add emptyFM names
		add env name = addToFM env (moduleName (nameModule name), nameOccName name) name
\end{code}
