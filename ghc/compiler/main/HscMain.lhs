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
import PrelRules	( builtinRules )
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

hscMain dflags core_cmds stg_cmds summary maybe_old_iface
	output_filename mod_details pcs1 
 = do
      source_unchanged :: Bool -- extracted from summary?

      (pcs2, check_errs, (recomp_reqd, maybe_checked_iface))
         <- checkOldIface dflags finder hit hst pcs1 mod source_unchanged
                          maybe_old_iface

      -- test check_errs and give up if a problem happened
      what_next = if recomp_reqd then hscRecomp else hscNoRecomp

      return $
         what_next dflags core_cmds stg_cmds summary hit hst 
                   pcs2 maybe_checked_iface

hscNoRecomp = panic "hscNoRecomp"

hscRecomp dflags core_cmds stg_cmds summary hit hst pcs maybe_old_iface
 = do 
      -- parsed :: RdrNameHsModule
      parsed <- parseModule summary
      -- check for parse errors

      (pcs_rn, maybe_rn_result) 
         <- renameModule dflags finder hit hst pcs mod parsed

      -- check maybe_rn_result for failure

      (new_iface, rn_hs_decls) = unJust maybe_rn_result

      maybe_tc_result
         <- typecheckModule dflags mod pcs hst hit pit rn_hs_decls

      -- check maybe_tc_result for failure
      let tc_result = unJust maybe_tc_result
      let tc_pcs = tc_pcs tc_result
      let tc_env = tc_env tc_result
      let tc_binds = tc_binds tc_result
      let local_tycons = tc_tycons tc_result
      let local_classes = tc_classes tc_result

      -- desugar, simplify and tidy, to create the unfoldings
      -- why is this IO-typed?
      (tidy_binds, orphan_rules, fe_binders, h_code, c_code)   -- return modDetails?
         <- dsThenSimplThenTidy dflags mod tc_result rule_base ds_uniqs

      -- convert to Stg; needed for binders
      (stg_binds, cost_centre_info, top_level_ids) 
         <- myCoreToStg c2s_uniqs st_uniqs this_mod tidy_binds

      -- cook up a new ModDetails now we (finally) have all the bits
      let new_details = completeModDetails tc_env tidy_binds top_level_ids orphan_rules

      -- and possibly create a new ModIface
      let maybe_final_iface = completeIface maybe_old_iface new_iface new_details 

      -- do the rest of code generation/emission
      -- this is obviously nonsensical: FIX
      (unlinkeds, stub_h_filename, stub_c_filename) 
         <- restOfCodeGeneration this_mod imported_modules cost_centre_info 
                                 fe_binders local_tycons local_classes stg_binds

      -- and the answer is ...
      return (HscOK new_details maybe_final_iface stub_h_filename stub_c_filename
                    unlinkeds tc_pcs (unionBags rn_warns tc_warns))


restOfCodeGeneration this_mod imported_modules cost_centre_info 
                     fe_binders local_tycons local_classes stg_binds
 = do --------------------------  Code generation -------------------------------
      show_pass "CodeGen"
      -- _scc_     "CodeGen"
      abstractC <- codeGen this_mod imported_modules
                           cost_centre_info fe_binders
                           local_tycons local_classes stg_binds

      --------------------------  Code output -------------------------------
      show_pass "CodeOutput"
      -- _scc_     "CodeOutput"
      (maybe_stub_h_name, maybe_stub_c_name)
         <- codeOutput this_mod local_tycons local_classes
                       occ_anal_tidy_binds stg_binds2
                       c_code h_code abstractC ncg_uniqs

      -- this is obviously nonsensical: FIX
      return (maybe_stub_h_name, maybe_stub_c_name, [])


dsThenSimplThenTidy dflags mod tc_result rule_base ds_uniqs
 = do --------------------------  Desugaring ----------------
      -- _scc_     "DeSugar"
      (desugared, rules, h_code, c_code, fe_binders) 
         <- deSugar this_mod ds_uniqs tc_results

      --------------------------  Main Core-language transformations ----------------
      -- _scc_     "Core2Core"
      (simplified, orphan_rules)  <- core2core core_cmds desugared rules

      -- Do the final tidy-up
      (tidy_binds, tidy_orphan_rules) 
         <- tidyCorePgm this_mod simplified orphan_rules
      
      return (tidy_binds, tidy_orphan_rules, fe_binders, h_code, c_code)



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

#if 0
-- BEGIN old stuff
	--------------------------  Reader  ----------------
    show_pass "Parser"	>>
    _scc_     "Parser"

    let src_filename -- name of the preprocessed source file
       = case ms_ppsource summary of
            Just (filename, fingerprint) -> filename
            Nothing -> pprPanic "hscMain:summary is not of a source module"
                                (ppr summary)

    buf <- hGetStringBuffer True{-expand tabs-} src_filename

    let glaexts | dopt Opt_GlasgowExts dflags = 1#
		| otherwise 		      = 0#

    case parse buf PState{ bol = 0#, atbol = 1#,
		           context = [], glasgow_exts = glaexts,
		           loc = mkSrcLoc src_filename 1 } of {

	PFailed err -> return (HscErrs pcs (unitBag err) emptyBag)

	POk _ rdr_module@(HsModule mod_name _ _ _ _ _ _) ->

    dumpIfSet (dopt_D_dump_parsed flags) "Parser" (ppr rdr_module) >>

    dumpIfSet (dopt_D_source_stats flags) "Source Statistics"
	(ppSourceStats False rdr_module)	 	>>

    -- UniqueSupplies for later use (these are the only lower case uniques)
    mkSplitUniqSupply 'd'	>>= \ ds_uniqs 	-> -- desugarer
    mkSplitUniqSupply 'r'	>>= \ ru_uniqs 	-> -- rules
    mkSplitUniqSupply 'c'	>>= \ c2s_uniqs -> -- core-to-stg
    mkSplitUniqSupply 'g'	>>= \ st_uniqs  -> -- stg-to-stg passes
    mkSplitUniqSupply 'n'	>>= \ ncg_uniqs -> -- native-code generator

	--------------------------  Rename  ----------------
    show_pass "Renamer" 			>>
    _scc_     "Renamer"

    renameModule dflags finder pcs hst rdr_module	
						>>= \ (pcs_rn, maybe_rn_stuff) ->
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
    tidyCorePgm this_mod
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
-- END old stuff
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
<<<<<<< HscMain.lhs
  = do prs <- initPersistentRenamerState
       return (
        PCS { pcs_PST   = initPackageDetails,
	      pcs_insts = emptyInstEnv,
	      pcs_rules = emptyRuleEnv,
	      pcs_PRS   = initPersistentRenamerState 
            }
        )
=======
  = PCS { pcs_PST   = initPackageDetails,
	  pcs_insts = emptyInstEnv,
	  pcs_rules = initRules,
	  pcs_PRS   = initPersistentRenamerState }
>>>>>>> 1.12

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


initRules :: RuleEnv
initRules = foldl add emptyVarEnv builtinRules
	  where
	    add env (name,rule) = extendNameEnv_C add1 env name [rule]
	    add1 rules _	= rule : rules
\end{code}



\begin{code}
writeIface this_mod old_iface new_iface
	   local_tycons local_classes inst_info
	   final_ids tidy_binds tidy_orphan_rules
  = 
    if isNothing opt_HiDir && isNothing opt_HiFile
	then return ()  -- not producing any .hi file
	else 

    let 
	hi_suf = case opt_HiSuf of { Nothing -> "hi"; Just suf -> suf }
	filename = case opt_HiFile of {
			Just f  -> f;
			Nothing -> 
		   case opt_HiDir of {
			Just dir -> dir ++ '/':moduleUserString this_mod 
					++ '.':hi_suf;
			Nothing  -> panic "writeIface"
		}}
    in

    do maybe_final_iface <- checkIface old_iface full_new_iface 	
       case maybe_final_iface of {
	  Nothing -> when opt_D_dump_rn_trace $
		     putStrLn "Interface file unchanged" ;  -- No need to update .hi file

	  Just final_iface ->

       do  let mod_vers_unchanged = case old_iface of
				      Just iface -> pi_vers iface == pi_vers final_iface
				      Nothing -> False
     	   when (mod_vers_unchanged && opt_D_dump_rn_trace) $
	        putStrLn "Module version unchanged, but usages differ; hence need new hi file"

	   if_hdl <- openFile filename WriteMode
	   printForIface if_hdl (pprIface final_iface)
	   hClose if_hdl
    }   
  where
    full_new_iface = completeIface new_iface local_tycons local_classes
				   	     inst_info final_ids tidy_binds
					     tidy_orphan_rules
\end{code}


%************************************************************************
%*				 					*
\subsection{Printing the interface}
%*				 					*
%************************************************************************

\begin{code}
pprIface (ParsedIface { pi_mod = mod, pi_vers = mod_vers, pi_orphan = orphan,
			pi_usages = usages, pi_exports = exports, 
			pi_fixity = (fix_vers, fixities),
			pi_insts = insts, pi_decls = decls, 
			pi_rules = (rule_vers, rules), pi_deprecs = deprecs })
 = vcat [ ptext SLIT("__interface")
		<+> doubleQuotes (ptext opt_InPackage)
		<+> ppr mod <+> ppr mod_vers <+> pp_sub_vers
		<+> (if orphan then char '!' else empty)
		<+> int opt_HiVersion
		<+> ptext SLIT("where")
	, vcat (map pprExport exports)
	, vcat (map pprUsage usages)
	, pprFixities fixities
	, vcat [ppr i <+> semi | i <- insts]
	, vcat [ppr_vers v <+> ppr d <> semi | (v,d) <- decls]
	, pprRules rules
	, pprDeprecs deprecs
	]
  where
    ppr_vers v | v == initialVersion = empty
	       | otherwise	     = int v
    pp_sub_vers 
	| fix_vers == initialVersion && rule_vers == initialVersion = empty
	| otherwise = brackets (ppr fix_vers <+> ppr rule_vers)
\end{code}

When printing export lists, we print like this:
	Avail   f		f
	AvailTC C [C, x, y]	C(x,y)
	AvailTC C [x, y]	C!(x,y)		-- Exporting x, y but not C

\begin{code}
pprExport :: ExportItem -> SDoc
pprExport (mod, items)
 = hsep [ ptext SLIT("__export "), ppr mod, hsep (map upp_avail items) ] <> semi
  where
    upp_avail :: RdrAvailInfo -> SDoc
    upp_avail (Avail name)      = pprOccName name
    upp_avail (AvailTC name []) = empty
    upp_avail (AvailTC name ns) = hcat [pprOccName name, bang, upp_export ns']
				where
				  bang | name `elem` ns = empty
				       | otherwise	= char '|'
				  ns' = filter (/= name) ns
    
    upp_export []    = empty
    upp_export names = braces (hsep (map pprOccName names))
\end{code}


\begin{code}
pprUsage :: ImportVersion OccName -> SDoc
pprUsage (m, has_orphans, is_boot, whats_imported)
  = hsep [ptext SLIT("import"), pprModuleName m, 
	  pp_orphan, pp_boot,
	  upp_import_versions whats_imported
    ] <> semi
  where
    pp_orphan | has_orphans = char '!'
	      | otherwise   = empty
    pp_boot   | is_boot     = char '@'
              | otherwise   = empty

	-- Importing the whole module is indicated by an empty list
    upp_import_versions NothingAtAll   = empty
    upp_import_versions (Everything v) = dcolon <+> int v
    upp_import_versions (Specifically vm vf vr nvs)
      = dcolon <+> int vm <+> int vf <+> int vr <+> hsep [ ppr n <+> int v | (n,v) <- nvs ]
\end{code}


\begin{code}
pprFixities []    = empty
pprFixities fixes = hsep (map ppr fixes) <> semi

pprRules []    = empty
pprRules rules = hsep [ptext SLIT("{-## __R"), hsep (map ppr rules), ptext SLIT("##-}")]

pprDeprecs []   = empty
pprDeprecs deps = hsep [ ptext SLIT("{-## __D"), guts, ptext SLIT("##-}")]
		where
		  guts = hsep [ ppr ie <+> doubleQuotes (ppr txt) <> semi 
			      | Deprecation ie txt _ <- deps ]
\end{code}


