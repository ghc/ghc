%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
module Main ( main ) where

#include "HsVersions.h"

import IO		( hPutStr, stderr )
import HsSyn
import BasicTypes	( NewOrData(..) )

import RdrHsSyn		( RdrNameHsModule )
import FastString	( mkFastCharString, unpackFS )
import StringBuffer	( hGetStringBuffer )
import Parser		( parse )
import Lex		( PState(..), P, ParseResult(..) )
import SrcLoc		( mkSrcLoc )

import Rename		( renameModule )

import MkIface		( startIface, ifaceDecls, endIface )
import TcModule		( TcResults(..), typecheckModule )
import Desugar		( deSugar )
import SimplCore	( core2core )
import CoreLint		( endPass )
import CoreSyn		( coreBindsSize )
import CoreTidy		( tidyCorePgm )
import CoreToStg	( topCoreBindsToStg )
import StgSyn		( collectFinalStgBinders, pprStgBindings )
import SimplStg		( stg2stg )
import CodeGen		( codeGen )
import CodeOutput	( codeOutput )

import Module		( ModuleName, moduleNameUserString )
import AbsCSyn		( absCNop )
import CmdLineOpts
import ErrUtils		( ghcExit, doIfSet, dumpIfSet )
import Maybes		( maybeToBool, MaybeErr(..) )
import TyCon		( isDataTyCon )
import Class		( classTyCon )
import UniqSupply	( mkSplitUniqSupply )

import FiniteMap	( emptyFM )
import Outputable
import Char		( isSpace )
#if REPORT_TO_MOTHERLODE && __GLASGOW_HASKELL__ >= 303
import SocketPrim
import BSD
import IOExts		( unsafePerformIO )
import NativeInfo       ( os, arch )
#endif

\end{code}

\begin{code}
main =
 --  _scc_ "main" 
 doIt classifyOpts
\end{code}

\begin{code}
parseModule :: IO (ModuleName, RdrNameHsModule)
parseModule = do
    buf <- hGetStringBuffer True{-expand tabs-} (unpackFS src_filename)
    case parse buf PState{ bol = 0#, atbol = 1#,
		           context = [], glasgow_exts = glaexts,
		           loc = mkSrcLoc src_filename 1 } of

	PFailed err -> do
		printErrs err
		ghcExit 1
		return (error "parseModule") -- just to get the types right

	POk _ m@(HsModule mod _ _ _ _ _) -> 
		return (mod, m)
  where
	glaexts | opt_GlasgowExts = 1#
		| otherwise       = 0#
\end{code}

\begin{code}
doIt :: ([CoreToDo], [StgToDo]) -> IO ()

doIt (core_cmds, stg_cmds)
  = doIfSet opt_Verbose 
	(hPutStr stderr "Glasgow Haskell Compiler, version " 	>>
 	 hPutStr stderr compiler_version                    	>>
	 hPutStr stderr ", for Haskell 98, compiled by GHC version " >>
	 hPutStr stderr booter_version				>>
	 hPutStr stderr "\n")					>>

	--------------------------  Reader  ----------------
    show_pass "Parser"	>>
    _scc_     "Parser"
    parseModule		>>= \ (mod_name, rdr_module) ->

    dumpIfSet opt_D_dump_parsed "Parser" (ppr rdr_module) >>

    dumpIfSet opt_D_source_stats "Source Statistics"
	(ppSourceStats False rdr_module)	 	>>

    -- UniqueSupplies for later use (these are the only lower case uniques)
    mkSplitUniqSupply 'r'	>>= \ rn_uniqs 	-> -- renamer
    mkSplitUniqSupply 'a'	>>= \ tc_uniqs 	-> -- typechecker
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
	
	Just (this_mod, rn_mod, iface_file_stuff, rn_name_supply, imported_modules) ->
			-- Oh well, we've got to recompile for real


	--------------------------  Start interface file  ----------------
    -- Safely past renaming: we can start the interface file:
    -- (the iface file is produced incrementally, as we have
    -- the information that we need...; we use "iface<blah>")
    -- "endIface" finishes the job.
    startIface this_mod iface_file_stuff	>>= \ if_handle ->


	--------------------------  Typechecking ----------------
    show_pass "TypeCheck" 				>>
    _scc_     "TypeCheck"
    typecheckModule tc_uniqs rn_name_supply
		    iface_file_stuff rn_mod	        >>= \ maybe_tc_stuff ->
    case maybe_tc_stuff of {
	Nothing -> ghcExit 1;	-- Type checker failed

	Just (tc_results@(TcResults {tc_tycons  = local_tycons, 
		   	 	     tc_classes = local_classes, 
		   	 	     tc_insts   = inst_info })) ->


	--------------------------  Desugaring ----------------
    _scc_     "DeSugar"
    deSugar this_mod ds_uniqs tc_results	>>= \ (desugared, rules, h_code, c_code) ->


	--------------------------  Main Core-language transformations ----------------
    _scc_     "Core2Core"
    core2core core_cmds desugared rules			>>= \ (simplified, imp_rule_ids) ->

	-- Do the final tidy-up
    tidyCorePgm tidy_uniqs this_mod
		simplified imp_rule_ids			>>= \ (tidy_binds, tidy_imp_rule_ids) -> 


	--------------------------  Convert to STG code -------------------------------
    show_pass "Core2Stg" 			>>
    _scc_     "Core2Stg"
    let
	stg_binds   = topCoreBindsToStg c2s_uniqs tidy_binds
    in

	--------------------------  Simplify STG code -------------------------------
    show_pass "Stg2Stg" 			>>
    _scc_     "Stg2Stg"
    stg2stg stg_cmds this_mod st_uniqs stg_binds >>= \ (stg_binds2, cost_centre_info) ->


	--------------------------  Interface file -------------------------------
	-- Dump instance decls and type signatures into the interface file
    _scc_     "Interface"
    let
	final_ids = collectFinalStgBinders (map fst stg_binds2)
    in
    coreBindsSize tidy_binds `seq`
--	TEMP: the above call zaps some space usage allocated by the
--	simplifier, which for reasons I don't understand, persists
--	thoroughout code generation

    ifaceDecls if_handle local_tycons local_classes 
	       inst_info final_ids tidy_binds imp_rule_ids	>>
    endIface if_handle						>>
	    -- We are definitely done w/ interface-file stuff at this point:
	    -- (See comments near call to "startIface".)


	--------------------------  Code generation -------------------------------
    show_pass "CodeGen" 			>>
    _scc_     "CodeGen"
    codeGen this_mod imported_modules
	    cost_centre_info
	    local_tycons local_classes 
	    stg_binds2				>>= \ abstractC ->


	--------------------------  Code output -------------------------------
    show_pass "CodeOutput" 				>>
    _scc_     "CodeOutput"
    codeOutput this_mod c_code h_code abstractC ncg_uniqs	>>


	--------------------------  Final report -------------------------------
    reportCompile mod_name (showSDoc (ppSourceStats True rdr_module)) >>

    ghcExit 0
    } }
  where
    -------------------------------------------------------------
    -- ****** help functions:

    show_pass
      = if opt_D_show_passes
	then \ what -> hPutStr stderr ("*** "++what++":\n")
	else \ what -> return ()

ppSourceStats short (HsModule name version exports imports decls src_loc)
 = (if short then hcat else vcat)
        (map pp_val
	       [("ExportAll        ", export_all), -- 1 if no export list
		("ExportDecls      ", export_ds),
		("ExportModules    ", export_ms),
		("Imports          ", import_no),
		("  ImpQual        ", import_qual),
		("  ImpAs          ", import_as),
		("  ImpAll         ", import_all),
		("  ImpPartial     ", import_partial),
		("  ImpHiding      ", import_hiding),
		("FixityDecls      ", fixity_ds),
		("DefaultDecls     ", default_ds),
	      	("TypeDecls        ", type_ds),
	      	("DataDecls        ", data_ds),
	      	("NewTypeDecls     ", newt_ds),
	      	("DataConstrs      ", data_constrs),
		("DataDerivings    ", data_derivs),
	      	("ClassDecls       ", class_ds),
	      	("ClassMethods     ", class_method_ds),
	      	("DefaultMethods   ", default_method_ds),
	      	("InstDecls        ", inst_ds),
	      	("InstMethods      ", inst_method_ds),
	      	("TypeSigs         ", bind_tys),
	      	("ValBinds         ", val_bind_ds),
	      	("FunBinds         ", fn_bind_ds),
	      	("InlineMeths      ", method_inlines),
		("InlineBinds      ", bind_inlines),
--	      	("SpecialisedData  ", data_specs),
--	      	("SpecialisedInsts ", inst_specs),
	      	("SpecialisedMeths ", method_specs),
	      	("SpecialisedBinds ", bind_specs)
	       ])
  where
    pp_val (str, 0) = empty
    pp_val (str, n) 
      | not short   = hcat [text str, int n]
      | otherwise   = hcat [text (trim str), equals, int n, semi]
    
    trim ls     = takeWhile (not.isSpace) (dropWhile isSpace ls)

    fixity_ds   = length [() | FixD d <- decls]
		-- NB: this omits fixity decls on local bindings and
		-- in class decls.  ToDo

    tycl_decls  = [d | TyClD d <- decls]
    (class_ds, data_ds, newt_ds, type_ds) = countTyClDecls tycl_decls

    inst_decls  = [d | InstD d <- decls]
    inst_ds     = length inst_decls
    default_ds  = length [() | DefD _ <- decls]
    val_decls   = [d | ValD d <- decls]

    real_exports = case exports of { Nothing -> []; Just es -> es }
    n_exports  	 = length real_exports
    export_ms  	 = length [() | IEModuleContents _ <- real_exports]
    export_ds  	 = n_exports - export_ms
    export_all 	 = case exports of { Nothing -> 1; other -> 0 }

    (val_bind_ds, fn_bind_ds, bind_tys, bind_specs, bind_inlines)
	= count_binds (foldr ThenBinds EmptyBinds val_decls)

    (import_no, import_qual, import_as, import_all, import_partial, import_hiding)
	= foldr add6 (0,0,0,0,0,0) (map import_info imports)
    (data_constrs, data_derivs)
	= foldr add2 (0,0) (map data_info tycl_decls)
    (class_method_ds, default_method_ds)
	= foldr add2 (0,0) (map class_info tycl_decls)
    (inst_method_ds, method_specs, method_inlines)
	= foldr add3 (0,0,0) (map inst_info inst_decls)


    count_binds EmptyBinds        = (0,0,0,0,0)
    count_binds (ThenBinds b1 b2) = count_binds b1 `add5` count_binds b2
    count_binds (MonoBind b sigs _) = case (count_monobinds b, count_sigs sigs) of
				        ((vs,fs),(ts,_,ss,is)) -> (vs,fs,ts,ss,is)

    count_monobinds EmptyMonoBinds	  	   = (0,0)
    count_monobinds (AndMonoBinds b1 b2)  	   = count_monobinds b1 `add2` count_monobinds b2
    count_monobinds (PatMonoBind (VarPatIn n) r _) = (1,0)
    count_monobinds (PatMonoBind p r _)            = (0,1)
    count_monobinds (FunMonoBind f _ m _)          = (0,1)

    count_sigs sigs = foldr add4 (0,0,0,0) (map sig_info sigs)

    sig_info (Sig _ _ _)          = (1,0,0,0)
    sig_info (ClassOpSig _ _ _ _) = (0,1,0,0)
    sig_info (SpecSig _ _ _)      = (0,0,1,0)
    sig_info (InlineSig _ _ _)    = (0,0,0,1)
    sig_info (NoInlineSig _ _ _)  = (0,0,0,1)
    sig_info _                    = (0,0,0,0)

    import_info (ImportDecl _ _ qual as spec _)
	= add6 (1, qual_info qual, as_info as, 0,0,0) (spec_info spec)
    qual_info False  = 0
    qual_info True   = 1
    as_info Nothing  = 0
    as_info (Just _) = 1
    spec_info Nothing 	        = (0,0,0,1,0,0)
    spec_info (Just (False, _)) = (0,0,0,0,1,0)
    spec_info (Just (True, _))  = (0,0,0,0,0,1)

    data_info (TyData _ _ _ _ constrs derivs _ _)
	= (length constrs, case derivs of {Nothing -> 0; Just ds -> length ds})
    data_info other = (0,0)

    class_info (ClassDecl _ _ _ meth_sigs def_meths _ _ _ _ _)
	= case count_sigs meth_sigs of
	    (_,classops,_,_) ->
	       (classops, addpr (count_monobinds def_meths))
    class_info other = (0,0)

    inst_info (InstDecl _ inst_meths inst_sigs _ _)
	= case count_sigs inst_sigs of
	    (_,_,ss,is) ->
	       (addpr (count_monobinds inst_meths), ss, is)

    addpr :: (Int,Int) -> Int
    add1  :: Int -> Int -> Int
    add2  :: (Int,Int) -> (Int,Int) -> (Int, Int)
    add3  :: (Int,Int,Int) -> (Int,Int,Int) -> (Int, Int, Int)
    add4  :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> (Int, Int, Int, Int)
    add5  :: (Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int) -> (Int, Int, Int, Int, Int)
    add6  :: (Int,Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int) -> (Int, Int, Int, Int, Int, Int)

    addpr (x,y) = x+y
    add1 x1 y1  = x1+y1
    add2 (x1,x2) (y1,y2) = (x1+y1,x2+y2)
    add3 (x1,x2,x3) (y1,y2,y3) = (x1+y1,x2+y2,x3+y3)
    add4 (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1+y1,x2+y2,x3+y3,x4+y4)
    add5 (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (x1+y1,x2+y2,x3+y3,x4+y4,x5+y5)
    add6 (x1,x2,x3,x4,x5,x6) (y1,y2,y3,y4,y5,y6) = (x1+y1,x2+y2,x3+y3,x4+y4,x5+y5,x6+y6)
\end{code}

\begin{code}
compiler_version :: String
compiler_version =
     case (show opt_HiVersion) of
	[x]	 -> ['0','.',x]
	ls@[x,y] -> "0." ++ ls
	ls       -> go ls
 where
  -- 10232353 => 10232.53
  go ls@[x,y] = '.':ls
  go (x:xs)   = x:go xs

booter_version
 = case "\ 
	\ __GLASGOW_HASKELL__" of
    ' ':n:ns -> n:'.':ns
    ' ':m    -> m
\end{code}

\begin{code}
reportCompile :: ModuleName -> String -> IO ()
#if REPORT_TO_MOTHERLODE && __GLASGOW_HASKELL__ >= 303
reportCompile mod_name info
  | not opt_ReportCompile = return ()
  | otherwise = (do 
      sock <- udpSocket 0
      addr <- motherShip
      sendTo sock (moduleNameUserString mod_name ++ ';': compiler_version ++ 
		   ';': os ++ ';':arch ++ '\n':' ':info ++ "\n") addr
      return ()) `catch` (\ _ -> return ())

motherShip :: IO SockAddr
motherShip = do
  he <- getHostByName "laysan.dcs.gla.ac.uk"
  case (hostAddresses he) of
    []    -> IOERROR (userError "No address!")
    (x:_) -> return (SockAddrInet motherShipPort x)

--magick
motherShipPort :: PortNumber
motherShipPort = mkPortNumber 12345

-- creates a socket capable of sending datagrams,
-- binding it to a port
--  ( 0 => have the system pick next available port no.)
udpSocket :: Int -> IO Socket
udpSocket p = do
  pr <- getProtocolNumber "udp"
  s  <- socket AF_INET Datagram pr
  bindSocket s (SockAddrInet (mkPortNumber p) iNADDR_ANY)
  return s
#else
reportCompile _ _ = return ()
#endif

\end{code}
