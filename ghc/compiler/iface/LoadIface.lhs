%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Dealing with interface files}

\begin{code}
module LoadIface (
	loadHomeInterface, loadInterface, loadSysInterface,
	loadSrcInterface, loadOrphanModules,
	readIface,	-- Used when reading the module's old interface
	predInstGates, ifaceInstGates, ifaceStats,
	initExternalPackageState
   ) where

#include "HsVersions.h"

import DriverState	( v_GhcMode, isCompManagerMode )
import DriverUtil	( replaceFilenameSuffix )
import CmdLineOpts	( DynFlags( verbosity ), DynFlag( Opt_IgnoreInterfacePragmas ), 
			  opt_InPackage )
import Parser		( parseIface )

import IfaceSyn		( IfaceDecl(..), IfaceConDecl(..), IfaceClassOp(..), IfaceInst(..), 
			  IfaceRule(..), IfaceExpr(..), IfaceTyCon(..), IfaceIdInfo(..),
			  IfaceType(..), IfacePredType(..), IfaceExtName, mkIfaceExtName )
import IfaceEnv		( newGlobalBinder, lookupIfaceExt, lookupIfaceTc )
import HscTypes		( HscEnv(..), ModIface(..), emptyModIface,
			  ExternalPackageState(..), emptyTypeEnv, emptyPool, 
			  lookupIfaceByModName, emptyPackageIfaceTable,
			  IsBootInterface, mkIfaceFixCache, 
			  Pool(..), DeclPool, InstPool, 
			  RulePool, addRuleToPool, RulePoolContents
			 )

import BasicTypes	( Version, Fixity(..), FixityDirection(..) )
import TcType		( Type, tcSplitTyConApp_maybe )
import Type		( funTyCon )
import TcRnMonad

import PrelNames	( gHC_PRIM_Name )
import PrelInfo		( ghcPrimExports )
import PrelRules	( builtinRules )
import Rules		( emptyRuleBase )
import InstEnv		( emptyInstEnv )
import Name		( Name {-instance NamedThing-}, getOccName,
			  nameModuleName, isInternalName )
import NameEnv
import MkId		( seqId )
import Packages		( basePackage )
import Module		( Module, ModuleName, ModLocation(ml_hi_file),
			  moduleName, isHomeModule, moduleEnvElts,
			  extendModuleEnv, lookupModuleEnvByName, moduleUserString
			)
import OccName		( OccName, mkClassTyConOcc, mkClassDataConOcc,
			  mkSuperDictSelOcc, 
			  mkDataConWrapperOcc, mkDataConWorkerOcc )
import Class		( Class, className )
import TyCon		( DataConDetails(..), tyConName )
import SrcLoc		( mkSrcLoc, importedSrcLoc )
import Maybes		( isJust, mapCatMaybes )
import StringBuffer     ( hGetStringBuffer )
import FastString	( mkFastString )
import ErrUtils         ( Message )
import Finder		( findModule, findPackageModule, 
			  hiBootExt, hiBootVerExt )
import Lexer
import Outputable
import BinIface		( readBinIface )
import Panic

import DATA_IOREF	( readIORef )

import Directory
\end{code}


%************************************************************************
%*									*
		loadSrcInterface, loadOrphanModules

		These two are called from TcM-land	
%*									*
%************************************************************************

\begin{code}
loadSrcInterface :: SDoc -> ModuleName -> IsBootInterface -> RnM ModIface
-- This is called for each 'import' declaration in the source code
-- On a failure, fail in the mnad with an error message

loadSrcInterface doc mod_name want_boot
  = do 	{ mb_iface <- initIfaceTcRn $ loadInterface doc mod_name 
					   (ImportByUser want_boot)
	; case mb_iface of
	    Left err    -> failWithTc (elaborate err) 
	    Right iface -> return iface
	}
  where
    elaborate err = hang (ptext SLIT("Failed to load interface for") <+> 
			 quotes (ppr mod_name) <> colon) 4 err

loadOrphanModules :: [ModuleName] -> TcM ()
loadOrphanModules mods
  | null mods = returnM ()
  | otherwise = initIfaceTcRn $
		do { traceIf (text "Loading orphan modules:" <+> 
		     		 fsep (map ppr mods))
		   ; mappM_ load mods
		   ; returnM () }
  where
    load mod   = loadSysInterface (mk_doc mod) mod
    mk_doc mod = ppr mod <+> ptext SLIT("is a orphan-instance module")
\end{code}

%*********************************************************
%*							*
		loadHomeInterface
		Called from Iface-land
%*							*
%*********************************************************

\begin{code}
loadHomeInterface :: SDoc -> Name -> IfM lcl ModIface
loadHomeInterface doc name
  = ASSERT2( not (isInternalName name), ppr name <+> parens doc )
    loadSysInterface doc (nameModuleName name)

loadSysInterface :: SDoc -> ModuleName -> IfM lcl ModIface
-- A wrapper for loadInterface that Throws an exception if it fails
loadSysInterface doc mod_name
  = do	{ mb_iface <- loadInterface doc mod_name ImportBySystem
	; case mb_iface of 
	    Left err    -> ghcError (ProgramError (showSDoc err))
	    Right iface -> return iface }
\end{code}


%*********************************************************
%*							*
		loadInterface

	The main function to load an interface
	for an imported module, and put it in
	the External Package State
%*							*
%*********************************************************

\begin{code}
loadInterface :: SDoc -> ModuleName -> WhereFrom 
	      -> IfM lcl (Either Message ModIface)
-- If it can't find a suitable interface file, we
--	a) modify the PackageIfaceTable to have an empty entry
--		(to avoid repeated complaints)
--	b) return (Left message)
--
-- It's not necessarily an error for there not to be an interface
-- file -- perhaps the module has changed, and that interface 
-- is no longer used -- but the caller can deal with that by 
-- catching the exception

loadInterface doc_str mod_name from
  = do	{ 	-- Read the state
	  env <- getTopEnv 
	; let { hpt	= hsc_HPT env
	      ; eps_var = hsc_EPS env }
	; eps <- readMutVar eps_var
	; let { pit = eps_PIT eps }

		-- Check whether we have the interface already
	; case lookupIfaceByModName hpt pit mod_name of {
	    Just iface 
		-> returnM (Right iface) ;	-- Already loaded
			-- The (src_imp == mi_boot iface) test checks that the already-loaded
			-- interface isn't a boot iface.  This can conceivably happen,
			-- if an earlier import had a before we got to real imports.   I think.
	    other -> do

	{ if_gbl_env <- getGblEnv
	; let { hi_boot_file = case from of
				ImportByUser usr_boot -> usr_boot
				ImportBySystem        -> sys_boot

	      ; mb_dep   = lookupModuleEnvByName (if_is_boot if_gbl_env) mod_name
	      ; sys_boot = case mb_dep of
				Just (_, is_boot) -> is_boot
				Nothing		  -> False
			-- The boot-ness of the requested interface, 
	      }		-- based on the dependencies in directly-imported modules

	-- READ THE MODULE IN
	; read_result <- findAndReadIface doc_str mod_name hi_boot_file
	; case read_result of {
	    Left err -> do
	  	{ let {	-- Not found, so add an empty iface to 
			-- the EPS map so that we don't look again
		      	fake_iface = emptyModIface opt_InPackage mod_name
		      ; new_pit    = extendModuleEnv pit (mi_module fake_iface) fake_iface
		      ; new_eps    = eps { eps_PIT = new_pit } }
		; writeMutVar eps_var new_eps
		; returnM (Left err) } ;

	-- Found and parsed!
	    Right iface -> 

	let { mod = mi_module iface } in

	-- Sanity check.  If we're system-importing a module we know nothing at all
	-- about, it should be from a different package to this one
	WARN(   case from of { ImportBySystem -> True; other -> False } &&
		not (isJust mb_dep) && 
	 	isHomeModule mod,
		ppr mod $$ ppr mb_dep)

	initIfaceLcl (moduleName mod) $ do
	-- 	Load the new ModIface into the External Package State
	-- Even home-package interfaces loaded by loadInterface 
	-- 	(which only happens in OneShot mode; in Batch/Interactive 
	--  	mode, home-package modules are loaded one by one into the HPT)
	-- are put in the EPS.
	--
	-- The main thing is to add the ModIface to the PIT, but
	-- we also take the
	--	IfaceDecls, IfaceInst, IfaceRules
	-- out of the ModIface and put them into the big EPS pools

	-- NB: *first* we do loadDecl, so that the provenance of all the locally-defined
	---    names is done correctly (notably, whether this is an .hi file or .hi-boot file).
	--     If we do loadExport first the wrong info gets into the cache (unless we
	-- 	explicitly tag each export which seems a bit of a bore)

	{ new_eps_decls <- loadDecls mod (eps_decls eps) (mi_decls iface)
	; new_eps_insts <- loadInsts mod (eps_insts eps) (mi_insts iface)
	; new_eps_rules <- loadRules mod (eps_rules eps) (mi_rules iface)

	; let {	final_iface = iface {	mi_decls = panic "No mi_decls in PIT",
					mi_insts = panic "No mi_insts in PIT",
					mi_rules = panic "No mi_rules in PIT" }

	      ;	new_eps = eps { eps_PIT	  = extendModuleEnv pit mod final_iface,
				eps_decls = new_eps_decls,
				eps_rules = new_eps_rules,
				eps_insts = new_eps_insts } }
	; writeMutVar eps_var new_eps
	; return (Right final_iface)
    }}}}}

-----------------------------------------------------
--	Loading type/class/value decls
-- We pass the full Module name here, replete with
-- its package info, so that we can build a Name for
-- each binder with the right package info in it
-- All subsequent lookups, including crucially lookups during typechecking
-- the declaration itself, will find the fully-glorious Name
-----------------------------------------------------

loadDecls :: Module -> DeclPool
	  -> [(Version, IfaceDecl)]
	  -> IfM lcl DeclPool
loadDecls mod (Pool decls_map n_in n_out) decls
  = do	{ ignore_prags <- doptM Opt_IgnoreInterfacePragmas
	; decls_map' <- foldlM (loadDecl ignore_prags mod) decls_map decls
	; returnM (Pool decls_map' (n_in + length decls) n_out) }

loadDecl ignore_prags mod decls_map (_version, decl)
  = do 	{ main_name <- mk_new_bndr Nothing (ifName decl)
	; let decl' | ignore_prags = zapIdInfo decl
		    | otherwise    = decl

	-- Populate the name cache with final versions of all the subordinate names
	; mapM_ (mk_new_bndr (Just main_name)) (ifaceDeclSubBndrs decl')

	-- Extend the decls pool with a mapping for the main name (only)
	; returnM (extendNameEnv decls_map main_name decl') }
  where
	-- mk_new_bndr allocates in the name cache the final canonical
	-- name for the thing, with the correct 
	--	* package info
	--	* parent
	--	* location
	-- imported name, to fix the module correctly in the cache
    mk_new_bndr mb_parent occ = newGlobalBinder mod occ mb_parent loc
    loc = importedSrcLoc (moduleUserString mod)

zapIdInfo decl@(IfaceId {ifIdInfo = HasInfo _}) = decl { ifIdInfo = DiscardedInfo }
zapIdInfo decl 		    			= decl
	-- Don't alter "NoInfo", just "HasInfo"

-----------------
ifaceDeclSubBndrs :: IfaceDecl -> [OccName]
-- *Excludes* the 'main' name, but *includes* the implicitly-bound names
-- Rather revolting, because it has to predict what gets bound

ifaceDeclSubBndrs (IfaceClass {ifCtxt = sc_ctxt, ifName = cls_occ, ifSigs = sigs })
  = [tc_occ, dc_occ] ++ 
    [op | IfaceClassOp op _ _ <- sigs] ++
    [mkSuperDictSelOcc n cls_occ | n <- [1..length sc_ctxt]] ++
	-- The worker and wrapper for the DataCon of the class TyCon
	-- are based off the data-con name
    [mkDataConWrapperOcc dc_occ, mkDataConWorkerOcc dc_occ]
  where
    tc_occ  = mkClassTyConOcc cls_occ
    dc_occ  = mkClassDataConOcc cls_occ	

ifaceDeclSubBndrs (IfaceData {ifCons = Unknown}) = []
ifaceDeclSubBndrs (IfaceData {ifCons = DataCons cons})
  = foldr ((++) . conDeclBndrs) [] cons

ifaceDeclSubBndrs other = []

conDeclBndrs (IfaceConDecl con_occ _ _ _ _ fields)
  = fields ++ 
    [con_occ, mkDataConWrapperOcc con_occ, mkDataConWorkerOcc con_occ]


-----------------------------------------------------
--	Loading instance decls
-----------------------------------------------------

loadInsts :: Module -> InstPool -> [IfaceInst] -> IfL InstPool
loadInsts mod (Pool pool n_in n_out) decls
  = do	{ new_pool <- foldlM (loadInstDecl (moduleName mod)) pool decls
 	; returnM (Pool new_pool
			(n_in + length decls) 
			n_out) }

loadInstDecl mod pool decl@(IfaceInst {ifInstHead = inst_ty})
  = do 	{
	-- Find out what type constructors and classes are "gates" for the
	-- instance declaration.  If all these "gates" are slurped in then
	-- we should slurp the instance decl too.
	-- 
	-- We *don't* want to count names in the context part as gates, though.
	-- For example:
	--		instance Foo a => Baz (T a) where ...
	--
	-- Here the gates are Baz and T, but *not* Foo.
	-- 
	-- HOWEVER: functional dependencies make things more complicated
	--	class C a b | a->b where ...
	--	instance C Foo Baz where ...
	-- Here, the gates are really only C and Foo, *not* Baz.
	-- That is, if C and Foo are visible, even if Baz isn't, we must
	-- slurp the decl.
	--
	-- Rather than take fundeps into account "properly", we just slurp
	-- if C is visible and *any one* of the Names in the types
	-- This is a slightly brutal approximation, but most instance decls
	-- are regular H98 ones and it's perfect for them.
	--
	-- NOTICE that we rename the type before extracting its free
	-- variables.  The free-variable finder for a renamed HsType 
	-- does the Right Thing for built-in syntax like [] and (,).
	  let { (cls_ext, tc_exts) = ifaceInstGates inst_ty }
	; cls <- lookupIfaceExt cls_ext
	; tcs <- mapM lookupIfaceTc tc_exts
	; let {	new_pool = extendNameEnv_C combine pool cls [(tcs, (mod,decl))]
	      ; combine old _ = (tcs,(mod,decl)) : old }
	; returnM new_pool
	}

-----------------------------------------------------
--	Loading Rules
-----------------------------------------------------

loadRules :: Module -> RulePool -> [IfaceRule] -> IfL RulePool
loadRules mod pool@(Pool rule_pool n_in n_out) rules
  = do	{ ignore_prags <- doptM Opt_IgnoreInterfacePragmas
	; if ignore_prags then 
		 returnM pool
	  else do
	{ new_pool <- foldlM (loadRule (moduleName mod)) rule_pool rules
	; returnM (Pool new_pool (n_in + length rules) n_out) } }

loadRule :: ModuleName -> RulePoolContents -> IfaceRule -> IfL RulePoolContents
-- "Gate" the rule simply by a crude notion of the free vars of
-- the LHS.  It can be crude, because having too few free vars is safe.
loadRule mod_name pool decl@(IfaceRule {ifRuleHead = fn, ifRuleArgs = args})
  = do	{ names <- mapM lookupIfaceExt (fn : arg_fvs)
	; returnM (addRuleToPool pool (mod_name, decl) names) }
  where
    arg_fvs = [n | arg <- args, n <- crudeIfExprGblFvs arg]

---------------------------
crudeIfExprGblFvs :: IfaceExpr -> [IfaceExtName]
-- A crude approximation to the free external names of an IfExpr
-- Returns a subset of the true answer
crudeIfExprGblFvs (IfaceType ty) = get_tcs ty
crudeIfExprGblFvs (IfaceExt v)   = [v]
crudeIfExprGblFvs other	         = []	-- Well, I said it was crude

get_tcs :: IfaceType -> [IfaceExtName]
-- Get a crude subset of the TyCons of an IfaceType
get_tcs (IfaceTyVar _) 	    = []
get_tcs (IfaceAppTy t1 t2)  = get_tcs t1 ++ get_tcs t2
get_tcs (IfaceFunTy t1 t2)  = get_tcs t1 ++ get_tcs t2
get_tcs (IfaceForAllTy _ t) = get_tcs t
get_tcs (IfacePredTy st)    = case st of
				 IfaceClassP cl ts -> get_tcs_s ts
				 IfaceIParam _ t   -> get_tcs t
get_tcs (IfaceTyConApp (IfaceTc tc) ts) = tc : get_tcs_s ts
get_tcs (IfaceTyConApp other        ts) = get_tcs_s ts

-- The lists are always small => appending is fine
get_tcs_s :: [IfaceType] -> [IfaceExtName]
get_tcs_s tys = foldr ((++) . get_tcs) [] tys
\end{code}


%*********************************************************
%*							*
		Gating
%*							*
%*********************************************************

Extract the gates of an instance declaration

\begin{code}
ifaceInstGates :: IfaceType -> (IfaceExtName, [IfaceTyCon])
-- Return the class, and the tycons mentioned in the rest of the head
-- We only pick the TyCon at the root of each type, to avoid
-- difficulties with overlap.  For example, suppose there are interfaces
-- in the pool for
--	C Int b
--	C a [b]
--	C a [T] 
-- Then, if we are trying to resolve (C Int x), we need the first
--       if we are trying to resolve (C x [y]), we need *both* the latter
--	 two, even though T is not involved yet, so that we spot the overlap

ifaceInstGates (IfaceForAllTy _ t) 		   = ifaceInstGates t
ifaceInstGates (IfaceFunTy _ t)    		   = ifaceInstGates t
ifaceInstGates (IfacePredTy (IfaceClassP cls tys)) = instHeadGates cls tys
ifaceInstGates other = pprPanic "ifaceInstGates" (ppr other)
	-- The other cases should not happen

instHeadGates cls tys = (cls, mapCatMaybes root_tycon tys)
  where
    root_tycon (IfaceFunTy _ _)      = Just (IfaceTc funTyConExtName)
    root_tycon (IfaceTyConApp tc _)  = Just tc
    root_tycon other		     = Nothing

funTyConExtName = mkIfaceExtName (tyConName funTyCon)


predInstGates :: Class -> [Type] -> (Name, [Name])
-- The same function, only this time on the predicate found in a dictionary
predInstGates cls tys
  = (className cls, mapCatMaybes root_tycon tys)
  where
    root_tycon ty = case tcSplitTyConApp_maybe ty of
			Just (tc, _) -> Just (tyConName tc)
			Nothing	     -> Nothing
\end{code}


%*********************************************************
%*							*
\subsection{Reading an interface file}
%*							*
%*********************************************************

\begin{code}
findAndReadIface :: SDoc -> ModuleName 
		 -> IsBootInterface	-- True  <=> Look for a .hi-boot file
					-- False <=> Look for .hi file
		 -> IfM lcl (Either Message ModIface)
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 

	-- It *doesn't* add an error to the monad, because 
	-- sometimes it's ok to fail... see notes with loadInterface

findAndReadIface doc_str mod_name hi_boot_file
  = do	{ traceIf (sep [hsep [ptext SLIT("Reading"), 
			      if hi_boot_file 
				then ptext SLIT("[boot]") 
				else empty,
			      ptext SLIT("interface for"), 
			      ppr mod_name <> semi],
		        nest 4 (ptext SLIT("reason:") <+> doc_str)])

	-- Check for GHC.Prim, and return its static interface
	; if mod_name == gHC_PRIM_Name
	  then returnM (Right ghcPrimIface)
	  else do

	-- Look for the file
	; mb_found <- ioToIOEnv (findHiFile mod_name hi_boot_file)
	; case mb_found of {
	      Left files -> do
		{ traceIf (ptext SLIT("...not found"))
		; dflags <- getDOpts
		; returnM (Left (noIfaceErr dflags mod_name hi_boot_file files)) } ;

	      Right file_path -> do

	-- Found file, so read it
	{ traceIf (ptext SLIT("readIFace") <+> text file_path)
	; read_result <- readIface mod_name file_path hi_boot_file
	; case read_result of
	    Left err    -> returnM (Left (badIfaceFile file_path err))
	    Right iface -> returnM (Right iface)
	}}}

findHiFile :: ModuleName -> IsBootInterface
	   -> IO (Either [FilePath] FilePath)
findHiFile mod_name hi_boot_file
 = do { 
	-- In interactive or --make mode, we are *not allowed* to demand-load
	-- a home package .hi file.  So don't even look for them.
	-- This helps in the case where you are sitting in eg. ghc/lib/std
	-- and start up GHCi - it won't complain that all the modules it tries
	-- to load are found in the home location.
	ghci_mode <- readIORef v_GhcMode ;
	let { home_allowed = hi_boot_file || 
			     not (isCompManagerMode ghci_mode) } ;
	maybe_found <-	if home_allowed 
			then findModule mod_name
			else findPackageModule mod_name ;

	case maybe_found of {
	  Left files -> return (Left files) ;

	  Right (_, loc) -> do {	-- Don't need module returned by finder

	-- Return the path to M.hi, M.hi-boot, or M.hi-boot-n as appropriate
	let { hi_path            = ml_hi_file loc ;
    	      hi_boot_path       = replaceFilenameSuffix hi_path hiBootExt ;
    	      hi_boot_ver_path   = replaceFilenameSuffix hi_path hiBootVerExt 
	    };

	if not hi_boot_file then
	   return (Right hi_path)
	else do {
		hi_ver_exists <- doesFileExist hi_boot_ver_path ;
		if hi_ver_exists then return (Right hi_boot_ver_path)
				 else return (Right hi_boot_path)
	}}}}
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: ModuleName -> String -> IsBootInterface 
	  -> IfM lcl (Either Message ModIface)
	-- Left err    <=> file not found, or unreadable, or illegible
	-- Right iface <=> successfully found and parsed 

readIface wanted_mod_name file_path is_hi_boot_file
  = do	{ dflags <- getDOpts
	; ioToIOEnv (read_iface dflags wanted_mod_name file_path is_hi_boot_file) }

read_iface dflags wanted_mod file_path is_hi_boot_file
 | is_hi_boot_file		-- Read ascii
 = do { res <- tryMost (hGetStringBuffer file_path) ;
	case res of {
	  Left exn     -> return (Left (text (showException exn))) ;
	  Right buffer -> 
        case unP parseIface (mkPState buffer loc dflags) of
	  PFailed loc1 loc2 err -> return (Left (showPFailed loc1 loc2 err))
	  POk _ iface 
	     | wanted_mod == actual_mod -> return (Right iface)
	     | otherwise		-> return (Left err) 
	     where
		actual_mod = moduleName (mi_module iface)
		err = hiModuleNameMismatchWarn wanted_mod actual_mod
     }}

 | otherwise		-- Read binary
 = do	{ res <- tryMost (readBinIface file_path)
	; case res of
	    Right iface -> return (Right iface)
	    Left exn    -> return (Left (text (showException exn))) }
 where
    loc  = mkSrcLoc (mkFastString file_path) 1 0
\end{code}


%*********************************************************
%*						 	 *
	Wired-in interface for GHC.Prim
%*							 *
%*********************************************************

\begin{code}
initExternalPackageState :: ExternalPackageState
initExternalPackageState
  = EPS { 
      eps_PIT        = emptyPackageIfaceTable,
      eps_PTE        = emptyTypeEnv,
      eps_inst_env   = emptyInstEnv,
      eps_rule_base  = emptyRuleBase,
      eps_decls      = emptyPool emptyNameEnv,
      eps_insts      = emptyPool emptyNameEnv,
      eps_rules      = foldr add (emptyPool []) builtinRules
    }
  where
	-- Initialise the EPS rule pool with the built-in rules
    add (fn_name, core_rule) (Pool rules n_in n_out) 
      = Pool rules' (n_in+1) n_out
      where
 	rules' = addRuleToPool rules iface_rule [fn_name]
	iface_rule = (nameModuleName fn_name, IfaceBuiltinRule (mkIfaceExtName fn_name) core_rule)
\end{code}


%*********************************************************
%*						 	 *
	Wired-in interface for GHC.Prim
%*							 *
%*********************************************************

\begin{code}
ghcPrimIface :: ModIface
ghcPrimIface
  = (emptyModIface basePackage gHC_PRIM_Name) {
	mi_exports  = [(gHC_PRIM_Name, ghcPrimExports)],
	mi_decls    = [],
	mi_fixities = fixities,
	mi_fix_fn  = mkIfaceFixCache fixities
    }		
  where
    fixities = [(getOccName seqId, Fixity 0 InfixR)]
			-- seq is infixr 0
\end{code}

%*********************************************************
%*							*
\subsection{Statistics}
%*							*
%*********************************************************

\begin{code}
ifaceStats :: ExternalPackageState -> SDoc
ifaceStats eps 
  = hcat [text "Renamer stats: ", stats]
  where
    n_mods = length [() | _ <- moduleEnvElts (eps_PIT eps)]
	-- This is really only right for a one-shot compile

    Pool _ n_decls_in n_decls_out = eps_decls eps
    Pool _ n_insts_in n_insts_out = eps_insts eps
    Pool _ n_rules_in n_rules_out = eps_rules eps
    
    stats = vcat 
    	[int n_mods <+> text "interfaces read",
    	 hsep [ int n_decls_out, text "type/class/variable imported, out of", 
    	        int n_decls_in, text "read"],
    	 hsep [ int n_insts_out, text "instance decls imported, out of",  
    	        int n_insts_in, text "read"],
    	 hsep [ int n_rules_out, text "rule decls imported, out of",  
    	        int n_rules_in, text "read"]
	]
\end{code}    


%*********************************************************
%*						 	 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
badIfaceFile file err
  = vcat [ptext SLIT("Bad interface file:") <+> text file, 
	  nest 4 err]

hiModuleNameMismatchWarn :: ModuleName -> ModuleName -> Message
hiModuleNameMismatchWarn requested_mod read_mod = 
    hsep [ ptext SLIT("Something is amiss; requested module name")
	 , ppr requested_mod
	 , ptext SLIT("differs from name found in the interface file")
   	 , ppr read_mod
  	 ]

noIfaceErr dflags mod_name boot_file files
  = ptext SLIT("Could not find interface file for") <+> quotes (ppr mod_name)
    $$ extra
  where 
   extra
    | verbosity dflags < 3 = 
        text "(use -v to see a list of the files searched for)"
    | otherwise =
        hang (ptext SLIT("locations searched:")) 4 (vcat (map text files))
\end{code}
