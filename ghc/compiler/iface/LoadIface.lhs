%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Dealing with interface files}

\begin{code}
module LoadIface (
	loadHomeInterface, loadInterface, loadDecls,
	loadSrcInterface, loadOrphanModules, loadHiBootInterface,
	readIface,	-- Used when reading the module's old interface
	predInstGates, ifaceInstGates, ifaceStats, discardDeclPrags,
	initExternalPackageState
   ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcIface( tcIfaceDecl )

import Packages		( PackageState(..), PackageIdH(..), isHomePackage )
import DriverState	( v_GhcMode, isCompManagerMode )
import CmdLineOpts	( DynFlags(..), DynFlag( Opt_IgnoreInterfacePragmas ) )
import IfaceSyn		( IfaceDecl(..), IfaceConDecl(..), IfaceClassOp(..),
			  IfaceConDecls(..), IfaceInst(..), IfaceRule(..),
			  IfaceExpr(..), IfaceTyCon(..), IfaceIdInfo(..), 
			  IfaceType(..), IfacePredType(..), IfaceExtName,
			  mkIfaceExtName )
import IfaceEnv		( newGlobalBinder, lookupIfaceExt, lookupIfaceTc,
			  lookupOrig )
import HscTypes		( ModIface(..), TyThing, emptyModIface, EpsStats(..),
			  addEpsInStats, ExternalPackageState(..),
			  PackageTypeEnv, emptyTypeEnv,  
			  lookupIfaceByModule, emptyPackageIfaceTable,
			  IsBootInterface, mkIfaceFixCache, Gated,
			  implicitTyThings, addRulesToPool, addInstsToPool,
			  availNames
			 )

import BasicTypes	( Version, Fixity(..), FixityDirection(..),
			  isMarkedStrict )
import TcType		( Type, tcSplitTyConApp_maybe )
import Type		( funTyCon )
import TcRnMonad

import PrelNames	( gHC_PRIM )
import PrelInfo		( ghcPrimExports )
import PrelRules	( builtinRules )
import Rules		( emptyRuleBase )
import InstEnv		( emptyInstEnv )
import Name		( Name {-instance NamedThing-}, getOccName,
			  nameModule, isInternalName )
import NameEnv
import MkId		( seqId )
import Module		( Module, ModLocation(ml_hi_file), emptyModuleEnv, 
			  addBootSuffix_maybe,
			  extendModuleEnv, lookupModuleEnv, moduleUserString
			)
import OccName		( OccName, mkOccEnv, lookupOccEnv, mkClassTyConOcc, mkClassDataConOcc,
			  mkSuperDictSelOcc, mkDataConWrapperOcc, mkDataConWorkerOcc )
import Class		( Class, className )
import TyCon		( tyConName )
import SrcLoc		( importedSrcLoc )
import Maybes		( mapCatMaybes, MaybeErr(..) )
import FastString	( mkFastString )
import ErrUtils         ( Message )
import Finder		( findModule, findPackageModule,  FindResult(..), cantFindError )
import Outputable
import BinIface		( readBinIface )
import Panic		( ghcError, tryMost, showException, GhcException(..) )
import List		( nub )

import DATA_IOREF	( readIORef )
\end{code}


%************************************************************************
%*									*
		loadSrcInterface, loadOrphanModules

		These two are called from TcM-land	
%*									*
%************************************************************************

\begin{code}
loadSrcInterface :: SDoc -> Module -> IsBootInterface -> RnM ModIface
-- This is called for each 'import' declaration in the source code
-- On a failure, fail in the monad with an error message

loadSrcInterface doc mod_name want_boot
  = do 	{ mb_iface <- initIfaceTcRn $ loadInterface doc mod_name 
					   (ImportByUser want_boot)
	; case mb_iface of
	    Failed err      -> failWithTc (elaborate err) 
	    Succeeded iface -> return iface
	}
  where
    elaborate err = hang (ptext SLIT("Failed to load interface for") <+> 
			 quotes (ppr mod_name) <> colon) 4 err

loadHiBootInterface :: TcRn [Name]
-- Load the hi-boot iface for the module being compiled,
-- if it indeed exists in the transitive closure of imports
-- Return the list of names exported by the hi-boot file
loadHiBootInterface
  = do 	{ eps <- getEps
	; mod <- getModule

	; traceIf (text "loadHiBootInterface" <+> ppr mod)

	-- We're read all the direct imports by now, so eps_is_boot will
	-- record if any of our imports mention us by way of hi-boot file
	; case lookupModuleEnv (eps_is_boot eps) mod of {
	    Nothing             -> return [] ;	-- The typical case

	    Just (_, False) -> 		-- Someone below us imported us!
		-- This is a loop with no hi-boot in the way
		failWithTc (moduleLoop mod) ;

	    Just (mod_nm, True) -> 	-- There's a hi-boot interface below us
		

    do	{ 	-- Load it (into the PTE), and return the exported names
	  iface <- loadSrcInterface (mk_doc mod_nm) mod_nm True
	; sequenceM [ lookupOrig mod_nm occ
		    | (mod,avails) <- mi_exports iface, 
		      avail <- avails, occ <- availNames avail]
    }}}
  where
    mk_doc mod = ptext SLIT("Need the hi-boot interface for") <+> ppr mod
		 <+> ptext SLIT("to compare against the Real Thing")

    moduleLoop mod = ptext SLIT("Circular imports: module") <+> quotes (ppr mod) 
		     <+> ptext SLIT("depends on itself")

loadOrphanModules :: [Module] -> TcM ()
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
    loadSysInterface doc (nameModule name)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
-- A wrapper for loadInterface that Throws an exception if it fails
loadSysInterface doc mod_name
  = do	{ mb_iface <- loadInterface doc mod_name ImportBySystem
	; case mb_iface of 
	    Failed err      -> ghcError (ProgramError (showSDoc err))
	    Succeeded iface -> return iface }
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
loadInterface :: SDoc -> Module -> WhereFrom 
	      -> IfM lcl (MaybeErr Message ModIface)
-- If it can't find a suitable interface file, we
--	a) modify the PackageIfaceTable to have an empty entry
--		(to avoid repeated complaints)
--	b) return (Left message)
--
-- It's not necessarily an error for there not to be an interface
-- file -- perhaps the module has changed, and that interface 
-- is no longer used

loadInterface doc_str mod from
  = do	{ 	-- Read the state
	  (eps,hpt) <- getEpsAndHpt

	; traceIf (text "Considering whether to load" <+> ppr mod <+> ppr from)

		-- Check whether we have the interface already
	; case lookupIfaceByModule hpt (eps_PIT eps) mod of {
	    Just iface 
		-> returnM (Succeeded iface) ;	-- Already loaded
			-- The (src_imp == mi_boot iface) test checks that the already-loaded
			-- interface isn't a boot iface.  This can conceivably happen,
			-- if an earlier import had a before we got to real imports.   I think.
	    other -> do

	{ let { hi_boot_file = case from of
				ImportByUser usr_boot -> usr_boot
				ImportBySystem        -> sys_boot

	      ; mb_dep   = lookupModuleEnv (eps_is_boot eps) mod
	      ; sys_boot = case mb_dep of
				Just (_, is_boot) -> is_boot
				Nothing		  -> False
			-- The boot-ness of the requested interface, 
	      }		-- based on the dependencies in directly-imported modules

	-- READ THE MODULE IN
	; let explicit | ImportByUser _ <- from = True
		       | otherwise              = False
	; read_result <- findAndReadIface explicit doc_str mod hi_boot_file
	; dflags <- getDOpts
	; case read_result of {
	    Failed err -> do
	  	{ let fake_iface = emptyModIface HomePackage mod

		; updateEps_ $ \eps ->
			eps { eps_PIT = extendModuleEnv (eps_PIT eps) (mi_module fake_iface) fake_iface }
			-- Not found, so add an empty iface to 
			-- the EPS map so that we don't look again
				
		; returnM (Failed err) } ;

	-- Found and parsed!
	    Succeeded (iface, file_path) 			-- Sanity check:
		| ImportBySystem <- from,		--   system-importing...
		  isHomePackage (mi_package iface),	--   ...a home-package module
		  Nothing <- mb_dep			--   ...that we know nothing about
		-> returnM (Failed (badDepMsg mod))

		| otherwise ->

	let 
	    loc_doc = text file_path <+> colon
	in 
	initIfaceLcl mod loc_doc $ do

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

	; ignore_prags <- doptM Opt_IgnoreInterfacePragmas
	; new_eps_decls <- loadDecls ignore_prags (mi_decls iface)
	; new_eps_insts <- mapM loadInst (mi_insts iface)
	; new_eps_rules <- if ignore_prags 
			   then return []
			   else mapM loadRule (mi_rules iface)

	; let {	final_iface = iface {	mi_decls = panic "No mi_decls in PIT",
					mi_insts = panic "No mi_insts in PIT",
					mi_rules = panic "No mi_rules in PIT" } }

	; updateEps_  $ \ eps -> 
		eps {	eps_PIT   = extendModuleEnv (eps_PIT eps) mod final_iface,
			eps_PTE   = addDeclsToPTE   (eps_PTE eps) new_eps_decls,
			eps_rules = addRulesToPool  (eps_rules eps) new_eps_rules,
			eps_insts = addInstsToPool  (eps_insts eps) new_eps_insts,
			eps_stats = addEpsInStats   (eps_stats eps) (length new_eps_decls)
						    (length new_eps_insts) (length new_eps_rules) }

	; return (Succeeded final_iface)
    }}}}

badDepMsg mod 
  = hang (ptext SLIT("Interface file inconsistency:"))
       2 (sep [ptext SLIT("home-package module") <+> quotes (ppr mod) <+> ptext SLIT("is mentioned,"), 
	       ptext SLIT("but does not appear in the dependencies of the interface")])

-----------------------------------------------------
--	Loading type/class/value decls
-- We pass the full Module name here, replete with
-- its package info, so that we can build a Name for
-- each binder with the right package info in it
-- All subsequent lookups, including crucially lookups during typechecking
-- the declaration itself, will find the fully-glorious Name
-----------------------------------------------------

addDeclsToPTE :: PackageTypeEnv -> [(Name,TyThing)] -> PackageTypeEnv
addDeclsToPTE pte things = extendNameEnvList pte things

loadDecls :: Bool
	  -> [(Version, IfaceDecl)]
	  -> IfL [(Name,TyThing)]
loadDecls ignore_prags ver_decls
   = do { mod <- getIfModule
 	; thingss <- mapM (loadDecl ignore_prags mod) ver_decls
	; return (concat thingss)
	}

loadDecl :: Bool			-- Don't load pragmas into the decl pool
	 -> Module
	  -> (Version, IfaceDecl)
	  -> IfL [(Name,TyThing)]	-- The list can be poked eagerly, but the
					-- TyThings are forkM'd thunks
loadDecl ignore_prags mod (_version, decl)
  = do 	{ 	-- Populate the name cache with final versions of all 
		-- the names associated with the decl
	  main_name      <- mk_new_bndr mod Nothing (ifName decl)
	; implicit_names <- mapM (mk_new_bndr mod (Just main_name)) (ifaceDeclSubBndrs decl)

	-- Typecheck the thing, lazily
	-- NB. firstly, the laziness is there in case we never need the
	-- declaration (in one-shot mode), and secondly it is there so that 
	-- we don't look up the occurrence of a name before calling mk_new_bndr
	-- on the binder.  This is important because we must get the right name
	-- which includes its nameParent.
	; thing <- forkM doc (bumpDeclStats main_name >> tcIfaceDecl stripped_decl)
	; let mini_env = mkOccEnv [(getOccName t, t) | t <- implicitTyThings thing]
	      lookup n = case lookupOccEnv mini_env (getOccName n) of
			   Just thing -> thing
			   Nothing    -> pprPanic "loadDecl" (ppr main_name <+> ppr n)

	; returnM ((main_name, thing) : [(n, lookup n) | n <- implicit_names]) }
		-- We build a list from the *known* names, with (lookup n) thunks
		-- as the TyThings.  That way we can extend the PTE without poking the
		-- thunks
  where
    stripped_decl | ignore_prags = discardDeclPrags decl
		  | otherwise    = decl

	-- mk_new_bndr allocates in the name cache the final canonical
	-- name for the thing, with the correct 
	--	* parent
	--	* location
	-- imported name, to fix the module correctly in the cache
    mk_new_bndr mod mb_parent occ 
	= newGlobalBinder mod occ mb_parent 
			  (importedSrcLoc (moduleUserString mod))

    doc = ptext SLIT("Declaration for") <+> ppr (ifName decl)

discardDeclPrags :: IfaceDecl -> IfaceDecl
discardDeclPrags decl@(IfaceId {ifIdInfo = HasInfo _}) = decl { ifIdInfo = NoInfo }
discardDeclPrags decl 		    	  	       = decl

bumpDeclStats :: Name -> IfL ()		-- Record that one more declaration has actually been used
bumpDeclStats name
  = do	{ traceIf (text "Loading decl for" <+> ppr name)
	; updateEps_ (\eps -> let stats = eps_stats eps
			      in eps { eps_stats = stats { n_decls_out = n_decls_out stats + 1 } })
	}

-----------------
ifaceDeclSubBndrs :: IfaceDecl -> [OccName]
-- *Excludes* the 'main' name, but *includes* the implicitly-bound names
-- Deeply revolting, because it has to predict what gets bound,
-- especially the question of whether there's a wrapper for a datacon

ifaceDeclSubBndrs (IfaceClass {ifCtxt = sc_ctxt, ifName = cls_occ, ifSigs = sigs })
  = [tc_occ, dc_occ, dcww_occ] ++
    [op | IfaceClassOp op _ _ <- sigs] ++
    [mkSuperDictSelOcc n cls_occ | n <- [1..n_ctxt]] 
  where
    n_ctxt = length sc_ctxt
    n_sigs = length sigs
    tc_occ  = mkClassTyConOcc cls_occ
    dc_occ  = mkClassDataConOcc cls_occ	
    dcww_occ | is_newtype = mkDataConWrapperOcc dc_occ	-- Newtypes have wrapper but no worker
	     | otherwise  = mkDataConWorkerOcc dc_occ	-- Otherwise worker but no wrapper
    is_newtype = n_sigs + n_ctxt == 1			-- Sigh 

ifaceDeclSubBndrs (IfaceData {ifCons = IfAbstractTyCon}) 
  = []
-- Newtype
ifaceDeclSubBndrs (IfaceData {ifCons = IfNewTyCon (IfVanillaCon { ifConOcc = con_occ, 
								  ifConFields = fields})}) 
  = fields ++ [con_occ, mkDataConWrapperOcc con_occ] 	
	-- Wrapper, no worker; see MkId.mkDataConIds

ifaceDeclSubBndrs (IfaceData {ifCons = IfDataTyCon _ cons})
  = nub (concatMap fld_occs cons) 	-- Eliminate duplicate fields
    ++ concatMap dc_occs cons
  where
    fld_occs (IfVanillaCon { ifConFields = fields }) = fields
    fld_occs (IfGadtCon {}) 			     = []
    dc_occs con_decl
	| has_wrapper = [con_occ, work_occ, wrap_occ]
	| otherwise   = [con_occ, work_occ]
	where
	  con_occ = ifConOcc con_decl
	  strs    = ifConStricts con_decl
	  wrap_occ = mkDataConWrapperOcc con_occ
	  work_occ = mkDataConWorkerOcc con_occ
	  has_wrapper = any isMarkedStrict strs	-- See MkId.mkDataConIds (sigh)
		-- ToDo: may miss strictness in existential dicts

ifaceDeclSubBndrs _other 		      = []

-----------------------------------------------------
--	Loading instance decls
-----------------------------------------------------

loadInst :: IfaceInst -> IfL (Name, Gated IfaceInst)

loadInst decl@(IfaceInst {ifInstHead = inst_ty})
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
	; (mod, doc) <- getIfCtxt 
	; returnM (cls, (tcs, (mod, doc, decl)))
	}

-----------------------------------------------------
--	Loading Rules
-----------------------------------------------------

loadRule :: IfaceRule -> IfL (Gated IfaceRule)
-- "Gate" the rule simply by a crude notion of the free vars of
-- the LHS.  It can be crude, because having too few free vars is safe.
loadRule decl@(IfaceRule {ifRuleHead = fn, ifRuleArgs = args})
  = do	{ names <- mapM lookupIfaceExt (fn : arg_fvs)
	; (mod, doc) <- getIfCtxt 
	; returnM (names, (mod, doc, decl)) }
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


----------------
getIfCtxt :: IfL (Module, SDoc)
getIfCtxt = do { env <- getLclEnv; return (if_mod env, if_loc env) }
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
ifaceInstGates (IfacePredTy (IfaceClassP cls tys)) = (cls, instHeadTyconGates tys)
ifaceInstGates other = pprPanic "ifaceInstGates" (ppr other)
	-- The other cases should not happen

instHeadTyconGates tys = mapCatMaybes root_tycon tys
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
findAndReadIface :: Bool 		-- True <=> explicit user import
		 -> SDoc -> Module 
		 -> IsBootInterface	-- True  <=> Look for a .hi-boot file
					-- False <=> Look for .hi file
		 -> IfM lcl (MaybeErr Message (ModIface, FilePath))
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 

	-- It *doesn't* add an error to the monad, because 
	-- sometimes it's ok to fail... see notes with loadInterface

findAndReadIface explicit doc_str mod_name hi_boot_file
  = do	{ traceIf (sep [hsep [ptext SLIT("Reading"), 
			      if hi_boot_file 
				then ptext SLIT("[boot]") 
				else empty,
			      ptext SLIT("interface for"), 
			      ppr mod_name <> semi],
		        nest 4 (ptext SLIT("reason:") <+> doc_str)])

	-- Check for GHC.Prim, and return its static interface
	; dflags <- getDOpts
	; let base_pkg = basePackageId (pkgState dflags)
	; if mod_name == gHC_PRIM
	  then returnM (Succeeded (ghcPrimIface{ mi_package = base_pkg }, 
			"<built in interface for GHC.Prim>"))
	  else do

	-- Look for the file
	; mb_found <- ioToIOEnv (findHiFile dflags explicit mod_name hi_boot_file)
	; case mb_found of {
	      Failed err -> do
		{ traceIf (ptext SLIT("...not found"))
		; dflags <- getDOpts
		; returnM (Failed (cantFindError dflags mod_name err)) } ;

	      Succeeded (file_path, pkg) -> do 

	-- Found file, so read it
	{ traceIf (ptext SLIT("readIFace") <+> text file_path)
	; read_result <- readIface mod_name file_path hi_boot_file
	; case read_result of
	    Failed err -> returnM (Failed (badIfaceFile file_path err))
	    Succeeded iface 
		| mi_module iface /= mod_name ->
		  return (Failed (wrongIfaceModErr iface mod_name file_path))
		| otherwise ->
		  returnM (Succeeded (iface{mi_package=pkg}, file_path))
			-- Don't forget to fill in the package name...
	}}}

findHiFile :: DynFlags -> Bool -> Module -> IsBootInterface
	   -> IO (MaybeErr FindResult (FilePath, PackageIdH))
findHiFile dflags explicit mod_name hi_boot_file
 = do { 
	-- In interactive or --make mode, we are *not allowed* to demand-load
	-- a home package .hi file.  So don't even look for them.
	-- This helps in the case where you are sitting in eg. ghc/lib/std
	-- and start up GHCi - it won't complain that all the modules it tries
	-- to load are found in the home location.
	ghci_mode <- readIORef v_GhcMode ;
	let { home_allowed = not (isCompManagerMode ghci_mode) } ;
	maybe_found <-	if home_allowed 
			then findModule        dflags mod_name explicit
			else findPackageModule dflags mod_name explicit;

	case maybe_found of
	  Found loc pkg -> return (Succeeded (path, pkg))
			where
			   path = addBootSuffix_maybe hi_boot_file (ml_hi_file loc)

	  err -> return (Failed err)
	}
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: Module -> String -> IsBootInterface 
	  -> IfM lcl (MaybeErr Message ModIface)
	-- Failed err    <=> file not found, or unreadable, or illegible
	-- Succeeded iface <=> successfully found and parsed 

readIface wanted_mod file_path is_hi_boot_file
  = do	{ dflags <- getDOpts
	; ioToIOEnv $ do
	{ res <- tryMost (readBinIface file_path)
	; case res of
	    Right iface 
		| wanted_mod == actual_mod -> return (Succeeded iface)
		| otherwise	  	   -> return (Failed err)
		where
		  actual_mod = mi_module iface
		  err = hiModuleNameMismatchWarn wanted_mod actual_mod

	    Left exn    -> return (Failed (text (showException exn)))
    }}
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
      eps_is_boot    = emptyModuleEnv,
      eps_PIT        = emptyPackageIfaceTable,
      eps_PTE        = emptyTypeEnv,
      eps_inst_env   = emptyInstEnv,
      eps_rule_base  = emptyRuleBase,
      eps_insts      = emptyNameEnv,
      eps_rules      = addRulesToPool [] (map mk_gated_rule builtinRules),
	-- Initialise the EPS rule pool with the built-in rules
      eps_stats = EpsStats { n_ifaces_in = 0, n_decls_in = 0, n_decls_out = 0
			   , n_insts_in = 0, n_insts_out = 0
			   , n_rules_in = length builtinRules, n_rules_out = 0 }
    }
  where
    mk_gated_rule (fn_name, core_rule)
	= ([fn_name], (nameModule fn_name, ptext SLIT("<built-in rule>"),
	   IfaceBuiltinRule (mkIfaceExtName fn_name) core_rule))
\end{code}


%*********************************************************
%*						 	 *
	Wired-in interface for GHC.Prim
%*							 *
%*********************************************************

\begin{code}
ghcPrimIface :: ModIface
ghcPrimIface
  = (emptyModIface HomePackage gHC_PRIM) {
	mi_exports  = [(gHC_PRIM, ghcPrimExports)],
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
  = hcat [text "Renamer stats: ", msg]
  where
    stats = eps_stats eps
    msg = vcat 
    	[int (n_ifaces_in stats) <+> text "interfaces read",
    	 hsep [ int (n_decls_out stats), text "type/class/variable imported, out of", 
    	        int (n_decls_in stats), text "read"],
    	 hsep [ int (n_insts_out stats), text "instance decls imported, out of",  
    	        int (n_insts_in stats), text "read"],
    	 hsep [ int (n_rules_out stats), text "rule decls imported, out of",  
    	        int (n_rules_in stats), text "read"]
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

hiModuleNameMismatchWarn :: Module -> Module -> Message
hiModuleNameMismatchWarn requested_mod read_mod = 
    hsep [ ptext SLIT("Something is amiss; requested module name")
	 , ppr requested_mod
	 , ptext SLIT("differs from name found in the interface file")
   	 , ppr read_mod
  	 ]

wrongIfaceModErr iface mod_name file_path 
  = sep [ptext SLIT("Interface file") <+> iface_file,
         ptext SLIT("contains module") <+> quotes (ppr (mi_module iface)) <> comma,
         ptext SLIT("but we were expecting module") <+> quotes (ppr mod_name),
	 sep [ptext SLIT("Probable cause: the source code which generated"),
	     nest 2 iface_file,
	     ptext SLIT("has an incompatible module name")
	    ]
	]
  where iface_file = doubleQuotes (text file_path)
\end{code}
