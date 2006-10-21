%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
module RnNames (
	rnImports, importsFromLocalDecls,
	rnExports,
	getLocalDeclBinders, extendRdrEnvRn,
	reportUnusedNames, reportDeprecations
    ) where

#include "HsVersions.h"

import DynFlags		( DynFlag(..), GhcMode(..), DynFlags(..) )
import HsSyn		( IE(..), ieName, ImportDecl(..), LImportDecl,
			  ForeignDecl(..), HsGroup(..), HsValBinds(..),
			  Sig(..), collectHsBindLocatedBinders, tyClDeclNames,
			  instDeclATs, isIdxTyDecl,
			  LIE )
import RnEnv
import RnHsDoc          ( rnHsDoc )
import IfaceEnv		( ifaceExportNames )
import LoadIface	( loadSrcInterface )
import TcRnMonad hiding (LIE)

import PrelNames
import Module
import Name
import NameEnv
import NameSet
import OccName		( srcDataName, pprNonVarNameSpace,
			  occNameSpace,
			  OccEnv, mkOccEnv, mkOccEnv_C, lookupOccEnv,
			  emptyOccEnv, extendOccEnv )
import HscTypes		( GenAvailInfo(..), AvailInfo, availNames, availName,
			  HomePackageTable, PackageIfaceTable, 
			  mkPrintUnqualified, availsToNameSet,
			  Deprecs(..), ModIface(..), Dependencies(..), 
			  lookupIfaceByModule, ExternalPackageState(..)
			)
import RdrName		( RdrName, rdrNameOcc, setRdrNameSpace, Parent(..),
		  	  GlobalRdrEnv, mkGlobalRdrEnv, GlobalRdrElt(..), 
			  emptyGlobalRdrEnv, plusGlobalRdrEnv, globalRdrEnvElts,
			  extendGlobalRdrEnv, lookupGlobalRdrEnv,
			  lookupGRE_RdrName, lookupGRE_Name, 
			  Provenance(..), ImportSpec(..), ImpDeclSpec(..), ImpItemSpec(..), 
			  importSpecLoc, importSpecModule, isLocalGRE, pprNameProvenance,
			  unQualSpecOK, qualSpecOK )
import Outputable
import Maybes
import SrcLoc		( Located(..), mkGeneralSrcSpan, getLoc,
			  unLoc, noLoc, srcLocSpan, SrcSpan )
import FiniteMap
import ErrUtils
import BasicTypes	( DeprecTxt )
import DriverPhases	( isHsBoot )
import Util
import ListSetOps
import Data.List        ( partition, concatMap, (\\), delete )
import IO		( openFile, IOMode(..) )
import Monad		( when )
\end{code}



%************************************************************************
%*									*
		rnImports
%*									*
%************************************************************************

\begin{code}
rnImports :: [LImportDecl RdrName]
           -> RnM ([LImportDecl Name], GlobalRdrEnv, ImportAvails)

rnImports imports
         -- PROCESS IMPORT DECLS
         -- Do the non {- SOURCE -} ones first, so that we get a helpful
         -- warning for {- SOURCE -} ones that are unnecessary
    = do this_mod <- getModule
         implicit_prelude <- doptM Opt_ImplicitPrelude
         let all_imports	       = mk_prel_imports this_mod implicit_prelude ++ imports
             (source, ordinary) = partition is_source_import all_imports
             is_source_import (L _ (ImportDecl _ is_boot _ _ _)) = is_boot

         stuff1 <- mapM (rnImportDecl this_mod) ordinary
         stuff2 <- mapM (rnImportDecl this_mod) source
         let (decls, rdr_env, imp_avails) = combine (stuff1 ++ stuff2)
         return (decls, rdr_env, imp_avails) 

    where
-- NB: opt_NoImplicitPrelude is slightly different to import Prelude ();
-- because the former doesn't even look at Prelude.hi for instance 
-- declarations, whereas the latter does.
   mk_prel_imports this_mod implicit_prelude
       |  this_mod == pRELUDE
          || explicit_prelude_import
          || not implicit_prelude
           = []
       | otherwise = [preludeImportDecl]
   explicit_prelude_import
       = notNull [ () | L _ (ImportDecl mod _ _ _ _) <- imports, 
	           unLoc mod == pRELUDE_NAME ]

   combine :: [(LImportDecl Name,  GlobalRdrEnv, ImportAvails)]
           -> ([LImportDecl Name], GlobalRdrEnv, ImportAvails)
   combine = foldr plus ([], emptyGlobalRdrEnv, emptyImportAvails)
        where plus (decl,  gbl_env1, imp_avails1)
                   (decls, gbl_env2, imp_avails2)
                = (decl:decls, 
                   gbl_env1 `plusGlobalRdrEnv` gbl_env2,
                   imp_avails1 `plusImportAvails` imp_avails2)

preludeImportDecl :: LImportDecl RdrName
preludeImportDecl
  = L loc $
	ImportDecl (L loc pRELUDE_NAME)
	       False {- Not a boot interface -}
	       False	{- Not qualified -}
	       Nothing	{- No "as" -}
	       Nothing	{- No import list -}
  where
    loc = mkGeneralSrcSpan FSLIT("Implicit import declaration")         

	

rnImportDecl  :: Module
	      -> LImportDecl RdrName
	      -> RnM (LImportDecl Name, GlobalRdrEnv, ImportAvails)

rnImportDecl this_mod (L loc (ImportDecl loc_imp_mod_name want_boot
                                         qual_only as_mod imp_details))
  = 
    setSrcSpan loc $ do

	-- If there's an error in loadInterface, (e.g. interface
	-- file not found) we get lots of spurious errors from 'filterImports'
    let
	imp_mod_name = unLoc loc_imp_mod_name
	doc = ppr imp_mod_name <+> ptext SLIT("is directly imported")

    iface <- loadSrcInterface doc imp_mod_name want_boot

	-- Compiler sanity check: if the import didn't say
	-- {-# SOURCE #-} we should not get a hi-boot file
    WARN( not want_boot && mi_boot iface, ppr imp_mod_name ) (do

	-- Issue a user warning for a redundant {- SOURCE -} import
	-- NB that we arrange to read all the ordinary imports before 
	-- any of the {- SOURCE -} imports
    warnIf (want_boot && not (mi_boot iface))
	   (warnRedundantSourceImport imp_mod_name)

    let
	imp_mod	   = mi_module iface
	deprecs	   = mi_deprecs iface
	is_orph	   = mi_orphan iface 
	has_finsts = mi_finsts iface 
	deps 	   = mi_deps iface

	filtered_exports = filter not_this_mod (mi_exports iface)
	not_this_mod (mod,_) = mod /= this_mod
	-- If the module exports anything defined in this module, just
	-- ignore it.  Reason: otherwise it looks as if there are two
	-- local definition sites for the thing, and an error gets
	-- reported.  Easiest thing is just to filter them out up
	-- front. This situation only arises if a module imports
	-- itself, or another module that imported it.  (Necessarily,
	-- this invoves a loop.)
	--
	-- Tiresome consequence: if you say
	--	module A where
	--	   import B( AType )
	--	   type AType = ...
	--
	--	module B( AType ) where
	--	   import {-# SOURCE #-} A( AType )
	--
	-- then you'll get a 'B does not export AType' message.  Oh well.

	qual_mod_name = case as_mod of
			  Nothing  	    -> imp_mod_name
			  Just another_name -> another_name
	imp_spec  = ImpDeclSpec { is_mod = imp_mod_name, is_qual = qual_only,  
		  		  is_dloc = loc, is_as = qual_mod_name }
    -- in

	-- Get the total exports from this module
    total_avails <- ifaceExportNames filtered_exports

        -- filter the imports according to the import declaration
    (new_imp_details, gbl_env) <- 
        filterImports2 iface imp_spec imp_details total_avails

    dflags <- getDOpts

    let
	-- Compute new transitive dependencies

 	orphans | is_orph   = ASSERT( not (imp_mod `elem` dep_orphs deps) )
			      imp_mod : dep_orphs deps
		| otherwise = dep_orphs deps

 	finsts | has_finsts = ASSERT( not (imp_mod `elem` dep_finsts deps) )
			      imp_mod : dep_finsts deps
		| otherwise = dep_finsts deps

	pkg = modulePackageId (mi_module iface)

	(dependent_mods, dependent_pkgs) 
	   | pkg == thisPackage dflags =
	    	-- Imported module is from the home package
		-- Take its dependent modules and add imp_mod itself
		-- Take its dependent packages unchanged
		--
		-- NB: (dep_mods deps) might include a hi-boot file
		-- for the module being compiled, CM. Do *not* filter
		-- this out (as we used to), because when we've
		-- finished dealing with the direct imports we want to
		-- know if any of them depended on CM.hi-boot, in
		-- which case we should do the hi-boot consistency
		-- check.  See LoadIface.loadHiBootInterface
		  ((imp_mod_name, want_boot) : dep_mods deps, dep_pkgs deps)

	   | otherwise =
 	   	-- Imported module is from another package
		-- Dump the dependent modules
		-- Add the package imp_mod comes from to the dependent packages
	         ASSERT2( not (pkg `elem` dep_pkgs deps), ppr pkg <+> ppr (dep_pkgs deps) )
	         ([], pkg : dep_pkgs deps)

	-- True <=> import M ()
	import_all = case imp_details of
			Just (is_hiding, ls) -> not is_hiding && null ls	
			other 		     -> False

	imports   = ImportAvails { 
			imp_mods     = unitModuleEnv imp_mod (imp_mod, import_all, loc),
			imp_orphs    = orphans,
			imp_finsts   = finsts,
			imp_dep_mods = mkModDeps dependent_mods,
			imp_dep_pkgs = dependent_pkgs
                   }

	-- Complain if we import a deprecated module
    ifOptM Opt_WarnDeprecations	(
       case deprecs of	
	  DeprecAll txt -> addWarn (moduleDeprec imp_mod_name txt)
	  other	        -> returnM ()
     )

    let new_imp_decl = L loc (ImportDecl loc_imp_mod_name want_boot
                                         qual_only as_mod new_imp_details)

    returnM (new_imp_decl, gbl_env, imports)
    )

warnRedundantSourceImport mod_name
  = ptext SLIT("Unnecessary {-# SOURCE #-} in the import of module")
          <+> quotes (ppr mod_name)
\end{code}


%************************************************************************
%*									*
		importsFromLocalDecls
%*									*
%************************************************************************

From the top-level declarations of this module produce
  	* the lexical environment
	* the ImportAvails
created by its bindings.  
	
Complain about duplicate bindings

\begin{code}
importsFromLocalDecls :: HsGroup RdrName -> RnM TcGblEnv
importsFromLocalDecls group
  = do	{ gbl_env  <- getGblEnv

	; avails <- getLocalDeclBinders gbl_env group

	; rdr_env' <- extendRdrEnvRn (tcg_rdr_env gbl_env) avails

        ; traceRn (text "local avails: " <> ppr avails)

	; returnM (gbl_env { tcg_rdr_env = rdr_env' })
	}

extendRdrEnvRn :: GlobalRdrEnv -> [AvailInfo] -> RnM GlobalRdrEnv
-- Add the new locally-bound names one by one, checking for duplicates as
-- we do so.  Remember that in Template Haskell the duplicates
-- might *already be* in the GlobalRdrEnv from higher up the module
extendRdrEnvRn rdr_env avails
  = foldlM add_local rdr_env (gresFromAvails LocalDef avails)
  where
    add_local rdr_env gre
	| gres <- lookupGlobalRdrEnv rdr_env (nameOccName (gre_name gre))
	, (dup_gre:_) <- filter isLocalGRE gres	-- Check for existing *local* defns
	= do { addDupDeclErr (gre_name dup_gre) (gre_name gre)
	     ; return rdr_env }
	| otherwise
	= return (extendGlobalRdrEnv rdr_env gre)
\end{code}

@getLocalDeclBinders@ returns the names for an @HsDecl@.  It's
used for source code.

	*** See "THE NAMING STORY" in HsDecls ****

Instances of indexed types
~~~~~~~~~~~~~~~~~~~~~~~~~~
Indexed data/newtype instances contain data constructors that we need to
collect, too.  Moreover, we need to descend into the data/newtypes instances
of associated families.

We need to be careful with the handling of the type constructor of each type
instance as the family constructor is already defined, and we want to avoid
raising a duplicate declaration error.  So, we make a new name for it, but
don't return it in the 'AvailInfo'.

\begin{code}
getLocalDeclBinders :: TcGblEnv -> HsGroup RdrName -> RnM [AvailInfo]
getLocalDeclBinders gbl_env (HsGroup {hs_valds = ValBindsIn val_decls val_sigs,
				      hs_tyclds = tycl_decls, 
				      hs_instds = inst_decls,
				      hs_fords = foreign_decls })
  = do	{ tc_names_s <- mappM new_tc tycl_decls
	; at_names_s <- mappM inst_ats inst_decls
	; val_names  <- mappM new_simple val_bndrs
	; return (val_names ++ tc_names_s ++ concat at_names_s) }
  where
    mod        = tcg_mod gbl_env
    is_hs_boot = isHsBoot (tcg_src gbl_env) ;
    val_bndrs | is_hs_boot = sig_hs_bndrs
	      | otherwise  = for_hs_bndrs ++ val_hs_bndrs
	-- In a hs-boot file, the value binders come from the
	--  *signatures*, and there should be no foreign binders 

    new_simple rdr_name = do
        nm <- newTopSrcBinder mod rdr_name
        return (Avail nm)

    sig_hs_bndrs = [nm | L _ (TypeSig nm _) <- val_sigs]
    val_hs_bndrs = collectHsBindLocatedBinders val_decls
    for_hs_bndrs = [nm | L _ (ForeignImport nm _ _) <- foreign_decls]

    new_tc tc_decl 
      | isIdxTyDecl (unLoc tc_decl)
	= do { main_name <- lookupFamInstDeclBndr mod main_rdr
	     ; sub_names <- mappM (newTopSrcBinder mod) sub_rdrs
	     ; return (AvailTC main_name sub_names) }
                	-- main_name is not bound here!
      | otherwise
	= do { main_name <- newTopSrcBinder mod main_rdr
	     ; sub_names <- mappM (newTopSrcBinder mod) sub_rdrs
	     ; return (AvailTC main_name (main_name : sub_names)) }
      where
	(main_rdr : sub_rdrs) = tyClDeclNames (unLoc tc_decl)

    inst_ats inst_decl 
	= mappM new_tc (instDeclATs (unLoc inst_decl))

getLocalDeclBinders _ _ = panic "getLocalDeclBinders"	-- ValBindsOut can't happen
\end{code}


%************************************************************************
%*									*
\subsection{Filtering imports}
%*									*
%************************************************************************

@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

\begin{code}
filterImports :: ModIface
	      -> ImpDeclSpec			-- The span for the entire import decl
	      -> Maybe (Bool, [LIE RdrName])	-- Import spec; True => hiding
	      -> [AvailInfo]    		-- What's available
	      -> RnM (Maybe (Bool, [LIE Name]), -- Import spec w/ Names
		      GlobalRdrEnv)		-- Same again, but in GRE form
			
filterImports iface decl_spec Nothing all_avails
  = return (Nothing, mkGlobalRdrEnv (gresFromAvails prov all_avails))
  where
    prov = Imported [ImpSpec { is_decl = decl_spec, is_item = ImpAll }]


filterImports iface decl_spec (Just (want_hiding, import_items)) all_avails
  = do   -- check for errors, convert RdrNames to Names
        opt_indexedtypes <- doptM Opt_IndexedTypes
        items1 <- mapM (lookup_lie opt_indexedtypes) import_items

        let items2 :: [(LIE Name, AvailInfo)]
            items2 = concat items1
		-- NB the AvailInfo may have duplicates, and several items
		--    for the same parent; e.g N(x) and N(y)

            names  = availsToNameSet (map snd items2)
	    keep n = not (n `elemNameSet` names)
	    pruned_avails = filterAvails keep all_avails
	    hiding_prov = Imported [ImpSpec { is_decl = decl_spec, is_item = ImpAll }]

	    gres | want_hiding = gresFromAvails hiding_prov pruned_avails
		 | otherwise   = concatMap (gresFromIE decl_spec) items2

        return (Just (want_hiding, map fst items2), mkGlobalRdrEnv gres)
  where
	-- This environment is how we map names mentioned in the import
        -- list to the actual Name they correspond to, and the name family
        -- that the Name belongs to (the AvailInfo).  The situation is
        -- complicated by associated families, which introduce a three-level
        -- hierachy, where class = grand parent, assoc family = parent, and
        -- data constructors = children.  The occ_env entries for associated
        -- families needs to capture all this information; hence, we have the
        -- third component of the environment that gives the class name (=
        -- grand parent) in case of associated families.
        --
	-- This env will have entries for data constructors too,
	-- they won't make any difference because naked entities like T
	-- in an import list map to TcOccs, not VarOccs.
    occ_env :: OccEnv (Name,	    -- the name
		       AvailInfo,   -- the export item providing the name
		       Maybe Name)  -- the parent of associated types
    occ_env = mkOccEnv_C combine [ (nameOccName n, (n, a, Nothing)) 
			         | a <- all_avails, n <- availNames a]
      where
        -- we know that (1) there are at most entries for one name, (2) their
        -- first component is identical, (3) they are for tys/cls, and (4) one
        -- entry has the name in its parent position (the other doesn't)
        combine (name, AvailTC p1 subs1, Nothing)
		(_   , AvailTC p2 subs2, Nothing)
          = let
	      (parent, subs) = if p1 == name then (p2, subs1) else (p1, subs2)
	    in
	    (name, AvailTC name subs, Just parent)

    lookup_lie :: Bool -> LIE RdrName -> TcRn [(LIE Name, AvailInfo)]
    lookup_lie opt_indexedtypes (L loc ieRdr)
        = do 
             stuff <- setSrcSpan loc $ 
                      case lookup_ie opt_indexedtypes ieRdr of
                            Failed err  -> addErr err >> return []
                            Succeeded a -> return a
             checkDodgyImport stuff
             return [ (L loc ie, avail) | (ie,avail) <- stuff ]
        where
                -- Warn when importing T(..) if T was exported abstractly
            checkDodgyImport stuff
                | IEThingAll n <- ieRdr, (_, AvailTC _ [one]):_ <- stuff
                = ifOptM Opt_WarnDodgyImports (addWarn (dodgyImportWarn n))
                -- NB. use the RdrName for reporting the warning
            checkDodgyImport _
                = return ()

        -- For each import item, we convert its RdrNames to Names,
        -- and at the same time construct an AvailInfo corresponding
        -- to what is actually imported by this item.
        -- Returns Nothing on error.
        -- We return a list here, because in the case of an import
        -- item like C, if we are hiding, then C refers to *both* a
        -- type/class and a data constructor.  Moreover, when we import
	-- data constructors of an associated family, we need separate
	-- AvailInfos for the data constructors and the family (as they have
	-- different parents).  See the discussion at occ_env.
    lookup_ie :: Bool -> IE RdrName -> MaybeErr Message [(IE Name,AvailInfo)]
    lookup_ie opt_indexedtypes ie 
      = let bad_ie = Failed (badImportItemErr iface decl_spec ie)

            lookup_name rdrName = 
                case lookupOccEnv occ_env (rdrNameOcc rdrName) of
                   Nothing -> bad_ie
                   Just n  -> return n
        in
        case ie of
         IEVar n -> do
             (name, avail, _) <- lookup_name n
             return [(IEVar name, trimAvail avail name)]

         IEThingAll tc -> do
             (name, avail@(AvailTC name2 subs), mb_parent) <- lookup_name tc
             case mb_parent of
	       -- non-associated ty/cls
	       Nothing     -> return [(IEThingAll name, avail)]
	       -- associated ty
	       Just parent -> return [(IEThingAll name, 
				       AvailTC name2 (subs \\ [name])),
				      (IEThingAll name, AvailTC parent [name])]

         IEThingAbs tc
             | want_hiding   -- hiding ( C )
                        -- Here the 'C' can be a data constructor 
                        --  *or* a type/class, or even both
             -> let tc_name = lookup_name tc
                    dc_name = lookup_name (setRdrNameSpace tc srcDataName)
                in
                case catMaybeErr [ tc_name, dc_name ] of
                  []    -> bad_ie
                  names -> return [mkIEThingAbs name | name <- names]
             | otherwise
             -> do nameAvail <- lookup_name tc
                   return [mkIEThingAbs nameAvail]

         IEThingWith tc ns -> do
            (name, AvailTC name2 subnames, mb_parent) <- lookup_name tc
	    let 
	      env         = mkOccEnv [(nameOccName s, s) | s <- subnames]
	      mb_children = map (lookupOccEnv env . rdrNameOcc) ns
	    children <- if any isNothing mb_children
                        then bad_ie
                        else return (catMaybes mb_children)
              -- check for proper import of indexed types
	    when (not opt_indexedtypes && any isTyConName children) $
              Failed (typeItemErr (head . filter isTyConName $ children)
				  (text "in import list"))
            case mb_parent of
	       -- non-associated ty/cls
	      Nothing     -> return [(IEThingWith name children, 
				      AvailTC name (name:children))]
	       -- associated ty
	      Just parent -> return [(IEThingWith name children, 
				      AvailTC name children),
				     (IEThingWith name children, 
				      AvailTC parent [name])]

         _other -> Failed illegalImportItemErr
         -- could be IEModuleContents, IEGroup, IEDoc, IEDocNamed
         -- all errors.

      where
        mkIEThingAbs (n, av, Nothing    ) = (IEThingAbs n, trimAvail av n) 
	mkIEThingAbs (n, av, Just parent) = (IEThingAbs n, AvailTC parent [n]) 


catMaybeErr :: [MaybeErr err a] -> [a]
catMaybeErr ms =  [ a | Succeeded a <- ms ]
\end{code}

\begin{code}
filterImports2 :: ModIface
	      -> ImpDeclSpec			-- The span for the entire import decl
	      -> Maybe (Bool, [LIE RdrName])	-- Import spec; True => hiding
	      -> [AvailInfo]    		-- What's available
	      -> RnM (Maybe (Bool, [LIE Name]), -- Import spec w/ Names
		      GlobalRdrEnv)		-- Same again, but in GRE form
			
filterImports2 iface decl_spec Nothing all_avails
  = return (Nothing, mkGlobalRdrEnv (gresFromAvails prov all_avails))
  where
    prov = Imported [ImpSpec { is_decl = decl_spec, is_item = ImpAll }]


filterImports2 iface decl_spec (Just (want_hiding, import_items)) all_avails
  = do   -- check for errors, convert RdrNames to Names
        opt_indexedtypes <- doptM Opt_IndexedTypes
        items1 <- mapM (lookup_lie opt_indexedtypes) import_items

        let items2 :: [(LIE Name, AvailInfo)]
            items2 = concat items1
		-- NB the AvailInfo may have duplicates, and several items
		--    for the same parent; e.g N(x) and N(y)

            names  = availsToNameSet (map snd items2)
	    keep n = not (n `elemNameSet` names)
	    pruned_avails = filterAvails keep all_avails
	    hiding_prov = Imported [ImpSpec { is_decl = decl_spec, is_item = ImpAll }]

	    gres | want_hiding = gresFromAvails hiding_prov pruned_avails
		 | otherwise   = concatMap (gresFromIE decl_spec) items2

        return (Just (want_hiding, map fst items2), mkGlobalRdrEnv gres)
  where
	-- This environment is how we map names mentioned in the import
        -- list to the actual Name they correspond to, and the family
        -- that the Name belongs to (an AvailInfo).
        --
	-- This env will have entries for data constructors too,
	-- they won't make any difference because naked entities like T
	-- in an import list map to TcOccs, not VarOccs.
    occ_env :: OccEnv (Name,AvailInfo)
    occ_env = mkOccEnv [ (nameOccName n, (n,a)) 
                       | a <- all_avails, n <- availNames a ]

    lookup_lie :: Bool -> LIE RdrName -> TcRn [(LIE Name, AvailInfo)]
    lookup_lie opt_indexedtypes (L loc ieRdr)
        = do 
             stuff <- setSrcSpan loc $ 
                      case lookup_ie opt_indexedtypes ieRdr of
                            Failed err  -> addErr err >> return []
                            Succeeded a -> return a
             checkDodgyImport stuff
             return [ (L loc ie, avail) | (ie,avail) <- stuff ]
        where
                -- Warn when importing T(..) if T was exported abstractly
            checkDodgyImport stuff
                | IEThingAll n <- ieRdr, (_, AvailTC _ [one]):_ <- stuff
                = ifOptM Opt_WarnDodgyImports (addWarn (dodgyImportWarn n))
                -- NB. use the RdrName for reporting the warning
            checkDodgyImport _
                = return ()

        -- For each import item, we convert its RdrNames to Names,
        -- and at the same time construct an AvailInfo corresponding
        -- to what is actually imported by this item.
        -- Returns Nothing on error.
        -- We return a list here, because in the case of an import
        -- item like C, if we are hiding, then C refers to *both* a
        -- type/class and a data constructor.
    lookup_ie :: Bool -> IE RdrName -> MaybeErr Message [(IE Name,AvailInfo)]
    lookup_ie opt_indexedtypes ie 
      = let bad_ie = Failed (badImportItemErr iface decl_spec ie)

            lookup_name rdrName = 
                case lookupOccEnv occ_env (rdrNameOcc rdrName) of
                   Nothing -> bad_ie
                   Just n  -> return n
        in
        case ie of
         IEVar n -> do
             (name,avail) <- lookup_name n
             return [(IEVar name, trimAvail avail name)]

         IEThingAll tc -> do
             (name,avail) <- lookup_name tc
             return [(IEThingAll name, avail)]

         IEThingAbs tc
             | want_hiding   -- hiding ( C )
                        -- Here the 'C' can be a data constructor 
                        --  *or* a type/class, or even both
             -> let tc_name = lookup_name tc
                    dc_name = lookup_name (setRdrNameSpace tc srcDataName)
                in
                case catMaybeErr [ tc_name, dc_name ] of
                  []    -> bad_ie
                  names -> return [ (IEThingAbs n, trimAvail av n) 
				  | (n,av) <- names ]
             | otherwise
             -> do (name,avail) <- lookup_name tc
                   return [(IEThingAbs name, AvailTC name [name])]

         IEThingWith n ns -> do
            (name,avail) <- lookup_name n
            case avail of
                AvailTC nm subnames | nm == name -> do
                     let env = mkOccEnv [ (nameOccName s, s) 
                                        | s <- subnames ]
                     let mb_children = map (lookupOccEnv env . rdrNameOcc) ns
                     children <- 
                        if any isNothing mb_children
                          then bad_ie
                          else return (catMaybes mb_children)
                        -- check for proper import of indexed types
                     when (not opt_indexedtypes && any isTyConName children) $
                        Failed (typeItemErr (head . filter isTyConName 
                                                $ children )
			             (text "in import list"))
                     return [(IEThingWith name children, AvailTC name (name:children))]

                _otherwise -> bad_ie

         _other -> Failed illegalImportItemErr
         -- could be IEModuleContents, IEGroup, IEDoc, IEDocNamed
         -- all errors.
\end{code}

%************************************************************************
%*									*
        Import/Export Utils
%*									*
%************************************************************************

\begin{code}
-- | make a 'GlobalRdrEnv' where all the elements point to the same
-- import declaration (useful for "hiding" imports, or imports with
-- no details).
gresFromAvails :: Provenance -> [AvailInfo] -> [GlobalRdrElt]
gresFromAvails prov avails
  = concatMap (gresFromAvail (const prov)) avails

gresFromAvail :: (Name -> Provenance) -> AvailInfo -> [GlobalRdrElt]
gresFromAvail prov_fn avail
  = [ GRE {gre_name = n, 
	   gre_par = availParent n avail, 
	   gre_prov = prov_fn n}
    | n <- availNames avail ]
  
greAvail :: GlobalRdrElt -> AvailInfo
greAvail gre = mkUnitAvail (gre_name gre) (gre_par gre)

mkUnitAvail :: Name -> Parent -> AvailInfo
mkUnitAvail me (ParentIs p) 		 = AvailTC p  [me]
mkUnitAvail me NoParent | isTyConName me = AvailTC me [me]
			| otherwise	 = Avail me

plusAvail (Avail n1)	   (Avail n2)	    = Avail n1
plusAvail (AvailTC n1 ns1) (AvailTC n2 ns2) = AvailTC n2 (ns1 `unionLists` ns2)
plusAvail a1 a2 = pprPanic "RnEnv.plusAvail" (hsep [ppr a1,ppr a2])

availParent :: Name -> AvailInfo -> Parent
availParent n (Avail _) 		 = NoParent
availParent n (AvailTC m ms) | n==m      = NoParent
			     | otherwise = ParentIs m

trimAvail :: AvailInfo -> Name -> AvailInfo
trimAvail (Avail n)      m = Avail n
trimAvail (AvailTC n ns) m = ASSERT( m `elem` ns) AvailTC n [m]

-- | filters 'AvailInfo's by the given predicate
filterAvails  :: (Name -> Bool) -> [AvailInfo] -> [AvailInfo]
filterAvails keep avails = foldr (filterAvail keep) [] avails

-- | filters an 'AvailInfo' by the given predicate
filterAvail :: (Name -> Bool) -> AvailInfo -> [AvailInfo] -> [AvailInfo]
filterAvail keep ie rest =
  case ie of
    Avail n | keep n    -> ie : rest
            | otherwise -> rest
    AvailTC tc ns ->
        let left = filter keep ns in
        if null left then rest else AvailTC tc left : rest

-- | Given an import/export spec, construct the appropriate 'GlobalRdrElt's.
gresFromIE :: ImpDeclSpec -> (LIE Name, AvailInfo) -> [GlobalRdrElt]
gresFromIE decl_spec (L loc ie, avail)
  = gresFromAvail prov_fn avail
  where
    is_explicit = case ie of
		    IEThingAll name -> \n -> n==name
		    other	    -> \n -> True
    prov_fn name = Imported [imp_spec]
	where
	  imp_spec  = ImpSpec { is_decl = decl_spec, is_item = item_spec }
	  item_spec = ImpSome { is_explicit = is_explicit name, is_iloc = loc }

mkChildEnv :: [GlobalRdrElt] -> NameEnv [Name]
mkChildEnv gres = foldr add emptyNameEnv gres
    where
	add (GRE { gre_name = n, gre_par = ParentIs p }) env = extendNameEnv_C (++) env p [n]
	add other_gre					 env = env

findChildren :: NameEnv [Name] -> Name -> [Name]
findChildren env n = lookupNameEnv env n `orElse` []
\end{code}

---------------------------------------
	AvailEnv and friends

All this AvailEnv stuff is hardly used; only in a very small
part of RnNames.  Todo: remove?
---------------------------------------

\begin{code}
type AvailEnv = NameEnv AvailInfo	-- Maps a Name to the AvailInfo that contains it

emptyAvailEnv :: AvailEnv
emptyAvailEnv = emptyNameEnv

unitAvailEnv :: AvailInfo -> AvailEnv
unitAvailEnv a = unitNameEnv (availName a) a

plusAvailEnv :: AvailEnv -> AvailEnv -> AvailEnv
plusAvailEnv = plusNameEnv_C plusAvail

availEnvElts :: AvailEnv -> [AvailInfo]
availEnvElts = nameEnvElts

addAvail :: AvailEnv -> AvailInfo -> AvailEnv
addAvail avails avail = extendNameEnv_C plusAvail avails (availName avail) avail

mkAvailEnv :: [AvailInfo] -> AvailEnv
	-- 'avails' may have several items with the same availName
	-- E.g  import Ix( Ix(..), index )
	-- will give Ix(Ix,index,range) and Ix(index)
	-- We want to combine these; addAvail does that
mkAvailEnv avails = foldl addAvail emptyAvailEnv avails

-- After combining the avails, we need to ensure that the parent name is the
-- first entry in the list of subnames, if it is included at all.  (Subsequent
-- functions rely on that.)
normaliseAvail :: AvailInfo -> AvailInfo
normaliseAvail avail@(Avail _)     = avail
normaliseAvail (AvailTC name subs) = AvailTC name subs'
  where
    subs' = if name `elem` subs then name : (delete name subs) else subs

-- | combines 'AvailInfo's from the same family
nubAvails :: [AvailInfo] -> [AvailInfo]
nubAvails avails = map normaliseAvail . nameEnvElts . mkAvailEnv $ avails
\end{code}


%************************************************************************
%*									*
\subsection{Export list processing}
%*									*
%************************************************************************

Processing the export list.

You might think that we should record things that appear in the export
list as ``occurrences'' (using @addOccurrenceName@), but you'd be
wrong.  We do check (here) that they are in scope, but there is no
need to slurp in their actual declaration (which is what
@addOccurrenceName@ forces).

Indeed, doing so would big trouble when compiling @PrelBase@, because
it re-exports @GHC@, which includes @takeMVar#@, whose type includes
@ConcBase.StateAndSynchVar#@, and so on...

\begin{code}
type ExportAccum	-- The type of the accumulating parameter of
			-- the main worker function in rnExports
     = ([LIE Name],             -- Export items with Names
	ExportOccMap,		-- Tracks exported occurrence names
	[AvailInfo])	        -- The accumulated exported stuff
				--   Not nub'd!

emptyExportAccum = ([], emptyOccEnv, []) 

type ExportOccMap = OccEnv (Name, IE RdrName)
	-- Tracks what a particular exported OccName
	--   in an export list refers to, and which item
	--   it came from.  It's illegal to export two distinct things
	--   that have the same occurrence name

rnExports :: Bool    -- False => no 'module M(..) where' header at all
          -> Maybe [LIE RdrName]        -- Nothing => no explicit export list
          -> RnM (Maybe [LIE Name], [AvailInfo])

	-- Complains if two distinct exports have same OccName
        -- Warns about identical exports.
	-- Complains about exports items not in scope

rnExports explicit_mod exports
 = do TcGblEnv { tcg_mod     = this_mod,
                 tcg_rdr_env = rdr_env, 
                 tcg_imports = imports } <- getGblEnv

	-- If the module header is omitted altogether, then behave
	-- as if the user had written "module Main(main) where..."
	-- EXCEPT in interactive mode, when we behave as if he had
	-- written "module Main where ..."
	-- Reason: don't want to complain about 'main' not in scope
	--	   in interactive mode
      ghc_mode <- getGhcMode
      real_exports <- 
          case () of
            () | explicit_mod
                   -> return exports
               | ghc_mode == Interactive
                   -> return Nothing
               | otherwise
                   -> do mainName <- lookupGlobalOccRn main_RDR_Unqual
                         return (Just ([noLoc (IEVar main_RDR_Unqual)]))
		-- ToDo: the 'noLoc' here is unhelpful if 'main' turns
		-- out to be out of scope

      (exp_spec, avails) <- exports_from_avail real_exports rdr_env imports this_mod

      return (exp_spec, nubAvails avails)     -- Combine families

exports_from_avail :: Maybe [LIE RdrName]
                         -- Nothing => no explicit export list
                   -> GlobalRdrEnv
                   -> ImportAvails
                   -> Module
                   -> RnM (Maybe [LIE Name], [AvailInfo])

exports_from_avail Nothing rdr_env imports this_mod
 = -- The same as (module M) where M is the current module name,
   -- so that's how we handle it.
   let
       avails = [ greAvail gre | gre <- globalRdrEnvElts rdr_env,
                                 isLocalGRE gre ]
   in
   return (Nothing, avails)

exports_from_avail (Just rdr_items) rdr_env imports this_mod
  = do (ie_names, _, exports) <- foldlM do_litem emptyExportAccum rdr_items

       return (Just ie_names, exports)
  where
    do_litem :: ExportAccum -> LIE RdrName -> RnM ExportAccum
    do_litem acc lie = setSrcSpan (getLoc lie) (exports_from_item acc lie)

    kids_env :: NameEnv [Name]	-- Maps a parent to its in-scope children
    kids_env = mkChildEnv (globalRdrEnvElts rdr_env)

    exports_from_item :: ExportAccum -> LIE RdrName -> RnM ExportAccum
    exports_from_item acc@(ie_names, occs, exports) 
                      (L loc ie@(IEModuleContents mod))
	| let earlier_mods = [ mod | (L _ (IEModuleContents mod)) <- ie_names ]
	, mod `elem` earlier_mods 	-- Duplicate export of M
	= do { warn_dup_exports <- doptM Opt_WarnDuplicateExports ;
	       warnIf warn_dup_exports (dupModuleExport mod) ;
	       returnM acc }

	| otherwise
	= do { implicit_prelude <- doptM Opt_ImplicitPrelude
	     ; let gres = filter (isModuleExported implicit_prelude mod) 
				 (globalRdrEnvElts rdr_env)

	     ; warnIf (null gres) (nullModuleExport mod)

	     ; occs' <- check_occs ie occs (map gre_name gres)
                      -- This check_occs not only finds conflicts
                      -- between this item and others, but also
                      -- internally within this item.  That is, if
                      -- 'M.x' is in scope in several ways, we'll have
                      -- several members of mod_avails with the same
                      -- OccName.
	     ; return (L loc (IEModuleContents mod) : ie_names,
                       occs', map greAvail gres ++ exports) }

    exports_from_item acc@(lie_names, occs, exports) (L loc ie)
	| isDoc ie
	= do new_ie <- lookup_doc_ie ie
	     return (L loc new_ie : lie_names, occs, exports)

	| otherwise
        = do (new_ie, avail) <- lookup_ie ie
             if isUnboundName (ieName new_ie)
                  then return acc 	-- Avoid error cascade
                  else do

             occs' <- check_occs ie occs (availNames avail)

             return (L loc new_ie : lie_names, occs', avail : exports)

    -------------
    lookup_ie :: IE RdrName -> RnM (IE Name, AvailInfo)
    lookup_ie (IEVar rdr) 
        = do gre <- lookupGreRn rdr
             return (IEVar (gre_name gre), greAvail gre)

    lookup_ie (IEThingAbs rdr) 
        = do name <- lookupGlobalOccRn rdr
	     case lookupGRE_RdrName rdr rdr_env of
	       []    -> panic "RnNames.lookup_ie"
	       elt:_ -> case gre_par elt of
			  NoParent   -> return (IEThingAbs name, 
						AvailTC name [name])
			  ParentIs p -> return (IEThingAbs name, 
						AvailTC p [name])

    lookup_ie ie@(IEThingAll rdr) 
        = do name <- lookupGlobalOccRn rdr
	     let kids = findChildren kids_env name
	     when (null kids)
		  (if (isTyConName name) then addWarn (dodgyExportWarn name)
				-- This occurs when you export T(..), but
				-- only import T abstractly, or T is a synonym.  
		   else addErr (exportItemErr ie))
			
             return (IEThingAll name, AvailTC name (name:kids))

    lookup_ie ie@(IEThingWith rdr sub_rdrs)
        = do name <- lookupGlobalOccRn rdr
             if isUnboundName name
                then return (IEThingWith name [], AvailTC name [name])
                else do
             let env = mkOccEnv [ (nameOccName s, s) 
                                | s <- findChildren kids_env name ]
                 mb_names = map (lookupOccEnv env . rdrNameOcc) sub_rdrs
             if any isNothing mb_names
                then do addErr (exportItemErr ie)
                        return (IEThingWith name [], AvailTC name [name])
                else do let names = catMaybes mb_names
                        optIdxTypes <- doptM Opt_IndexedTypes
                        when (not optIdxTypes && any isTyConName names) $
                          addErr (typeItemErr ( head
                                              . filter isTyConName 
                                              $ names )
                                              (text "in export list"))
                        return (IEThingWith name names, AvailTC name (name:names))

    lookup_ie ie = panic "lookup_ie"	-- Other cases covered earlier

    -------------
    lookup_doc_ie :: IE RdrName -> RnM (IE Name)
    lookup_doc_ie (IEGroup lev doc) = do rn_doc <- rnHsDoc doc
					 return (IEGroup lev rn_doc)
    lookup_doc_ie (IEDoc doc)       = do rn_doc <- rnHsDoc doc
				         return (IEDoc rn_doc)
    lookup_doc_ie (IEDocNamed str)  = return (IEDocNamed str)
    lookup_doc_ie ie = panic "lookup_doc_ie"	-- Other cases covered earlier


isDoc (IEDoc _)      = True
isDoc (IEDocNamed _) = True
isDoc (IEGroup _ _)  = True
isDoc _ = False

-------------------------------
isModuleExported :: Bool -> ModuleName -> GlobalRdrElt -> Bool
-- True if the thing is in scope *both* unqualified, *and* with qualifier M
isModuleExported implicit_prelude mod (GRE { gre_name = name, gre_prov = prov })
  | implicit_prelude && isBuiltInSyntax name = False
	-- Optimisation: filter out names for built-in syntax
	-- They just clutter up the environment (esp tuples), and the parser
	-- will generate Exact RdrNames for them, so the cluttered
	-- envt is no use.  To avoid doing this filter all the time,
	-- we use -fno-implicit-prelude as a clue that the filter is
	-- worth while.  Really, it's only useful for GHC.Base and GHC.Tuple.
	--
	-- It's worth doing because it makes the environment smaller for
	-- every module that imports the Prelude
  | otherwise
  = case prov of
	LocalDef    -> moduleName (nameModule name) == mod
	Imported is -> any unQualSpecOK is && any (qualSpecOK mod) is

-------------------------------
check_occs :: IE RdrName -> ExportOccMap -> [Name] -> RnM ExportOccMap
check_occs ie occs names
  = foldlM check occs names
  where
    check occs name
      = case lookupOccEnv occs name_occ of
	  Nothing -> returnM (extendOccEnv occs name_occ (name, ie))

	  Just (name', ie') 
	    | name == name'  	-- Duplicate export
	    ->	do { warn_dup_exports <- doptM Opt_WarnDuplicateExports ;
		     warnIf warn_dup_exports (dupExportWarn name_occ ie ie') ;
		     returnM occs }

	    | otherwise		-- Same occ name but different names: an error
	    ->	do { global_env <- getGlobalRdrEnv ;
  		     addErr (exportClashErr global_env name' name ie' ie) ;
		     returnM occs }
      where
	name_occ = nameOccName name
\end{code}

%*********************************************************
%*						 	 *
		Deprecations
%*							 *
%*********************************************************

\begin{code}
reportDeprecations :: DynFlags -> TcGblEnv -> RnM ()
reportDeprecations dflags tcg_env
  = ifOptM Opt_WarnDeprecations	$
    do	{ (eps,hpt) <- getEpsAndHpt
		-- By this time, typechecking is complete, 
		-- so the PIT is fully populated
	; mapM_ (check hpt (eps_PIT eps)) all_gres }
  where
    used_names = allUses (tcg_dus tcg_env) 
	-- Report on all deprecated uses; hence allUses
    all_gres   = globalRdrEnvElts (tcg_rdr_env tcg_env)

    check hpt pit gre@(GRE {gre_name = name, gre_prov = Imported (imp_spec:_)})
      | name `elemNameSet` used_names
      ,	Just deprec_txt <- lookupDeprec dflags hpt pit gre
      = addWarnAt (importSpecLoc imp_spec)
		  (sep [ptext SLIT("Deprecated use of") <+> 
			pprNonVarNameSpace (occNameSpace (nameOccName name)) <+> 
		 	quotes (ppr name),
		      (parens imp_msg) <> colon,
		      (ppr deprec_txt) ])
	where
	  name_mod = nameModule name
	  imp_mod  = importSpecModule imp_spec
	  imp_msg  = ptext SLIT("imported from") <+> ppr imp_mod <> extra
	  extra | imp_mod == moduleName name_mod = empty
		| otherwise = ptext SLIT(", but defined in") <+> ppr name_mod

    check hpt pit ok_gre = returnM ()	-- Local, or not used, or not deprectated
	    -- The Imported pattern-match: don't deprecate locally defined names
	    -- For a start, we may be exporting a deprecated thing
	    -- Also we may use a deprecated thing in the defn of another
	    -- deprecated things.  We may even use a deprecated thing in
	    -- the defn of a non-deprecated thing, when changing a module's 
	    -- interface

lookupDeprec :: DynFlags -> HomePackageTable -> PackageIfaceTable 
	     -> GlobalRdrElt -> Maybe DeprecTxt
lookupDeprec dflags hpt pit gre
  = case lookupIfaceByModule dflags hpt pit (nameModule name) of
	Just iface -> mi_dep_fn iface name `seqMaybe` 	-- Bleat if the thing, *or
		      case gre_par gre of	
			ParentIs p -> mi_dep_fn iface p	-- its parent*, is deprec'd
			NoParent   -> Nothing
	Nothing    
	  | isWiredInName name -> Nothing
		-- We have not necessarily loaded the .hi file for a 
		-- wired-in name (yet), although we *could*.
		-- And we never deprecate them

	 | otherwise -> pprPanic "lookupDeprec" (ppr name)	
		-- By now all the interfaces should have been loaded
  where
	name = gre_name gre
\end{code}

%*********************************************************
%*						 	 *
		Unused names
%*							 *
%*********************************************************

\begin{code}
reportUnusedNames :: Maybe [LIE RdrName] 	-- Export list
		  -> TcGblEnv -> RnM ()
reportUnusedNames export_decls gbl_env 
  = do	{ traceRn ((text "RUN") <+> (ppr (tcg_dus gbl_env)))
	; warnUnusedTopBinds   unused_locals
	; warnUnusedModules    unused_imp_mods
	; warnUnusedImports    unused_imports	
	; warnDuplicateImports defined_and_used
	; printMinimalImports  minimal_imports }
  where
    used_names :: NameSet
    used_names = findUses (tcg_dus gbl_env) emptyNameSet
	-- NB: currently, if f x = g, we only treat 'g' as used if 'f' is used
	-- Hence findUses

	-- Collect the defined names from the in-scope environment
    defined_names :: [GlobalRdrElt]
    defined_names = globalRdrEnvElts (tcg_rdr_env gbl_env)

	-- Note that defined_and_used, defined_but_not_used
	-- are both [GRE]; that's why we need defined_and_used
	-- rather than just used_names
    defined_and_used, defined_but_not_used :: [GlobalRdrElt]
    (defined_and_used, defined_but_not_used) 
	= partition (gre_is_used used_names) defined_names
    
    kids_env = mkChildEnv defined_names
	-- This is done in mkExports too; duplicated work

    gre_is_used :: NameSet -> GlobalRdrElt -> Bool
    gre_is_used used_names (GRE {gre_name = name})
	= name `elemNameSet` used_names
	  || any (`elemNameSet` used_names) (findChildren kids_env name)
		-- A use of C implies a use of T,
		-- if C was brought into scope by T(..) or T(C)

	-- Filter out the ones that are 
	--  (a) defined in this module, and
	--  (b) not defined by a 'deriving' clause 
	-- The latter have an Internal Name, so we can filter them out easily
    unused_locals :: [GlobalRdrElt]
    unused_locals = filter is_unused_local defined_but_not_used
    is_unused_local :: GlobalRdrElt -> Bool
    is_unused_local gre = isLocalGRE gre && isExternalName (gre_name gre)
    
    unused_imports :: [GlobalRdrElt]
    unused_imports = filter unused_imp defined_but_not_used
    unused_imp (GRE {gre_prov = Imported imp_specs}) 
	= not (all (module_unused . importSpecModule) imp_specs)
	  && or [exp | ImpSpec { is_item = ImpSome { is_explicit = exp } } <- imp_specs]
		-- Don't complain about unused imports if we've already said the
		-- entire import is unused
    unused_imp other = False
    
    -- To figure out the minimal set of imports, start with the things
    -- that are in scope (i.e. in gbl_env).  Then just combine them
    -- into a bunch of avails, so they are properly grouped
    --
    -- BUG WARNING: this does not deal properly with qualified imports!
    minimal_imports :: FiniteMap ModuleName AvailEnv
    minimal_imports0 = foldr add_expall   emptyFM 	   expall_mods
    minimal_imports1 = foldr add_name     minimal_imports0 defined_and_used
    minimal_imports  = foldr add_inst_mod minimal_imports1 direct_import_mods
 	-- The last line makes sure that we retain all direct imports
    	-- even if we import nothing explicitly.
    	-- It's not necessarily redundant to import such modules. Consider 
    	--	      module This
    	--		import M ()
    	--
    	-- The import M() is not *necessarily* redundant, even if
    	-- we suck in no instance decls from M (e.g. it contains 
    	-- no instance decls, or This contains no code).  It may be 
    	-- that we import M solely to ensure that M's orphan instance 
    	-- decls (or those in its imports) are visible to people who 
    	-- import This.  Sigh. 
    	-- There's really no good way to detect this, so the error message 
    	-- in RnEnv.warnUnusedModules is weakened instead
    
	-- We've carefully preserved the provenance so that we can
	-- construct minimal imports that import the name by (one of)
	-- the same route(s) as the programmer originally did.
    add_name gre@(GRE {gre_prov = Imported (imp_spec:_)}) acc 
	= addToFM_C plusAvailEnv acc 
		    (importSpecModule imp_spec) (unitAvailEnv (greAvail gre))
    add_name gre acc = acc	-- Local

	-- Modules mentioned as 'module M' in the export list
    expall_mods = case export_decls of
		    Nothing -> []
		    Just es -> [m | L _ (IEModuleContents m) <- es]

	-- This is really bogus.  The idea is that if we see 'module M' in 
	-- the export list we must retain the import decls that drive it
	-- If we aren't careful we might see
	--	module A( module M ) where
	--	  import M
	--	  import N
	-- and suppose that N exports everything that M does.  Then we 
	-- must not drop the import of M even though N brings it all into
	-- scope.
	--
	-- BUG WARNING: 'module M' exports aside, what if M.x is mentioned?!
	--
	-- The reason that add_expall is bogus is that it doesn't take
	-- qualified imports into account.  But it's an improvement.
    add_expall mod acc = addToFM_C plusAvailEnv acc mod emptyAvailEnv

    add_inst_mod (mod,_,_) acc 
      | mod_name `elemFM` acc = acc	-- We import something already
      | otherwise	      = addToFM acc mod_name emptyAvailEnv
      where
	mod_name = moduleName mod
    	-- Add an empty collection of imports for a module
    	-- from which we have sucked only instance decls
   
    imports = tcg_imports gbl_env

    direct_import_mods :: [(Module, Bool, SrcSpan)]
	-- See the type of the imp_mods for this triple
    direct_import_mods = moduleEnvElts (imp_mods imports)

    -- unused_imp_mods are the directly-imported modules 
    -- that are not mentioned in minimal_imports1
    -- [Note: not 'minimal_imports', because that includes directly-imported
    --	      modules even if we use nothing from them; see notes above]
    --
    -- BUG WARNING: does not deal correctly with multiple imports of the same module
    --	 	    becuase direct_import_mods has only one entry per module
    unused_imp_mods = [(mod_name,loc) | (mod,no_imp,loc) <- direct_import_mods,
		       let mod_name = moduleName mod,
    		       not (mod_name `elemFM` minimal_imports1),
    		       mod /= pRELUDE,
		       not no_imp]
	-- The not no_imp part is not to complain about
	-- import M (), which is an idiom for importing
	-- instance declarations
    
    module_unused :: ModuleName -> Bool
    module_unused mod = any (((==) mod) . fst) unused_imp_mods

---------------------
warnDuplicateImports :: [GlobalRdrElt] -> RnM ()
-- Given the GREs for names that are used, figure out which imports 
-- could be omitted without changing the top-level environment.
--
-- NB: Given import Foo( T )
--     	     import qualified Foo
-- we do not report a duplicate import, even though Foo.T is brought
-- into scope by both, because there's nothing you can *omit* without
-- changing the top-level environment.  So we complain only if it's
-- explicitly named in both imports or neither.
--
-- Furthermore, we complain about Foo.T only if 
-- there is no complaint about (unqualified) T

warnDuplicateImports gres
  = ifOptM Opt_WarnUnusedImports $ 
    sequenceM_	[ warn name pr
			-- The 'head' picks the first offending group
			-- for this particular name
		| GRE { gre_name = name, gre_prov = Imported imps } <- gres
		, pr <- redundants imps ]
  where
    warn name (red_imp, cov_imp)
	= addWarnAt (importSpecLoc red_imp)
	    (vcat [ptext SLIT("Redundant import of:") <+> quotes pp_name,
	           ptext SLIT("It is also") <+> ppr cov_imp])
	where
	  pp_name | is_qual red_decl = ppr (is_as red_decl) <> dot <> ppr occ
		  | otherwise	    = ppr occ
	  occ = nameOccName name
	  red_decl = is_decl red_imp
    
    redundants :: [ImportSpec] -> [(ImportSpec,ImportSpec)]
	-- The returned pair is (redundant-import, covering-import)
    redundants imps 
	= [ (red_imp, cov_imp) 
	  | red_imp <- imps
	  , cov_imp <- take 1 (filter (covers red_imp) imps) ]

	-- "red_imp" is a putative redundant import
	-- "cov_imp" potentially covers it
	-- This test decides whether red_imp could be dropped 
	--
	-- NOTE: currently the test does not warn about
	--		import M( x )
	--		imoprt N( x )
	-- even if the same underlying 'x' is involved, because dropping
	-- either import would change the qualified names in scope (M.x, N.x)
	-- But if the qualified names aren't used, the import is indeed redundant
	-- Sadly we don't know that.  Oh well.
    covers red_imp@(ImpSpec { is_decl = red_decl, is_item = red_item }) 
	   cov_imp@(ImpSpec { is_decl = cov_decl, is_item = cov_item })
	| red_loc == cov_loc
  	= False		-- Ignore diagonal elements
	| not (is_as red_decl == is_as cov_decl)
	= False		-- They bring into scope different qualified names
	| not (is_qual red_decl) && is_qual cov_decl
	= False		-- Covering one doesn't bring unqualified name into scope
	| red_selective
	= not cov_selective 	-- Redundant one is selective and covering one isn't
	  || red_later		-- Both are explicit; tie-break using red_later
	| otherwise		
	= not cov_selective 	-- Neither import is selective
	  && (is_mod red_decl == is_mod cov_decl)	-- They import the same module
	  && red_later 		-- Tie-break
	where
	  red_loc   = importSpecLoc red_imp
	  cov_loc   = importSpecLoc cov_imp
	  red_later = red_loc > cov_loc
	  cov_selective = selectiveImpItem cov_item
	  red_selective = selectiveImpItem red_item

selectiveImpItem :: ImpItemSpec -> Bool
selectiveImpItem ImpAll       = False
selectiveImpItem (ImpSome {}) = True

-- ToDo: deal with original imports with 'qualified' and 'as M' clauses
printMinimalImports :: FiniteMap ModuleName AvailEnv	-- Minimal imports
		    -> RnM ()
printMinimalImports imps
 = ifOptM Opt_D_dump_minimal_imports $ do {

   mod_ies  <-  mappM to_ies (fmToList imps) ;
   this_mod <- getModule ;
   rdr_env  <- getGlobalRdrEnv ;
   ioToTcRn (do { h <- openFile (mkFilename this_mod) WriteMode ;
		  printForUser h (mkPrintUnqualified rdr_env) 
				 (vcat (map ppr_mod_ie mod_ies)) })
   }
  where
    mkFilename this_mod = moduleNameString (moduleName this_mod) ++ ".imports"
    ppr_mod_ie (mod_name, ies) 
	| mod_name == moduleName pRELUDE
	= empty
	| null ies	-- Nothing except instances comes from here
	= ptext SLIT("import") <+> ppr mod_name <> ptext SLIT("()    -- Instances only")
	| otherwise
	= ptext SLIT("import") <+> ppr mod_name <> 
		    parens (fsep (punctuate comma (map ppr ies)))

    to_ies (mod, avail_env) = do ies <- mapM to_ie (availEnvElts avail_env)
                                 returnM (mod, ies)

    to_ie :: AvailInfo -> RnM (IE Name)
	-- The main trick here is that if we're importing all the constructors
	-- we want to say "T(..)", but if we're importing only a subset we want
	-- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie (Avail n)       = returnM (IEVar n)
    to_ie (AvailTC n [m]) = ASSERT( n==m ) 
			    returnM (IEThingAbs n)
    to_ie (AvailTC n ns)  
	= loadSrcInterface doc n_mod False			`thenM` \ iface ->
	  case [xs | (m,as) <- mi_exports iface,
		     moduleName m == n_mod,
		     AvailTC x xs <- as, 
		     x == nameOccName n] of
	      [xs] | all_used xs -> returnM (IEThingAll n)
		   | otherwise	 -> returnM (IEThingWith n (filter (/= n) ns))
	      other		 -> pprTrace "to_ie" (ppr n <+> ppr n_mod <+> ppr other) $
				    returnM (IEVar n)
	where
	  all_used avail_occs = all (`elem` map nameOccName ns) avail_occs
	  doc = text "Compute minimal imports from" <+> ppr n
	  n_mod = moduleName (nameModule n)
\end{code}


%************************************************************************
%*									*
\subsection{Errors}
%*									*
%************************************************************************

\begin{code}
badImportItemErr iface decl_spec ie
  = sep [ptext SLIT("Module"), quotes (ppr (is_mod decl_spec)), source_import,
	 ptext SLIT("does not export"), quotes (ppr ie)]
  where
    source_import | mi_boot iface = ptext SLIT("(hi-boot interface)")
		  | otherwise     = empty

illegalImportItemErr = ptext SLIT("Illegal import item")

dodgyImportWarn item = dodgyMsg (ptext SLIT("import")) item
dodgyExportWarn item = dodgyMsg (ptext SLIT("export")) item

dodgyMsg kind tc
  = sep [ ptext SLIT("The") <+> kind <+> ptext SLIT("item") <+> quotes (ppr (IEThingAll tc)),
	  ptext SLIT("suggests that") <+> quotes (ppr tc) <+> ptext SLIT("has constructor or class methods"),
	  ptext SLIT("but it has none; it is a type synonym or abstract type or class") ]
	  
exportItemErr export_item
  = sep [ ptext SLIT("The export item") <+> quotes (ppr export_item),
	  ptext SLIT("attempts to export constructors or class methods that are not visible here") ]

typeItemErr name wherestr
  = sep [ ptext SLIT("Using 'type' tag on") <+> quotes (ppr name) <+> wherestr,
	  ptext SLIT("Use -findexed-types to enable this extension") ]

exportClashErr global_env name1 name2 ie1 ie2
  = vcat [ ptext SLIT("Conflicting exports for") <+> quotes (ppr occ) <> colon
	 , ppr_export ie1 name1 
	 , ppr_export ie2 name2  ]
  where
    occ = nameOccName name1
    ppr_export ie name = nest 2 (quotes (ppr ie) <+> ptext SLIT("exports") <+> 
			 	 quotes (ppr name) <+> pprNameProvenance (get_gre name))

	-- get_gre finds a GRE for the Name, so that we can show its provenance
    get_gre name
	= case lookupGRE_Name global_env name of
	     (gre:_) -> gre
	     []	     -> pprPanic "exportClashErr" (ppr name)

addDupDeclErr :: Name -> Name -> TcRn ()
addDupDeclErr name_a name_b
  = addErrAt (srcLocSpan loc2) $
    vcat [ptext SLIT("Multiple declarations of") <+> quotes (ppr name1),
	  ptext SLIT("Declared at:") <+> vcat [ppr (nameSrcLoc name1), ppr loc2]]
  where
    loc2 = nameSrcLoc name2
    (name1,name2) | nameSrcLoc name_a > nameSrcLoc name_b = (name_b,name_a)
		  | otherwise				  = (name_a,name_b)
	-- Report the error at the later location

dupExportWarn occ_name ie1 ie2
  = hsep [quotes (ppr occ_name), 
          ptext SLIT("is exported by"), quotes (ppr ie1),
          ptext SLIT("and"),            quotes (ppr ie2)]

dupModuleExport mod
  = hsep [ptext SLIT("Duplicate"),
	  quotes (ptext SLIT("Module") <+> ppr mod), 
          ptext SLIT("in export list")]

nullModuleExport mod
  = ptext SLIT("The export item `module") <+> ppr mod <> ptext SLIT("' exports nothing")

moduleDeprec mod txt
  = sep [ ptext SLIT("Module") <+> quotes (ppr mod) <+> ptext SLIT("is deprecated:"), 
	  nest 4 (ppr txt) ]	  
\end{code}
