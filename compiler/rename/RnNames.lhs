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
import NameSet
import NameEnv
import OccName		( srcDataName, isTcOcc, pprNonVarNameSpace,
			  occNameSpace,
			  OccEnv, mkOccEnv, lookupOccEnv, emptyOccEnv,
			  extendOccEnv )
import HscTypes		( GenAvailInfo(..), AvailInfo, availNames, availName,
			  HomePackageTable, PackageIfaceTable, 
			  mkPrintUnqualified, availsToNameSet,
                          availsToNameEnv,
			  Deprecs(..), ModIface(..), Dependencies(..), 
			  lookupIfaceByModule, ExternalPackageState(..)
			)
import RdrName		( RdrName, rdrNameOcc, setRdrNameSpace, 
		  	  GlobalRdrEnv, mkGlobalRdrEnv, GlobalRdrElt(..), 
			  emptyGlobalRdrEnv, plusGlobalRdrEnv, globalRdrEnvElts,
			  extendGlobalRdrEnv, lookupGlobalRdrEnv, unQualOK, lookupGRE_Name,
			  Provenance(..), ImportSpec(..), ImpDeclSpec(..), ImpItemSpec(..), 
			  importSpecLoc, importSpecModule, isLocalGRE, pprNameProvenance )
import Outputable
import UniqFM
import Maybes
import SrcLoc		( Located(..), mkGeneralSrcSpan, getLoc,
			  unLoc, noLoc, srcLocSpan, SrcSpan )
import FiniteMap
import ErrUtils
import BasicTypes	( DeprecTxt )
import DriverPhases	( isHsBoot )
import Util		( notNull )
import Data.List        ( nub, partition, concatMap )
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
         let (decls, rdr_env, avails, imp_avails) = combine (stuff1 ++ stuff2)
         return (decls, rdr_env,
                 imp_avails{ imp_parent = availsToNameEnv (nubAvails avails) })
                    -- why wait until now to set the imp_parent, rather than
                    -- setting it in rnImportDecl for each import, and 
                    -- combining them with plusImportAvails?  The reason is
                    -- that we need to combine all the AvailInfos *before*
                    -- we build the NameEnv, otherwise the NameEnv can
                    -- end up with inconsistencies, eg. the parent can say
                    -- C(m1,m2), but the entry for m2 might only say C(m2).
                    -- The test mod118 illustrates the bug.
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

   combine :: [(LImportDecl Name,  GlobalRdrEnv, [AvailInfo], ImportAvails)]
           -> ([LImportDecl Name], GlobalRdrEnv, [AvailInfo], ImportAvails)
   combine = foldr plus ([], emptyGlobalRdrEnv, [], emptyImportAvails)
        where plus (decl,  gbl_env1, avails1, imp_avails1)
                   (decls, gbl_env2, avails2, imp_avails2)
                = (decl:decls, 
                   gbl_env1 `plusGlobalRdrEnv` gbl_env2,
                   avails1 ++ avails2,                   
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
	      -> RnM (LImportDecl Name, GlobalRdrEnv,
                      [AvailInfo], ImportAvails)

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
    WARN( not want_boot && mi_boot iface, ppr imp_mod_name ) $ do

	-- Issue a user warning for a redundant {- SOURCE -} import
	-- NB that we arrange to read all the ordinary imports before 
	-- any of the {- SOURCE -} imports
    warnIf (want_boot && not (mi_boot iface))
	   (warnRedundantSourceImport imp_mod_name)

    let
	imp_mod	= mi_module iface
	deprecs	= mi_deprecs iface
	is_orph	= mi_orphan iface 
	deps 	= mi_deps iface

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
    (new_imp_details, filtered_avails, gbl_env) <- 
        filterImports iface imp_spec imp_details total_avails

    dflags <- getDOpts

    let
	-- Compute new transitive dependencies

 	orphans | is_orph   = ASSERT( not (imp_mod `elem` dep_orphs deps) )
			      imp_mod : dep_orphs deps
		| otherwise = dep_orphs deps

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
			imp_env      = unitUFM qual_mod_name filtered_avails,
			imp_mods     = unitModuleEnv imp_mod (imp_mod, import_all, loc),
			imp_orphs    = orphans,
			imp_dep_mods = mkModDeps dependent_mods,
			imp_dep_pkgs = dependent_pkgs,
                        imp_parent   = emptyNameEnv
                   }

    -- in

	-- Complain if we import a deprecated module
    ifOptM Opt_WarnDeprecations	(
       case deprecs of	
	  DeprecAll txt -> addWarn (moduleDeprec imp_mod_name txt)
	  other	        -> returnM ()
     )

    let new_imp_decl = L loc (ImportDecl loc_imp_mod_name want_boot
                                         qual_only as_mod new_imp_details)

    returnM (new_imp_decl, gbl_env, filtered_avails, imports)

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

	; implicit_prelude <- doptM Opt_ImplicitPrelude
	; let {
	    -- Optimisation: filter out names for built-in syntax
	    -- They just clutter up the environment (esp tuples), and the parser
	    -- will generate Exact RdrNames for them, so the cluttered
	    -- envt is no use.  To avoid doing this filter all the time,
	    -- we use -fno-implicit-prelude as a clue that the filter is
	    -- worth while.  Really, it's only useful for GHC.Base and GHC.Tuple.
	    --
	    -- It's worth doing because it makes the environment smaller for
	    -- every module that imports the Prelude
	    --
	    -- Note: don't filter the gbl_env (hence all_names, not filered_all_names
	    -- in defn of gres above).      Stupid reason: when parsing 
	    -- data type decls, the constructors start as Exact tycon-names,
	    -- and then get turned into data con names by zapping the name space;
	    -- but that stops them being Exact, so they get looked up.  
	    -- Ditto in fixity decls; e.g. 	infix 5 :
	    -- Sigh. It doesn't matter because it only affects the Data.Tuple really.
	    -- The important thing is to trim down the exports.
              names = concatMap availNames avails;

	      filtered_avails
	        | implicit_prelude = avails
	        | otherwise	   = filterAvails (not.isBuiltInSyntax) avails;

	    ; this_mod = tcg_mod gbl_env
	    ; imports = emptyImportAvails {
			  imp_env = unitUFM (moduleName this_mod) 
                                        filtered_avails,
                          imp_parent = availsToNameEnv avails
		        }
	    }

	; rdr_env' <- extendRdrEnvRn (tcg_rdr_env gbl_env) names

        ; traceRn (text "local avails: " <> ppr avails)

	; returnM (gbl_env { tcg_rdr_env = rdr_env',
			     tcg_imports = imports `plusImportAvails` tcg_imports gbl_env }) 
	}

extendRdrEnvRn :: GlobalRdrEnv -> [Name] -> RnM GlobalRdrEnv
-- Add the new locally-bound names one by one, checking for duplicates as
-- we do so.  Remember that in Template Haskell the duplicates
-- might *already be* in the GlobalRdrEnv from higher up the module
extendRdrEnvRn rdr_env names
  = foldlM add_local rdr_env names
  where
    add_local rdr_env name
	| gres <- lookupGlobalRdrEnv rdr_env (nameOccName name)
	, (dup_gre:_) <- filter isLocalGRE gres	-- Check for existing *local* defns
	= do { addDupDeclErr (gre_name dup_gre) name
	     ; return rdr_env }
	| otherwise
	= return (extendGlobalRdrEnv rdr_env new_gre)
	where
	  new_gre = GRE {gre_name = name, gre_prov = LocalDef}
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
                      [AvailInfo],              -- What's imported
		      GlobalRdrEnv)		-- Same again, but in GRE form
			
filterImports iface decl_spec Nothing all_avails
  = return (Nothing, all_avails, mkGenericRdrEnv decl_spec all_avails)

filterImports iface decl_spec (Just (want_hiding, import_items)) all_avails
  = do
        -- check for errors, convert RdrNames to Names
        opt_indexedtypes <- doptM Opt_IndexedTypes
        items1 <- mapM (lookup_lie opt_indexedtypes) import_items

        let -- build the AvailInfo corresponding to each import item.
            items2 = [ (ie, filterAvailByIE (unLoc ie) av) 
                     | (ie,av) <- concat items1 ]

            -- eliminate duplicates
            avails = nubAvails (map snd items2)

            -- the new import spec, with Names instead of RdrNames            
            imp_spec_out = Just (want_hiding, map fst items2)

        case want_hiding of
          True ->
            let 
               keep n = not (n `elemNameSet` availsToNameSet avails)
               pruned_avails = filterAvails keep all_avails
            in do
            traceRn (text "pruned_avails: " <> ppr pruned_avails)
            return (imp_spec_out, pruned_avails,
                    mkGenericRdrEnv decl_spec pruned_avails)

          False ->
            let
                gres = concat [ mkGlobalRdrEltsFromIE decl_spec lie avail
                              | (lie, avail) <- items2 ]
            in do
            traceRn (text "imported avails: " <> ppr avails)
            return (imp_spec_out, avails, mkGlobalRdrEnv gres)
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
                -- warn when importing T(..) if T was exported absgtractly
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
             return [(IEVar name, avail)]

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
                  names -> return [ (IEThingAbs n, av) | (n,av) <- names ]
             | otherwise
             -> do (name,avail) <- lookup_name tc
                   return [(IEThingAbs name, avail)]

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
                     return [(IEThingWith name children, avail)]
                _otherwise -> bad_ie

         _other -> Failed illegalImportItemErr
         -- could be IEModuleContents, IEGroup, IEDoc, IEDocNamed
         -- all errors.

catMaybeErr :: [MaybeErr err a] -> [a]
catMaybeErr ms =  [ a | Succeeded a <- ms ]
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
mkGenericRdrEnv decl_spec avails
  = mkGlobalRdrEnv [ GRE { gre_name = name, gre_prov = Imported [imp_spec] }
		   | name <- concatMap availNames avails ]
  where
    imp_spec = ImpSpec { is_decl = decl_spec, is_item = ImpAll }


-- | filters an 'AvailInfo' by the given import/export spec.
filterAvailByIE :: IE Name -> AvailInfo -> AvailInfo
filterAvailByIE (IEVar n)          a@(Avail _)         = a
filterAvailByIE (IEVar n)          a@(AvailTC tc subs) = AvailTC tc [n]
filterAvailByIE (IEThingAbs n)     a@(AvailTC _ _)     = AvailTC n [n]
filterAvailByIE (IEThingAll n)     a@(AvailTC tc subs) = a
filterAvailByIE (IEThingWith n ns) a@(AvailTC tc subs) = 
        AvailTC tc (filter (`elem` (n:ns)) subs)
filterAvailByIE _ _  = panic "filterAvailByIE"

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

-- | combines 'AvailInfo's from the same family
nubAvails :: [AvailInfo] -> [AvailInfo]
nubAvails avails = nameEnvElts (foldr add emptyNameEnv avails)
 where
   add avail env = extendNameEnv_C comb_avails env (availName avail) avail
   comb_avails (AvailTC tc subs1) (AvailTC _ subs2)
                = AvailTC tc (nub (subs1 ++ subs2))
   comb_avails avail _ = avail

-- | Given an import/export spec, construct the appropriate 'GlobalRdrElt's.
mkGlobalRdrEltsFromIE :: ImpDeclSpec -> LIE Name -> AvailInfo -> [GlobalRdrElt]
mkGlobalRdrEltsFromIE decl_spec (L loc ie) avail = 
  case ie of
     IEVar name ->
        [mk_explicit_gre name]
     IEThingAbs name ->
        [mk_explicit_gre name]
     IEThingAll name | AvailTC _ subs <- avail -> 
        mk_explicit_gre name : map mk_implicit_gre subs
     IEThingWith name subs ->
        mk_explicit_gre name : map mk_explicit_gre subs
     _ ->
        panic "mkGlobalRdrEltsFromIE"
  where
        mk_explicit_gre = mk_gre True
        mk_implicit_gre = mk_gre False

	mk_gre explicit name = GRE { gre_name = name, 
				     gre_prov = Imported [imp_spec] }
	  where
	    imp_spec  = ImpSpec { is_decl = decl_spec, is_item = item_spec }
	    item_spec = ImpSome { is_explicit = explicit, is_iloc = loc }
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
     = ([LIE Name],             -- export items with Names
	ExportOccMap,		-- Tracks exported occurrence names
	[AvailInfo])	        -- The accumulated exported stuff
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
 = do TcGblEnv { tcg_mod = this_mod,
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

      (exp_spec, avails) <- exports_from_avail real_exports rdr_env 
                                imports this_mod
      return (exp_spec, nubAvails avails)
                        -- combine families

exports_from_avail :: Maybe [LIE RdrName]
                         -- Nothing => no explicit export list
                   -> GlobalRdrEnv
                   -> ImportAvails
                   -> Module
                   -> RnM (Maybe [LIE Name], [AvailInfo])

exports_from_avail Nothing rdr_env imports this_mod
 = -- the same as (module M) where M is the current module name,
   -- so that's how we handle it.
   let
       names  = [ gre_name gre | gre <- globalRdrEnvElts rdr_env,
                                 isLocalGRE gre ]
       avails = map (lookupNameEnv_NF (imp_parent imports)) names
   in
   return (Nothing, avails)

exports_from_avail (Just rdr_items) rdr_env imports this_mod
  = do traceRn (text "parent: " <> ppr (imp_parent imports))
       (ie_names, _, exports) <- foldlM do_litem emptyExportAccum rdr_items
       return (Just ie_names, exports)
  where
    do_litem :: ExportAccum -> LIE RdrName -> RnM ExportAccum
    do_litem acc lie = setSrcSpan (getLoc lie) (exports_from_item acc lie)

    exports_from_item :: ExportAccum -> LIE RdrName -> RnM ExportAccum
    exports_from_item acc@(ie_names, occs, exports) 
                        (L loc ie@(IEModuleContents mod))
	| mod `elem` mods 	-- Duplicate export of M
	= do { warn_dup_exports <- doptM Opt_WarnDuplicateExports ;
	       warnIf warn_dup_exports (dupModuleExport mod) ;
	       returnM acc }

	| otherwise
	= case lookupUFM (imp_env imports) mod of
            Nothing -> do addErr (modExportErr mod)
                          return acc
            Just avails
                -> do traceRn (text "mod avails: " <> ppr mod <+> ppr avails)
                      let avails'  = filterAvails (inScopeUnqual rdr_env) $
                                        nubAvails avails
                          new_exps = concatMap availNames avails'

                      occs' <- check_occs ie occs new_exps
                      -- This check_occs not only finds conflicts
                      -- between this item and others, but also
                      -- internally within this item.  That is, if
                      -- 'M.x' is in scope in several ways, we'll have
                      -- several members of mod_avails with the same
                      -- OccName.
                      return (L loc (IEModuleContents mod) : ie_names,
                              occs', avails' ++ exports)
        where
           mods = [ mod | (L _ (IEModuleContents mod)) <- ie_names ]

    exports_from_item acc@(lie_names, occs, exports) (L loc ie)
        = do new_ie <- lookup_ie ie
             let ie_name = ieName new_ie
             if isUnboundName ie_name
                  then return acc 	-- Avoid error cascade
                  else do
             if isDoc new_ie           -- deal with docs
                  then return (L loc new_ie : lie_names, occs, exports)
                  else do
             traceRn (text "lookup_avail: " <> ppr (lookup_avail ie_name))
             let avail = filterAvailByIE new_ie (lookup_avail ie_name)
                 new_exports = case new_ie of
                                     IEThingWith n ns -> n : ns
                                     _ -> availNames avail
                          -- ^^^ an IEThingWith might contain duplicates
                          -- whereas the avail doesn't, but we want
                          -- duplicates to be noticed by check_occs below.
             -- checkErr (not (null (drop 1 new_exports))) (exportItemErr ie)
             checkForDodgyExport new_ie new_exports
             occs' <- check_occs ie occs new_exports
             return (L loc new_ie : lie_names, occs', avail : exports)
	  
    lookup_avail :: Name -> AvailInfo
    lookup_avail name = 
        case lookupNameEnv avail_env name of
             Nothing -> pprPanic "rnExports:lookup_avail" (ppr name)
             Just a  -> a
        where avail_env = imp_parent imports

    lookup_ie :: IE RdrName -> RnM (IE Name)

    lookup_ie (IEVar rdr) 
        = do name <- lookupGlobalOccRn rdr
             return (IEVar name)

    lookup_ie (IEThingAbs rdr) 
        = do name <- lookupGlobalOccRn rdr
             return (IEThingAbs name)

    lookup_ie (IEThingAll rdr) 
        = do name <- lookupGlobalOccRn rdr
             return (IEThingAll name)

    lookup_ie ie@(IEThingWith rdr sub_rdrs)
        = do name <- lookupGlobalOccRn rdr
             if isUnboundName name
                then return (IEThingWith name [])
                else do
             let avail = lookup_avail name
                 env = mkOccEnv [ (nameOccName s, s) 
                                | AvailTC _ subnames <- [avail],
                                  s <- subnames ]
             let mb_names = map (lookupOccEnv env . rdrNameOcc) sub_rdrs
             if any isNothing mb_names
                then do addErr (exportItemErr ie)
                        return (IEThingWith name [])
                else do let names = catMaybes mb_names
                        optIdxTypes <- doptM Opt_IndexedTypes
                        when (not optIdxTypes && any isTyConName names) $
                          addErr (typeItemErr ( head
                                              . filter isTyConName 
                                              $ names )
                                              (text "in export list"))
                        return (IEThingWith name (catMaybes mb_names))

    lookup_ie (IEGroup lev doc) 
        = do rn_doc <- rnHsDoc doc
	     return (IEGroup lev rn_doc)
    lookup_ie (IEDoc doc)
        = do rn_doc <- rnHsDoc doc
             return (IEDoc rn_doc)
    lookup_ie (IEDocNamed str)
        = return (IEDocNamed str)

    lookup_ie (IEModuleContents _)
        = panic "rnExports:lookup_ie" -- caught earlier


isDoc (IEDoc _)      = True
isDoc (IEDocNamed _) = True
isDoc (IEGroup _ _)  = True
isDoc _ = False

-------------------------------
inScopeUnqual :: GlobalRdrEnv -> Name -> Bool
-- Checks whether the Name is in scope unqualified, 
-- regardless of whether it's ambiguous or not
inScopeUnqual env n = any unQualOK (lookupGRE_Name env n)

-------------------------------
checkForDodgyExport :: IE Name -> [Name] -> RnM ()
checkForDodgyExport ie@(IEThingAll tc) [n] 
  | isTcOcc (nameOccName n) = addWarn (dodgyExportWarn tc)
	-- This occurs when you export T(..), but
	-- only import T abstractly, or T is a synonym.  
	-- The single [n] is the type or class itself
  | otherwise = addErr (exportItemErr ie)
	-- This happes if you export x(..), which is bogus
checkForDodgyExport _ _ = return ()

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

    avail_env = imp_parent (tcg_imports tcg_env)

    check hpt pit (GRE {gre_name = name, gre_prov = Imported (imp_spec:_)})
      | name `elemNameSet` used_names
      ,	Just deprec_txt <- lookupDeprec dflags hpt pit avail_env name
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
             -> NameEnv AvailInfo       -- parent info
	     -> Name -> Maybe DeprecTxt
lookupDeprec dflags hpt pit avail_env n 
  = case lookupIfaceByModule dflags hpt pit (nameModule n) of
	Just iface -> mi_dep_fn iface n `seqMaybe` 	-- Bleat if the thing, *or
		      mi_dep_fn iface (nameParent n)	-- its parent*, is deprec'd
	Nothing    
	  | isWiredInName n -> Nothing
		-- We have not necessarily loaded the .hi file for a 
		-- wired-in name (yet), although we *could*.
		-- And we never deprecate them

	 | otherwise -> pprPanic "lookupDeprec" (ppr n)	
		-- By now all the interfaces should have been loaded
  where
        nameParent n = case lookupNameEnv avail_env n of
                         Just (AvailTC parent _) -> parent
                         _ -> n

gre_is_used :: NameSet -> GlobalRdrElt -> Bool
gre_is_used used_names gre = gre_name gre `elemNameSet` used_names
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
    used_names, all_used_names :: NameSet
    used_names = findUses (tcg_dus gbl_env) emptyNameSet
	-- NB: currently, if f x = g, we only treat 'g' as used if 'f' is used
	-- Hence findUses

    avail_env = imp_parent (tcg_imports gbl_env)
    nameParent_maybe n = case lookupNameEnv avail_env n of
                            Just (AvailTC tc _) | tc /= n  ->  Just tc
                            _otherwise  -> Nothing

    all_used_names = used_names `unionNameSets` 
		     mkNameSet (mapCatMaybes nameParent_maybe (nameSetToList used_names))
			-- A use of C implies a use of T,
			-- if C was brought into scope by T(..) or T(C)

	-- Collect the defined names from the in-scope environment
    defined_names :: [GlobalRdrElt]
    defined_names = globalRdrEnvElts (tcg_rdr_env gbl_env)

	-- Note that defined_and_used, defined_but_not_used
	-- are both [GRE]; that's why we need defined_and_used
	-- rather than just all_used_names
    defined_and_used, defined_but_not_used :: [GlobalRdrElt]
    (defined_and_used, defined_but_not_used) 
	= partition (gre_is_used all_used_names) defined_names
    
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
    add_name (GRE {gre_name = n, gre_prov = Imported imp_specs}) acc 
	= addToFM_C plusAvailEnv acc (importSpecModule (head imp_specs))
		    (unitAvailEnv (mk_avail n (nameParent_maybe n)))
    add_name other acc 
	= acc

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

	-- n is the name of the thing, p is the name of its parent
    mk_avail n (Just p)			 	 = AvailTC p [p,n]
    mk_avail n Nothing | isTcOcc (nameOccName n) = AvailTC n [n]
		       | otherwise		 = Avail n
    
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
	  
modExportErr mod
  = hsep [ ptext SLIT("Unknown module in export list: module"), quotes (ppr mod)]

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

moduleDeprec mod txt
  = sep [ ptext SLIT("Module") <+> quotes (ppr mod) <+> ptext SLIT("is deprecated:"), 
	  nest 4 (ppr txt) ]	  
\end{code}
