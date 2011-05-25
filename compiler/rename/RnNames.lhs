%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
module RnNames (
        rnImports, getLocalNonValBinders,
        rnExports, extendGlobalRdrEnvRn,
        gresFromAvails,
        reportUnusedNames, finishWarnings,
    ) where

#include "HsVersions.h"

import DynFlags
import HsSyn
import TcEnv            ( isBrackStage )
import RnEnv
import RnHsDoc          ( rnHsDoc )
import IfaceEnv		( ifaceExportNames )
import LoadIface        ( loadSrcInterface )
import TcRnMonad

import HeaderInfo       ( mkPrelImports )
import PrelNames
import Module
import Name
import NameEnv
import NameSet
import HscTypes
import RdrName
import Outputable
import Maybes
import SrcLoc
import ErrUtils
import Util
import FastString
import ListSetOps
import Data.List        ( partition, (\\), delete, find )
import qualified Data.Set as Set
import System.IO
import Control.Monad
import Data.Map         ( Map )
import qualified Data.Map as Map
\end{code}



%************************************************************************
%*                                                                      *
\subsection{rnImports}
%*                                                                      *
%************************************************************************

\begin{code}
rnImports :: [LImportDecl RdrName]
           -> RnM ([LImportDecl Name], GlobalRdrEnv, ImportAvails,AnyHpcUsage)

rnImports imports
         -- PROCESS IMPORT DECLS
         -- Do the non {- SOURCE -} ones first, so that we get a helpful
         -- warning for {- SOURCE -} ones that are unnecessary
    = do this_mod <- getModule
         implicit_prelude <- xoptM Opt_ImplicitPrelude
         let prel_imports       = mkPrelImports (moduleName this_mod) implicit_prelude imports
             (source, ordinary) = partition is_source_import imports
             is_source_import (L _ (ImportDecl _ _ is_boot _ _ _)) = is_boot

         ifDOptM Opt_WarnImplicitPrelude (
            when (notNull prel_imports) $ addWarn (implicitPreludeWarn)
          )

         stuff1 <- mapM (rnImportDecl this_mod True)  prel_imports
         stuff2 <- mapM (rnImportDecl this_mod False) ordinary
         stuff3 <- mapM (rnImportDecl this_mod False) source
         let (decls, rdr_env, imp_avails, hpc_usage) = combine (stuff1 ++ stuff2 ++ stuff3)
         return (decls, rdr_env, imp_avails, hpc_usage)

    where
   combine :: [(LImportDecl Name,  GlobalRdrEnv, ImportAvails,AnyHpcUsage)]
           -> ([LImportDecl Name], GlobalRdrEnv, ImportAvails,AnyHpcUsage)
   combine = foldr plus ([], emptyGlobalRdrEnv, emptyImportAvails,False)
        where plus (decl,  gbl_env1, imp_avails1,hpc_usage1)
                   (decls, gbl_env2, imp_avails2,hpc_usage2)
                = (decl:decls,
                   gbl_env1 `plusGlobalRdrEnv` gbl_env2,
                   imp_avails1 `plusImportAvails` imp_avails2,
                   hpc_usage1 || hpc_usage2)

rnImportDecl  :: Module -> Bool
              -> LImportDecl RdrName
              -> RnM (LImportDecl Name, GlobalRdrEnv, ImportAvails,AnyHpcUsage)

rnImportDecl this_mod implicit_prelude
             (L loc (ImportDecl { ideclName = loc_imp_mod_name, ideclPkgQual = mb_pkg
                                , ideclSource = want_boot, ideclQualified = qual_only
                                , ideclAs = as_mod, ideclHiding = imp_details }))
  = setSrcSpan loc $ do

    when (isJust mb_pkg) $ do
        pkg_imports <- xoptM Opt_PackageImports
        when (not pkg_imports) $ addErr packageImportErr

        -- If there's an error in loadInterface, (e.g. interface
        -- file not found) we get lots of spurious errors from 'filterImports'
    let
        imp_mod_name = unLoc loc_imp_mod_name
        doc = ppr imp_mod_name <+> ptext (sLit "is directly imported")

	-- Check for a missing import list
	-- (Opt_WarnMissingImportList also checks for T(..) items
	--  but that is done in checkDodgyImport below)
    case imp_details of
        Just (False, _)       -> return ()	-- Explicit import list
        _  | implicit_prelude -> return ()
           | qual_only	      -> return ()
           | otherwise        -> ifDOptM Opt_WarnMissingImportList $
                                 addWarn (missingImportListWarn imp_mod_name)

    iface <- loadSrcInterface doc imp_mod_name want_boot mb_pkg

        -- Compiler sanity check: if the import didn't say
        -- {-# SOURCE #-} we should not get a hi-boot file
    WARN( dflags, not want_boot && mi_boot iface, ppr imp_mod_name ) (do

        -- Issue a user warning for a redundant {- SOURCE -} import
        -- NB that we arrange to read all the ordinary imports before
        -- any of the {- SOURCE -} imports.
        --
        -- in --make and GHCi, the compilation manager checks for this,
        -- and indeed we shouldn't do it here because the existence of
        -- the non-boot module depends on the compilation order, which
        -- is not deterministic.  The hs-boot test can show this up.
    dflags <- getDOpts
    warnIf (want_boot && not (mi_boot iface) && isOneShot (ghcMode dflags))
           (warnRedundantSourceImport imp_mod_name)

    let
        imp_mod    = mi_module iface
        warns      = mi_warns iface
        orph_iface = mi_orphan iface
        has_finsts = mi_finsts iface
        deps       = mi_deps iface

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
        --      module A where
        --         import B( AType )
        --         type AType = ...
        --
        --      module B( AType ) where
        --         import {-# SOURCE #-} A( AType )
        --
        -- then you'll get a 'B does not export AType' message.  Oh well.

        qual_mod_name = case as_mod of
                          Nothing           -> imp_mod_name
                          Just another_name -> another_name
        imp_spec  = ImpDeclSpec { is_mod = imp_mod_name, is_qual = qual_only,
                                  is_dloc = loc, is_as = qual_mod_name }

    -- Get the total exports from this module
    total_avails <- ifaceExportNames filtered_exports

    -- filter the imports according to the import declaration
    (new_imp_details, gbl_env) <-
        filterImports iface imp_spec imp_details total_avails

    dflags <- getDOpts

    let
        -- Compute new transitive dependencies

        orphans | orph_iface = ASSERT( not (imp_mod `elem` dep_orphs deps) )
                               imp_mod : dep_orphs deps
                | otherwise  = dep_orphs deps

        finsts | has_finsts = ASSERT( not (imp_mod `elem` dep_finsts deps) )
                              imp_mod : dep_finsts deps
               | otherwise  = dep_finsts deps

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
                        _                    -> False

        imports   = ImportAvails {
                        imp_mods     = unitModuleEnv imp_mod [(qual_mod_name, import_all, loc)],
                        imp_orphs    = orphans,
                        imp_finsts   = finsts,
                        imp_dep_mods = mkModDeps dependent_mods,
                        imp_dep_pkgs = dependent_pkgs
                   }

    -- Complain if we import a deprecated module
    ifDOptM Opt_WarnWarningsDeprecations        (
       case warns of
          WarnAll txt -> addWarn (moduleWarn imp_mod_name txt)
          _           -> return ()
     )

    let new_imp_decl = L loc (ImportDecl loc_imp_mod_name mb_pkg want_boot
                                         qual_only as_mod new_imp_details)

    return (new_imp_decl, gbl_env, imports, mi_hpc iface)
    )

warnRedundantSourceImport :: ModuleName -> SDoc
warnRedundantSourceImport mod_name
  = ptext (sLit "Unnecessary {-# SOURCE #-} in the import of module")
          <+> quotes (ppr mod_name)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{importsFromLocalDecls}
%*                                                                      *
%************************************************************************

From the top-level declarations of this module produce
        * the lexical environment
        * the ImportAvails
created by its bindings.

Note [Top-level Names in Template Haskell decl quotes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a Template Haskell declaration quotation like this:
      module M where
        f x = h [d| f = 3 |]
When renaming the declarations inside [d| ...|], we treat the
top level binders specially in two ways

1.  We give them an Internal name, not (as usual) an External one.
    Otherwise the NameCache gets confused by a second allocation of
    M.f.  (We used to invent a fake module ThFake to avoid this, but
    that had other problems, notably in getting the correct answer for
    nameIsLocalOrFrom in lookupFixity. So we now leave tcg_module
    unaffected.)

2.  We make them *shadow* the outer bindings. If we don't do that,
    we'll get a complaint when extending the GlobalRdrEnv, saying that
    there are two bindings for 'f'.  There are several tricky points:

    * This shadowing applies even if the binding for 'f' is in a
      where-clause, and hence is in the *local* RdrEnv not the *global*
      RdrEnv.

    * The *qualified* name M.f from the enclosing module must certainly
      still be available.  So we don't nuke it entirely; we just make
      it seem like qualified import.

    * We only shadow *External* names (which come from the main module)
      Do not shadow *Inernal* names because in the bracket
          [d| class C a where f :: a
              f = 4 |]
      rnSrcDecls will first call extendGlobalRdrEnvRn with C[f] from the
      class decl, and *separately* extend the envt with the value binding.

3. We find out whether we are inside a [d| ... |] by testing the TH
   stage. This is a slight hack, because the stage field was really
   meant for the type checker, and here we are not interested in the
   fields of Brack, hence the error thunks in thRnBrack.

\begin{code}
extendGlobalRdrEnvRn :: [AvailInfo]
                     -> MiniFixityEnv
                     -> RnM (TcGblEnv, TcLclEnv)
-- Updates both the GlobalRdrEnv and the FixityEnv
-- We return a new TcLclEnv only because we might have to
-- delete some bindings from it;
-- see Note [Top-level Names in Template Haskell decl quotes]

extendGlobalRdrEnvRn avails new_fixities
  = do  { (gbl_env, lcl_env) <- getEnvs
        ; stage <- getStage
        ; let rdr_env = tcg_rdr_env gbl_env
              fix_env = tcg_fix_env gbl_env

              -- Delete new_occs from global and local envs
              -- If we are in a TemplateHaskell decl bracket,
              --    we are going to shadow them
              -- See Note [Top-level Names in Template Haskell decl quotes]
              shadowP  = isBrackStage stage
              new_occs = map (nameOccName . gre_name) gres
              rdr_env1 = transformGREs qual_gre new_occs rdr_env
              lcl_env1 = lcl_env { tcl_rdr = delListFromOccEnv (tcl_rdr lcl_env) new_occs }
              (rdr_env2, lcl_env2) | shadowP   = (rdr_env1, lcl_env1)
                                   | otherwise = (rdr_env,  lcl_env)

              rdr_env3 = foldl extendGlobalRdrEnv rdr_env2 gres
              fix_env' = foldl extend_fix_env     fix_env  gres
              (rdr_env', dups) = findLocalDupsRdrEnv rdr_env3 new_occs

              gbl_env' = gbl_env { tcg_rdr_env = rdr_env', tcg_fix_env = fix_env' }

        ; mapM_ addDupDeclErr dups

        ; traceRn (text "extendGlobalRdrEnvRn" <+> (ppr new_fixities $$ ppr fix_env $$ ppr fix_env'))
        ; return (gbl_env', lcl_env2) }
  where
    gres = gresFromAvails LocalDef avails

    -- If there is a fixity decl for the gre, add it to the fixity env
    extend_fix_env fix_env gre
      | Just (L _ fi) <- lookupFsEnv new_fixities (occNameFS occ)
      = extendNameEnv fix_env name (FixItem occ fi)
      | otherwise
      = fix_env
      where
        name = gre_name gre
        occ  = nameOccName name

    qual_gre :: GlobalRdrElt -> GlobalRdrElt
    -- Transform top-level GREs from the module being compiled
    -- so that they are out of the way of new definitions in a Template
    -- Haskell bracket
    -- See Note [Top-level Names in Template Haskell decl quotes]
    -- Seems like 5 times as much work as it deserves!
    --
    -- For a LocalDef we make a (fake) qualified imported GRE for a
    -- local GRE so that the original *qualified* name is still in scope
    -- but the *unqualified* one no longer is.  What a hack!

    qual_gre gre@(GRE { gre_prov = LocalDef, gre_name = name })
        | isExternalName name = gre { gre_prov = Imported [imp_spec] }
        | otherwise           = gre
          -- Do not shadow Internal (ie Template Haskell) Names
          -- See Note [Top-level Names in Template Haskell decl quotes]
        where
          mod = ASSERT2( isExternalName name, ppr name) moduleName (nameModule name)
          imp_spec = ImpSpec { is_item = ImpAll, is_decl = decl_spec }
          decl_spec = ImpDeclSpec { is_mod = mod, is_as = mod,
                                    is_qual = True,  -- Qualified only!
                                    is_dloc = srcLocSpan (nameSrcLoc name) }

    qual_gre gre@(GRE { gre_prov = Imported specs })
        = gre { gre_prov = Imported (map qual_spec specs) }

    qual_spec spec@(ImpSpec { is_decl = decl_spec })
        = spec { is_decl = decl_spec { is_qual = True } }
\end{code}

@getLocalDeclBinders@ returns the names for an @HsDecl@.  It's
used for source code.

        *** See "THE NAMING STORY" in HsDecls ****

Instances of type families
~~~~~~~~~~~~~~~~~~~~~~~~~~
Family instances contain data constructors that we need to collect and we also
need to descend into the type instances of associated families in class
instances. The type constructor of a family instance is a usage occurence.
Hence, we don't return it as a subname in 'AvailInfo'; otherwise, we would get
a duplicate declaration error.

Note [Looking up family names in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  module M where
    type family T a :: *
    type instance M.T Int = Bool

We might think that we can simply use 'lookupOccRn' when processing the type
instance to look up 'M.T'.  Alas, we can't!  The type family declaration is in
the *same* HsGroup as the type instance declaration.  Hence, as we are
currently collecting the binders declared in that HsGroup, these binders will
not have been added to the global environment yet.

In the case of type classes, this problem does not arise, as a class instance
does not define any binders of it's own.  So, we simply don't attempt to look
up the class names of class instances in 'get_local_binders' below.

If we don't look up class instances, can't we get away without looking up type
instances, too?  No, we can't.  Data type instances define data constructors
and we need to

  (1) collect those in 'get_local_binders' and
  (2) we need to get their parent name in 'get_local_binders', too, to
      produce an appropriate 'AvailTC'.

This parent name is exactly the family name of the type instance that is so
difficult to look up.

We solve this problem as follows:

  (a) We process all type declarations other than type instances first.
  (b) Then, we compute a 'GlobalRdrEnv' from the result of the first step.
  (c) Finally, we process all type instances (both those on the toplevel and
      those nested in class instances) and check for the family names in the
      'GlobalRdrEnv' produced in the previous step before using 'lookupOccRn'.

\begin{code}
getLocalNonValBinders :: HsGroup RdrName -> RnM [AvailInfo]
-- Get all the top-level binders bound the group *except*
-- for value bindings, which are treated separately
-- Specificaly we return AvailInfo for
--      type decls (incl constructors and record selectors)
--      class decls (including class ops)
--      associated types
--      foreign imports
--      (in hs-boot files) value signatures

getLocalNonValBinders group
  = do { gbl_env <- getGblEnv
       ; get_local_binders gbl_env group }

get_local_binders :: TcGblEnv -> HsGroup RdrName -> RnM [GenAvailInfo Name]
get_local_binders gbl_env (HsGroup {hs_valds  = ValBindsIn _ val_sigs,
                                    hs_tyclds = tycl_decls,
                                    hs_instds = inst_decls,
                                    hs_fords  = foreign_decls })
  = do  { -- separate out the family instance declarations
          let (tyinst_decls1, tycl_decls_noinsts)
                           = partition (isFamInstDecl . unLoc) (concat tycl_decls)
              tyinst_decls = tyinst_decls1 ++ instDeclATs inst_decls

          -- process all type/class decls except family instances
        ; tc_names  <- mapM new_tc tycl_decls_noinsts

          -- create a temporary rdr env of the type binders
        ; let tc_gres     = gresFromAvails LocalDef tc_names
              tc_name_env = foldl extendGlobalRdrEnv emptyGlobalRdrEnv tc_gres

          -- process all family instances
        ; ti_names  <- mapM (new_ti tc_name_env) tyinst_decls

          -- finish off with value binder in case of a hs-boot file
        ; val_names <- mapM new_simple val_bndrs
        ; return (val_names ++ tc_names ++ ti_names) }
  where
    is_hs_boot = isHsBoot (tcg_src gbl_env) ;

    for_hs_bndrs :: [Located RdrName]
    for_hs_bndrs = [nm | L _ (ForeignImport nm _ _) <- foreign_decls]

    -- In a hs-boot file, the value binders come from the
    --  *signatures*, and there should be no foreign binders
    val_bndrs :: [Located RdrName]
    val_bndrs | is_hs_boot = [nm | L _ (TypeSig nm _) <- val_sigs]
              | otherwise  = for_hs_bndrs

    new_simple :: Located RdrName -> RnM (GenAvailInfo Name)
    new_simple rdr_name = do
        nm <- newTopSrcBinder rdr_name
        return (Avail nm)

    new_tc tc_decl              -- NOT for type/data instances
        = do { main_name <- newTopSrcBinder main_rdr
             ; sub_names <- mapM newTopSrcBinder sub_rdrs
             ; return (AvailTC main_name (main_name : sub_names)) }
      where
        (main_rdr : sub_rdrs) = hsTyClDeclBinders tc_decl

    new_ti tc_name_env ti_decl  -- ONLY for type/data instances
        = do { main_name <- lookupFamInstDeclBndr tc_name_env main_rdr
             ; sub_names <- mapM newTopSrcBinder sub_rdrs
             ; return (AvailTC main_name sub_names) }
                        -- main_name is not bound here!
      where
        (main_rdr : sub_rdrs) = hsTyClDeclBinders ti_decl

get_local_binders _ g = pprPanic "get_local_binders" (ppr g)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Filtering imports}
%*                                                                      *
%************************************************************************

@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

\begin{code}
filterImports :: ModIface
              -> ImpDeclSpec                    -- The span for the entire import decl
              -> Maybe (Bool, [LIE RdrName])    -- Import spec; True => hiding
              -> [AvailInfo]                    -- What's available
              -> RnM (Maybe (Bool, [LIE Name]), -- Import spec w/ Names
                      GlobalRdrEnv)             -- Same again, but in GRE form
filterImports _ decl_spec Nothing all_avails
  = return (Nothing, mkGlobalRdrEnv (gresFromAvails prov all_avails))
  where
    prov = Imported [ImpSpec { is_decl = decl_spec, is_item = ImpAll }]


filterImports iface decl_spec (Just (want_hiding, import_items)) all_avails
  = do  -- check for errors, convert RdrNames to Names
        opt_typeFamilies <- xoptM Opt_TypeFamilies
        items1 <- mapM (lookup_lie opt_typeFamilies) import_items

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
    occ_env :: OccEnv (Name,        -- the name
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
        combine x y = pprPanic "filterImports/combine" (ppr x $$ ppr y)

    lookup_lie :: Bool -> LIE RdrName -> TcRn [(LIE Name, AvailInfo)]
    lookup_lie opt_typeFamilies (L loc ieRdr)
        = do
             stuff <- setSrcSpan loc $
                      case lookup_ie opt_typeFamilies ieRdr of
                            Failed err  -> addErr err >> return []
                            Succeeded a -> return a
             checkDodgyImport stuff
             return [ (L loc ie, avail) | (ie,avail) <- stuff ]
        where
            -- Warn when importing T(..) if T was exported abstractly
            checkDodgyImport stuff
                | IEThingAll n <- ieRdr, (_, AvailTC _ [_]):_ <- stuff
                = ifDOptM Opt_WarnDodgyImports (addWarn (dodgyImportWarn n))
                -- NB. use the RdrName for reporting the warning
		| IEThingAll {} <- ieRdr
		, not (is_qual decl_spec)
                = ifDOptM Opt_WarnMissingImportList $
                  addWarn (missingImportListItem ieRdr)
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
    lookup_ie opt_typeFamilies ie
      = let bad_ie :: MaybeErr Message a
            bad_ie = Failed (badImportItemErr iface decl_spec ie all_avails)

            lookup_name rdr
              | isQual rdr = Failed (qualImportItemErr rdr)
              | Just nm <- lookupOccEnv occ_env (rdrNameOcc rdr) = return nm
              | otherwise                                        = bad_ie
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
            (name, AvailTC _ subnames, mb_parent) <- lookup_name tc
            let
              env         = mkOccEnv [(nameOccName s, s) | s <- subnames]
              mb_children = map (lookupOccEnv env . rdrNameOcc) ns
            children <- if any isNothing mb_children
                        then bad_ie
                        else return (catMaybes mb_children)
            -- check for proper import of type families
            when (not opt_typeFamilies && any isTyConName children) $
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
        mkIEThingAbs (n, _,  Just parent) = (IEThingAbs n, AvailTC parent [n])


catMaybeErr :: [MaybeErr err a] -> [a]
catMaybeErr ms =  [ a | Succeeded a <- ms ]
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Import/Export Utils}
%*                                                                      *
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
mkUnitAvail me (ParentIs p)              = AvailTC p  [me]
mkUnitAvail me NoParent | isTyConName me = AvailTC me [me]
                        | otherwise      = Avail me

plusAvail :: GenAvailInfo Name -> GenAvailInfo Name -> GenAvailInfo Name
plusAvail (Avail n1)      (Avail _)        = Avail n1
plusAvail (AvailTC _ ns1) (AvailTC n2 ns2) = AvailTC n2 (ns1 `unionLists` ns2)
plusAvail a1 a2 = pprPanic "RnEnv.plusAvail" (hsep [ppr a1,ppr a2])

availParent :: Name -> AvailInfo -> Parent
availParent _ (Avail _)                 = NoParent
availParent n (AvailTC m _) | n == m    = NoParent
                            | otherwise = ParentIs m

trimAvail :: AvailInfo -> Name -> AvailInfo
trimAvail (Avail n)      _ = Avail n
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

-- | Given an import\/export spec, construct the appropriate 'GlobalRdrElt's.
gresFromIE :: ImpDeclSpec -> (LIE Name, AvailInfo) -> [GlobalRdrElt]
gresFromIE decl_spec (L loc ie, avail)
  = gresFromAvail prov_fn avail
  where
    is_explicit = case ie of
                    IEThingAll name -> \n -> n == name
                    _               -> \_ -> True
    prov_fn name = Imported [imp_spec]
        where
          imp_spec  = ImpSpec { is_decl = decl_spec, is_item = item_spec }
          item_spec = ImpSome { is_explicit = is_explicit name, is_iloc = loc }

mkChildEnv :: [GlobalRdrElt] -> NameEnv [Name]
mkChildEnv gres = foldr add emptyNameEnv gres
    where
        add (GRE { gre_name = n, gre_par = ParentIs p }) env = extendNameEnv_Acc (:) singleton env p n
        add _                                            env = env

findChildren :: NameEnv [Name] -> Name -> [Name]
findChildren env n = lookupNameEnv env n `orElse` []
\end{code}

---------------------------------------
        AvailEnv and friends

All this AvailEnv stuff is hardly used; only in a very small
part of RnNames.  Todo: remove?
---------------------------------------

\begin{code}
type AvailEnv = NameEnv AvailInfo       -- Maps a Name to the AvailInfo that contains it

emptyAvailEnv :: AvailEnv
emptyAvailEnv = emptyNameEnv

{- Dead code
unitAvailEnv :: AvailInfo -> AvailEnv
unitAvailEnv a = unitNameEnv (availName a) a

plusAvailEnv :: AvailEnv -> AvailEnv -> AvailEnv
plusAvailEnv = plusNameEnv_C plusAvail

availEnvElts :: AvailEnv -> [AvailInfo]
availEnvElts = nameEnvElts
-}

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
%*                                                                      *
\subsection{Export list processing}
%*                                                                      *
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
type ExportAccum        -- The type of the accumulating parameter of
                        -- the main worker function in rnExports
     = ([LIE Name],             -- Export items with Names
        ExportOccMap,                -- Tracks exported occurrence names
        [AvailInfo])            -- The accumulated exported stuff
                                --   Not nub'd!

emptyExportAccum :: ExportAccum
emptyExportAccum = ([], emptyOccEnv, [])

type ExportOccMap = OccEnv (Name, IE RdrName)
        -- Tracks what a particular exported OccName
        --   in an export list refers to, and which item
        --   it came from.  It's illegal to export two distinct things
        --   that have the same occurrence name

rnExports :: Bool       -- False => no 'module M(..) where' header at all
          -> Maybe [LIE RdrName]        -- Nothing => no explicit export list
          -> TcGblEnv
          -> RnM TcGblEnv

        -- Complains if two distinct exports have same OccName
        -- Warns about identical exports.
        -- Complains about exports items not in scope

rnExports explicit_mod exports
          tcg_env@(TcGblEnv { tcg_mod     = this_mod,
                              tcg_rdr_env = rdr_env,
                              tcg_imports = imports })
 = do   {
        -- If the module header is omitted altogether, then behave
        -- as if the user had written "module Main(main) where..."
        -- EXCEPT in interactive mode, when we behave as if he had
        -- written "module Main where ..."
        -- Reason: don't want to complain about 'main' not in scope
        --         in interactive mode
        ; dflags <- getDOpts
        ; let real_exports
                 | explicit_mod = exports
                 | ghcLink dflags == LinkInMemory = Nothing
                 | otherwise = Just ([noLoc (IEVar main_RDR_Unqual)])
                        -- ToDo: the 'noLoc' here is unhelpful if 'main'
                        --       turns out to be out of scope

        ; (rn_exports, avails) <- exports_from_avail real_exports rdr_env imports this_mod
        ; let final_avails = nubAvails avails    -- Combine families

        ; return (tcg_env { tcg_exports    = final_avails,
                            tcg_rn_exports = case tcg_rn_exports tcg_env of
                                                Nothing -> Nothing
                                                Just _  -> rn_exports,
                            tcg_dus = tcg_dus tcg_env `plusDU`
                                      usesOnly (availsToNameSet final_avails) }) }

exports_from_avail :: Maybe [LIE RdrName]
                         -- Nothing => no explicit export list
                   -> GlobalRdrEnv
                   -> ImportAvails
                   -> Module
                   -> RnM (Maybe [LIE Name], [AvailInfo])

exports_from_avail Nothing rdr_env _imports _this_mod
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

    kids_env :: NameEnv [Name]  -- Maps a parent to its in-scope children
    kids_env = mkChildEnv (globalRdrEnvElts rdr_env)

    imported_modules = [ qual_name
                       | xs <- moduleEnvElts $ imp_mods imports,
                         (qual_name, _, _) <- xs ]

    exports_from_item :: ExportAccum -> LIE RdrName -> RnM ExportAccum
    exports_from_item acc@(ie_names, occs, exports)
                      (L loc ie@(IEModuleContents mod))
        | let earlier_mods = [ mod | (L _ (IEModuleContents mod)) <- ie_names ]
        , mod `elem` earlier_mods    -- Duplicate export of M
        = do { warn_dup_exports <- doptM Opt_WarnDuplicateExports ;
               warnIf warn_dup_exports (dupModuleExport mod) ;
               return acc }

        | otherwise
        = do { implicit_prelude <- xoptM Opt_ImplicitPrelude
             ; warnDodgyExports <- doptM Opt_WarnDodgyExports
             ; let { exportValid = (mod `elem` imported_modules)
                            || (moduleName this_mod == mod)
                   ; gres = filter (isModuleExported implicit_prelude mod)
                                   (globalRdrEnvElts rdr_env)
                   ; names = map gre_name gres
                   }

             ; checkErr exportValid (moduleNotImported mod)
             ; warnIf (warnDodgyExports && exportValid && null gres) (nullModuleExport mod)

             ; addUsedRdrNames (concat [ [mkRdrQual mod occ, mkRdrUnqual occ]
                                       | occ <- map nameOccName names ])
                        -- The qualified and unqualified version of all of
                        -- these names are, in effect, used by this export

             ; occs' <- check_occs ie occs names
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
                  then return acc    -- Avoid error cascade
                  else do

             occs' <- check_occs ie occs (availNames avail)

             return (L loc new_ie : lie_names, occs', avail : exports)

    -------------
    lookup_ie :: IE RdrName -> RnM (IE Name, AvailInfo)
    lookup_ie (IEVar rdr)
        = do gre <- lookupGreRn rdr
             return (IEVar (gre_name gre), greAvail gre)

    lookup_ie (IEThingAbs rdr)
        = do gre <- lookupGreRn rdr
             let name = gre_name gre
             case gre_par gre of
                NoParent   -> return (IEThingAbs name,
                                      AvailTC name [name])
                ParentIs p -> return (IEThingAbs name,
                                      AvailTC p [name])

    lookup_ie ie@(IEThingAll rdr)
        = do name <- lookupGlobalOccRn rdr
             let kids = findChildren kids_env name
                 mkKidRdrName = case isQual_maybe rdr of
                                Nothing -> mkRdrUnqual
                                Just (modName, _) -> mkRdrQual modName
             addUsedRdrNames $ map (mkKidRdrName . nameOccName) kids
             warnDodgyExports <- doptM Opt_WarnDodgyExports
             when (null kids) $
                  if isTyConName name
                  then when warnDodgyExports $ addWarn (dodgyExportWarn name)
                  else -- This occurs when you export T(..), but
                       -- only import T abstractly, or T is a synonym.
                       addErr (exportItemErr ie)

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
                        optTyFam <- xoptM Opt_TypeFamilies
                        when (not optTyFam && any isTyConName names) $
                          addErr (typeItemErr ( head
                                              . filter isTyConName
                                              $ names )
                                              (text "in export list"))
                        return (IEThingWith name names, AvailTC name (name:names))

    lookup_ie _ = panic "lookup_ie"    -- Other cases covered earlier

    -------------
    lookup_doc_ie :: IE RdrName -> RnM (IE Name)
    lookup_doc_ie (IEGroup lev doc) = do rn_doc <- rnHsDoc doc
                                         return (IEGroup lev rn_doc)
    lookup_doc_ie (IEDoc doc)       = do rn_doc <- rnHsDoc doc
                                         return (IEDoc rn_doc)
    lookup_doc_ie (IEDocNamed str)  = return (IEDocNamed str)
    lookup_doc_ie _ = panic "lookup_doc_ie"    -- Other cases covered earlier


isDoc :: IE RdrName -> Bool
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
        -- we use -XNoImplicitPrelude as a clue that the filter is
        -- worth while.  Really, it's only useful for GHC.Base and GHC.Tuple.
        --
        -- It's worth doing because it makes the environment smaller for
        -- every module that imports the Prelude
  | otherwise
  = case prov of
        LocalDef | Just name_mod <- nameModule_maybe name
                 -> moduleName name_mod == mod
                 | otherwise -> False
        Imported is -> any unQualSpecOK is && any (qualSpecOK mod) is

-------------------------------
check_occs :: IE RdrName -> ExportOccMap -> [Name] -> RnM ExportOccMap
check_occs ie occs names
  = foldlM check occs names
  where
    check occs name
      = case lookupOccEnv occs name_occ of
          Nothing -> return (extendOccEnv occs name_occ (name, ie))

          Just (name', ie')
            | name == name'   -- Duplicate export
            -- But we don't want to warn if the same thing is exported
            -- by two different module exports. See ticket #4478.
            -> do unless (diffModules ie ie') $ do
                      warn_dup_exports <- doptM Opt_WarnDuplicateExports
                      warnIf warn_dup_exports (dupExportWarn name_occ ie ie')
                  return occs

            | otherwise    -- Same occ name but different names: an error
            ->  do { global_env <- getGlobalRdrEnv ;
                     addErr (exportClashErr global_env name' name ie' ie) ;
                     return occs }
      where
        name_occ = nameOccName name
        -- True if the two IE RdrName are different module exports.
        diffModules (IEModuleContents n1) (IEModuleContents n2) = n1 /= n2
        diffModules _                     _                     = False
\end{code}

%*********************************************************
%*                                                       *
\subsection{Deprecations}
%*                                                       *
%*********************************************************

\begin{code}
finishWarnings :: DynFlags -> Maybe WarningTxt
               -> TcGblEnv -> RnM TcGblEnv
-- (a) Report usage of imports that are deprecated or have other warnings
-- (b) If the whole module is warned about or deprecated, update tcg_warns
--     All this happens only once per module
finishWarnings dflags mod_warn tcg_env
  = do  { (eps,hpt) <- getEpsAndHpt
        ; ifDOptM Opt_WarnWarningsDeprecations $
          mapM_ (check hpt (eps_PIT eps)) all_gres
                -- By this time, typechecking is complete,
                -- so the PIT is fully populated

        -- Deal with a module deprecation; it overrides all existing warns
        ; let new_warns = case mod_warn of
                                Just txt -> WarnAll txt
                                Nothing  -> tcg_warns tcg_env
        ; return (tcg_env { tcg_warns = new_warns }) }
  where
    used_names = allUses (tcg_dus tcg_env)
        -- Report on all deprecated uses; hence allUses
    all_gres   = globalRdrEnvElts (tcg_rdr_env tcg_env)

    check hpt pit gre@(GRE {gre_name = name, gre_prov = Imported (imp_spec:_)})
      | name `elemNameSet` used_names
      , Just deprec_txt <- lookupImpDeprec dflags hpt pit gre
      = addWarnAt (importSpecLoc imp_spec)
                  (sep [ptext (sLit "In the use of") <+>
                        pprNonVarNameSpace (occNameSpace (nameOccName name)) <+>
                        quotes (ppr name),
                      (parens imp_msg) <> colon,
                      (ppr deprec_txt) ])
        where
          name_mod = ASSERT2( isExternalName name, ppr name ) nameModule name
          imp_mod  = importSpecModule imp_spec
          imp_msg  = ptext (sLit "imported from") <+> ppr imp_mod <> extra
          extra | imp_mod == moduleName name_mod = empty
                | otherwise = ptext (sLit ", but defined in") <+> ppr name_mod

    check _ _ _ = return ()        -- Local, or not used, or not deprectated
            -- The Imported pattern-match: don't deprecate locally defined names
            -- For a start, we may be exporting a deprecated thing
            -- Also we may use a deprecated thing in the defn of another
            -- deprecated things.  We may even use a deprecated thing in
            -- the defn of a non-deprecated thing, when changing a module's
            -- interface

lookupImpDeprec :: DynFlags -> HomePackageTable -> PackageIfaceTable
                -> GlobalRdrElt -> Maybe WarningTxt
-- The name is definitely imported, so look in HPT, PIT
lookupImpDeprec dflags hpt pit gre
  = case lookupIfaceByModule dflags hpt pit mod of
        Just iface -> mi_warn_fn iface name `mplus`    -- Bleat if the thing, *or
                      case gre_par gre of
                        ParentIs p -> mi_warn_fn iface p    -- its parent*, is warn'd
                        NoParent   -> Nothing

        Nothing -> Nothing    -- See Note [Used names with interface not loaded]
  where
    name = gre_name gre
    mod = ASSERT2( isExternalName name, ppr name ) nameModule name
\end{code}

Note [Used names with interface not loaded]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By now all the interfaces should have been loaded,
because reportDeprecations happens after typechecking.
However, it's still (just) possible to to find a used
Name whose interface hasn't been loaded:

a) It might be a WiredInName; in that case we may not load
   its interface (although we could).

b) It might be GHC.Real.fromRational, or GHC.Num.fromInteger
   These are seen as "used" by the renamer (if -XRebindableSyntax)
   is on), but the typechecker may discard their uses
   if in fact the in-scope fromRational is GHC.Read.fromRational,
   (see tcPat.tcOverloadedLit), and the typechecker sees that the type
   is fixed, say, to GHC.Base.Float (see Inst.lookupSimpleInst).
   In that obscure case it won't force the interface in.

In both cases we simply don't permit deprecations;
this is, after all, wired-in stuff.


%*********************************************************
%*                                                       *
\subsection{Unused names}
%*                                                       *
%*********************************************************

\begin{code}
reportUnusedNames :: Maybe [LIE RdrName]    -- Export list
                  -> TcGblEnv -> RnM ()
reportUnusedNames _export_decls gbl_env
  = do  { traceRn ((text "RUN") <+> (ppr (tcg_dus gbl_env)))
        ; warnUnusedImportDecls gbl_env
        ; warnUnusedTopBinds   unused_locals }
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
    _defined_and_used, defined_but_not_used :: [GlobalRdrElt]
    (_defined_and_used, defined_but_not_used)
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
\end{code}

%*********************************************************
%*                                                       *
\subsection{Unused imports}
%*                                                       *
%*********************************************************

This code finds which import declarations are unused.  The
specification and implementation notes are here:
  http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/UnusedImports

\begin{code}
type ImportDeclUsage
   = ( LImportDecl Name   -- The import declaration
     , [AvailInfo]        -- What *is* used (normalised)
     , [Name] )           -- What is imported but *not* used
\end{code}

\begin{code}
warnUnusedImportDecls :: TcGblEnv -> RnM ()
warnUnusedImportDecls gbl_env
  = do { uses <- readMutVar (tcg_used_rdrnames gbl_env)
       ; let imports = filter explicit_import (tcg_rn_imports gbl_env)
             rdr_env = tcg_rdr_env gbl_env

       ; let usage :: [ImportDeclUsage]
             usage = findImportUsage imports rdr_env (Set.elems uses)

       ; ifDOptM Opt_WarnUnusedImports $
         mapM_ warnUnusedImport usage

       ; ifDOptM Opt_D_dump_minimal_imports $
         printMinimalImports usage }
  where
    explicit_import (L loc _) = isGoodSrcSpan loc
        -- Filter out the implicit Prelude import
        -- which we do not want to bleat about
\end{code}

\begin{code}
findImportUsage :: [LImportDecl Name]
                -> GlobalRdrEnv
                -> [RdrName]
                -> [ImportDeclUsage]

type ImportMap = Map SrcLoc [AvailInfo]
-- The intermediate data struture records, for each import
-- declaration, what stuff brought into scope by that
-- declaration is actually used in the module.
--
-- The SrcLoc is the location of the start
-- of a particular 'import' declaration
--
-- The AvailInfos are the things imported from that decl
-- (just a list, not normalised)

findImportUsage imports rdr_env rdrs
  = map unused_decl imports
  where
    import_usage :: ImportMap
    import_usage = foldr (addUsedRdrName rdr_env) Map.empty rdrs

    unused_decl decl@(L loc (ImportDecl { ideclHiding = imps }))
      = (decl, nubAvails used_avails, unused_imps)
      where
        used_avails = Map.lookup (srcSpanStart loc) import_usage `orElse` []
        dont_report_as_unused = foldr add emptyNameSet used_avails
        add (Avail n) s = s `addOneToNameSet` n
        add (AvailTC n ns) s = s `addListToNameSet` (n:ns)
       -- If you use 'signum' from Num, then the user may well have
       -- imported Num(signum).  We don't want to complain that
       -- Num is not itself mentioned.  Hence adding 'n' as
       -- well to the list of of "don't report if unused" names

        unused_imps = case imps of
                        Just (False, imp_ies) -> nameSetToList unused_imps
                          where
                            imp_names = mkNameSet (concatMap (ieNames . unLoc) imp_ies)
                            unused_imps = imp_names `minusNameSet` dont_report_as_unused

                        _other -> []    -- No explicit import list => no unused-name list

addUsedRdrName :: GlobalRdrEnv -> RdrName -> ImportMap -> ImportMap
-- For a used RdrName, find all the import decls that brought
-- it into scope; choose one of them (bestImport), and record
-- the RdrName in that import decl's entry in the ImportMap
addUsedRdrName rdr_env rdr imp_map
  | [gre] <- lookupGRE_RdrName rdr rdr_env
  , Imported imps <- gre_prov gre
  = add_imp gre (bestImport imps) imp_map
  | otherwise
  = imp_map
  where
    add_imp :: GlobalRdrElt -> ImportSpec -> ImportMap -> ImportMap
    add_imp gre (ImpSpec { is_decl = imp_decl_spec }) imp_map
      = Map.insertWith add decl_loc [avail] imp_map
      where
        add _ avails = avail : avails -- add is really just a specialised (++)
        decl_loc = srcSpanStart (is_dloc imp_decl_spec)
        name     = gre_name gre
        avail    = case gre_par gre of
                      ParentIs p                  -> AvailTC p [name]
                      NoParent | isTyConName name -> AvailTC name [name]
                               | otherwise        -> Avail name

    bestImport :: [ImportSpec] -> ImportSpec
    bestImport iss
      = case partition isImpAll iss of
          ([], imp_somes) -> textuallyFirst imp_somes
          (imp_alls, _)   -> textuallyFirst imp_alls

    textuallyFirst :: [ImportSpec] -> ImportSpec
    textuallyFirst iss = case sortWith (is_dloc . is_decl) iss of
                           []     -> pprPanic "textuallyFirst" (ppr iss)
                           (is:_) -> is

    isImpAll :: ImportSpec -> Bool
    isImpAll (ImpSpec { is_item = ImpAll }) = True
    isImpAll _other                         = False
\end{code}

\begin{code}
warnUnusedImport :: ImportDeclUsage -> RnM ()
warnUnusedImport (L loc decl, used, unused)
  | Just (False,[]) <- ideclHiding decl
                = return ()            -- Do not warn for 'import M()'
  | null used   = addWarnAt loc msg1   -- Nothing used; drop entire decl
  | null unused = return ()            -- Everything imported is used; nop
  | otherwise   = addWarnAt loc msg2   -- Some imports are unused
  where
    msg1 = vcat [pp_herald <+> quotes pp_mod <+> pp_not_used,
                 nest 2 (ptext (sLit "except perhaps to import instances from")
                                   <+> quotes pp_mod),
                 ptext (sLit "To import instances alone, use:")
                                   <+> ptext (sLit "import") <+> pp_mod <> parens empty ]
    msg2 = sep [pp_herald <+> quotes (pprWithCommas ppr unused),
                    text "from module" <+> quotes pp_mod <+> pp_not_used]
    pp_herald   = text "The import of"
    pp_mod      = ppr (unLoc (ideclName decl))
    pp_not_used = text "is redundant"
\end{code}

To print the minimal imports we walk over the user-supplied import
decls, and simply trim their import lists.  NB that

  * We do *not* change the 'qualified' or 'as' parts!

  * We do not disard a decl altogether; we might need instances
    from it.  Instead we just trim to an empty import list

\begin{code}
printMinimalImports :: [ImportDeclUsage] -> RnM ()
printMinimalImports imports_w_usage
  = do { imports' <- mapM mk_minimal imports_w_usage
       ; this_mod <- getModule
       ; dflags <- getDOpts
       ; liftIO $
         do { h <- openFile (mkFilename this_mod) WriteMode
            ; printForUser dflags h neverQualify (vcat (map ppr imports')) }
              -- The neverQualify is important.  We are printing Names
              -- but they are in the context of an 'import' decl, and
              -- we never qualify things inside there
              -- E.g.   import Blag( f, b )
              -- not    import Blag( Blag.f, Blag.g )!
       }
  where
    mkFilename this_mod = moduleNameString (moduleName this_mod) ++ ".imports"

    mk_minimal (L l decl, used, unused)
      | null unused
      , Just (False, _) <- ideclHiding decl
      = return (L l decl)
      | otherwise
      = do { let ImportDecl { ideclName    = L _ mod_name
                            , ideclSource  = is_boot
                            , ideclPkgQual = mb_pkg } = decl
           ; iface <- loadSrcInterface doc mod_name is_boot mb_pkg
           ; let lies = map (L l) (concatMap (to_ie iface) used)
           ; return (L l (decl { ideclHiding = Just (False, lies) })) }
      where
        doc = text "Compute minimal imports for" <+> ppr decl

    to_ie :: ModIface -> AvailInfo -> [IE Name]
    -- The main trick here is that if we're importing all the constructors
    -- we want to say "T(..)", but if we're importing only a subset we want
    -- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie _ (Avail n)
       = [IEVar n]
    to_ie _ (AvailTC n [m])
       | n==m = [IEThingAbs n]
    to_ie iface (AvailTC n ns)
      = case [xs | (m,as) <- mi_exports iface
                 , m == n_mod
                 , AvailTC x xs <- as
                 , x == nameOccName n
                 , x `elem` xs    -- Note [Partial export]
                 ] of
           [xs] | all_used xs -> [IEThingAll n]
                | otherwise   -> [IEThingWith n (filter (/= n) ns)]
           _other             -> (map IEVar ns)
        where
          all_used avail_occs = all (`elem` map nameOccName ns) avail_occs
          n_mod = ASSERT( isExternalName n ) nameModule n
\end{code}

Note [Partial export]
~~~~~~~~~~~~~~~~~~~~~
Suppose we have

   module A( op ) where
     class C a where
       op :: a -> a

   module B where
   import A
   f = ..op...

Then the minimal import for module B is
   import A( op )
not
   import A( C( op ) )
which we would usually generate if C was exported from B.  Hence
the (x `elem` xs) test when deciding what to generate.


%************************************************************************
%*                                                                      *
\subsection{Errors}
%*                                                                      *
%************************************************************************

\begin{code}
qualImportItemErr :: RdrName -> SDoc
qualImportItemErr rdr
  = hang (ptext (sLit "Illegal qualified name in import item:"))
       2 (ppr rdr)

badImportItemErrStd :: ModIface -> ImpDeclSpec -> IE RdrName -> SDoc
badImportItemErrStd iface decl_spec ie
  = sep [ptext (sLit "Module"), quotes (ppr (is_mod decl_spec)), source_import,
         ptext (sLit "does not export"), quotes (ppr ie)]
  where
    source_import | mi_boot iface = ptext (sLit "(hi-boot interface)")
                  | otherwise     = empty

badImportItemErrDataCon :: OccName -> ModIface -> ImpDeclSpec -> IE RdrName -> SDoc
badImportItemErrDataCon dataType iface decl_spec ie
  = vcat [ ptext (sLit "In module")
             <+> quotes (ppr (is_mod decl_spec))
             <+> source_import <> colon
         , nest 2 $ quotes datacon
             <+> ptext (sLit "is a data constructor of")
             <+> quotes (ppr dataType)
         , ptext (sLit "To import it use")
         , nest 2 $ quotes (ptext (sLit "import")
             <+> ppr (is_mod decl_spec)
             <+> parens (ppr dataType <+> parens datacon))
         , ptext (sLit "or")
         , nest 2 $ quotes (ptext (sLit "import")
             <+> ppr (is_mod decl_spec)
             <+> parens (ppr dataType <+> parens (ptext $ sLit "..")))
         ]
  where
    datacon = ppr . rdrNameOcc $ ieName ie
    source_import | mi_boot iface = ptext (sLit "(hi-boot interface)")
                  | otherwise     = empty

badImportItemErr :: ModIface -> ImpDeclSpec -> IE RdrName -> [AvailInfo] -> SDoc
badImportItemErr iface decl_spec ie avails
  = case find checkIfDataCon avails of
      Just con -> badImportItemErrDataCon (availOccName con) iface decl_spec ie
      Nothing  -> badImportItemErrStd iface decl_spec ie
  where
    checkIfDataCon (AvailTC _ ns) =
      case find (\n -> importedFS == nameOccNameFS n) ns of
        Just n  -> isDataConName n
        Nothing -> False
    checkIfDataCon _ = False
    availOccName = nameOccName . availName
    nameOccNameFS = occNameFS . nameOccName
    importedFS = occNameFS . rdrNameOcc $ ieName ie

illegalImportItemErr :: SDoc
illegalImportItemErr = ptext (sLit "Illegal import item")

dodgyImportWarn :: RdrName -> SDoc
dodgyImportWarn item = dodgyMsg (ptext (sLit "import")) item
dodgyExportWarn :: Name -> SDoc
dodgyExportWarn item = dodgyMsg (ptext (sLit "export")) item

dodgyMsg :: OutputableBndr n => SDoc -> n -> SDoc
dodgyMsg kind tc
  = sep [ ptext (sLit "The") <+> kind <+> ptext (sLit "item") <+> quotes (ppr (IEThingAll tc))
                <+> ptext (sLit "suggests that"),
          quotes (ppr tc) <+> ptext (sLit "has (in-scope) constructors or class methods,"),
          ptext (sLit "but it has none") ]

exportItemErr :: IE RdrName -> SDoc
exportItemErr export_item
  = sep [ ptext (sLit "The export item") <+> quotes (ppr export_item),
          ptext (sLit "attempts to export constructors or class methods that are not visible here") ]

typeItemErr :: Name -> SDoc -> SDoc
typeItemErr name wherestr
  = sep [ ptext (sLit "Using 'type' tag on") <+> quotes (ppr name) <+> wherestr,
          ptext (sLit "Use -XTypeFamilies to enable this extension") ]

exportClashErr :: GlobalRdrEnv -> Name -> Name -> IE RdrName -> IE RdrName
               -> Message
exportClashErr global_env name1 name2 ie1 ie2
  = vcat [ ptext (sLit "Conflicting exports for") <+> quotes (ppr occ) <> colon
         , ppr_export ie1' name1'
         , ppr_export ie2' name2' ]
  where
    occ = nameOccName name1
    ppr_export ie name = nest 2 (quotes (ppr ie) <+> ptext (sLit "exports") <+>
                                 quotes (ppr name) <+> pprNameProvenance (get_gre name))

    -- get_gre finds a GRE for the Name, so that we can show its provenance
    get_gre name
        = case lookupGRE_Name global_env name of
             (gre:_) -> gre
             []      -> pprPanic "exportClashErr" (ppr name)
    get_loc name = greSrcSpan (get_gre name)
    (name1', ie1', name2', ie2') = if get_loc name1 < get_loc name2
                                   then (name1, ie1, name2, ie2)
                                   else (name2, ie2, name1, ie1)

-- the SrcSpan that pprNameProvenance prints out depends on whether
-- the Name is defined locally or not: for a local definition the
-- definition site is used, otherwise the location of the import
-- declaration.  We want to sort the export locations in
-- exportClashErr by this SrcSpan, we need to extract it:
greSrcSpan :: GlobalRdrElt -> SrcSpan
greSrcSpan gre
  | Imported (is:_) <- gre_prov gre = is_dloc (is_decl is)
  | otherwise                       = name_span
  where
    name_span = nameSrcSpan (gre_name gre)

addDupDeclErr :: [Name] -> TcRn ()
addDupDeclErr []
  = panic "addDupDeclErr: empty list"
addDupDeclErr names@(name : _)
  = addErrAt (getSrcSpan (last sorted_names)) $
    -- Report the error at the later location
    vcat [ptext (sLit "Multiple declarations of") <+> quotes (ppr name),
          ptext (sLit "Declared at:") <+> vcat (map (ppr . nameSrcLoc) sorted_names)]
  where
    sorted_names = sortWith nameSrcLoc names

dupExportWarn :: OccName -> IE RdrName -> IE RdrName -> SDoc
dupExportWarn occ_name ie1 ie2
  = hsep [quotes (ppr occ_name),
          ptext (sLit "is exported by"), quotes (ppr ie1),
          ptext (sLit "and"),            quotes (ppr ie2)]

dupModuleExport :: ModuleName -> SDoc
dupModuleExport mod
  = hsep [ptext (sLit "Duplicate"),
          quotes (ptext (sLit "Module") <+> ppr mod),
          ptext (sLit "in export list")]

moduleNotImported :: ModuleName -> SDoc
moduleNotImported mod
  = ptext (sLit "The export item `module") <+> ppr mod <>
    ptext (sLit "' is not imported")

nullModuleExport :: ModuleName -> SDoc
nullModuleExport mod
  = ptext (sLit "The export item `module") <+> ppr mod <> ptext (sLit "' exports nothing")

missingImportListWarn :: ModuleName -> SDoc
missingImportListWarn mod
  = ptext (sLit "The module") <+> quotes (ppr mod) <+> ptext (sLit "does not have an explicit import list")

missingImportListItem :: IE RdrName -> SDoc
missingImportListItem ie
  = ptext (sLit "The import item") <+> quotes (ppr ie) <+> ptext (sLit "does not have an explicit import list")

moduleWarn :: ModuleName -> WarningTxt -> SDoc
moduleWarn mod (WarningTxt txt)
  = sep [ ptext (sLit "Module") <+> quotes (ppr mod) <> ptext (sLit ":"),
          nest 2 (vcat (map ppr txt)) ]
moduleWarn mod (DeprecatedTxt txt)
  = sep [ ptext (sLit "Module") <+> quotes (ppr mod)
                                <+> ptext (sLit "is deprecated:"),
          nest 2 (vcat (map ppr txt)) ]

implicitPreludeWarn :: SDoc
implicitPreludeWarn
  = ptext (sLit "Module `Prelude' implicitly imported")

packageImportErr :: SDoc
packageImportErr
  = ptext (sLit "Package-qualified imports are not enabled; use -XPackageImports")
\end{code}
