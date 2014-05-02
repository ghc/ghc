%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
module RnNames (
        rnImports, getLocalNonValBinders,
        rnExports, extendGlobalRdrEnvRn,
        gresFromAvails,
        reportUnusedNames,
        checkConName
    ) where

#include "HsVersions.h"

import DynFlags
import HsSyn
import TcEnv            ( isBrackStage )
import RnEnv
import RnHsDoc          ( rnHsDoc )
import LoadIface        ( loadSrcInterface )
import TcRnMonad
import PrelNames
import Module
import Name
import NameEnv
import NameSet
import Avail
import HscTypes
import RdrName
import Outputable
import Maybes
import SrcLoc
import BasicTypes      ( TopLevelFlag(..) )
import ErrUtils
import Util
import FastString
import ListSetOps

import Control.Monad
import Data.Map         ( Map )
import qualified Data.Map as Map
import Data.List        ( partition, (\\), find )
import qualified Data.Set as Set
import System.FilePath  ((</>))
import System.IO
\end{code}


%************************************************************************
%*                                                                      *
\subsection{rnImports}
%*                                                                      *
%************************************************************************

Note [Tracking Trust Transitively]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we import a package as well as checking that the direct imports are safe
according to the rules outlined in the Note [HscMain . Safe Haskell Trust Check]
we must also check that these rules hold transitively for all dependent modules
and packages. Doing this without caching any trust information would be very
slow as we would need to touch all packages and interface files a module depends
on. To avoid this we make use of the property that if a modules Safe Haskell
mode changes, this triggers a recompilation from that module in the dependcy
graph. So we can just worry mostly about direct imports.

There is one trust property that can change for a package though without
recompliation being triggered: package trust. So we must check that all
packages a module tranitively depends on to be trusted are still trusted when
we are compiling this module (as due to recompilation avoidance some modules
below may not be considered trusted any more without recompilation being
triggered).

We handle this by augmenting the existing transitive list of packages a module M
depends on with a bool for each package that says if it must be trusted when the
module M is being checked for trust. This list of trust required packages for a
single import is gathered in the rnImportDecl function and stored in an
ImportAvails data structure. The union of these trust required packages for all
imports is done by the rnImports function using the combine function which calls
the plusImportAvails function that is a union operation for the ImportAvails
type. This gives us in an ImportAvails structure all packages required to be
trusted for the module we are currently compiling. Checking that these packages
are still trusted (and that direct imports are trusted) is done in
HscMain.checkSafeImports.

See the note below, [Trust Own Package] for a corner case in this method and
how its handled.


Note [Trust Own Package]
~~~~~~~~~~~~~~~~~~~~~~~~
There is a corner case of package trust checking that the usual transitive check
doesn't cover. (For how the usual check operates see the Note [Tracking Trust
Transitively] below). The case is when you import a -XSafe module M and M
imports a -XTrustworthy module N. If N resides in a different package than M,
then the usual check works as M will record a package dependency on N's package
and mark it as required to be trusted. If N resides in the same package as M
though, then importing M should require its own package be trusted due to N
(since M is -XSafe so doesn't create this requirement by itself). The usual
check fails as a module doesn't record a package dependency of its own package.
So instead we now have a bool field in a modules interface file that simply
states if the module requires its own package to be trusted. This field avoids
us having to load all interface files that the module depends on to see if one
is trustworthy.


Note [Trust Transitive Property]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
So there is an interesting design question in regards to transitive trust
checking. Say I have a module B compiled with -XSafe. B is dependent on a bunch
of modules and packages, some packages it requires to be trusted as its using
-XTrustworthy modules from them. Now if I have a module A that doesn't use safe
haskell at all and simply imports B, should A inherit all the the trust
requirements from B? Should A now also require that a package p is trusted since
B required it?

We currently say no but saying yes also makes sense. The difference is, if a
module M that doesn't use Safe Haskell imports a module N that does, should all
the trusted package requirements be dropped since M didn't declare that it cares
about Safe Haskell (so -XSafe is more strongly associated with the module doing
the importing) or should it be done still since the author of the module N that
uses Safe Haskell said they cared (so -XSafe is more strongly associated with
the module that was compiled that used it).

Going with yes is a simpler semantics we think and harder for the user to stuff
up but it does mean that Safe Haskell will affect users who don't care about
Safe Haskell as they might grab a package from Cabal which uses safe haskell (say
network) and that packages imports -XTrustworthy modules from another package
(say bytestring), so requires that package is trusted. The user may now get
compilation errors in code that doesn't do anything with Safe Haskell simply
because they are using the network package. They will have to call 'ghc-pkg
trust network' to get everything working. Due to this invasive nature of going
with yes we have gone with no for now.


\begin{code}
-- | Process Import Decls
-- Do the non SOURCE ones first, so that we get a helpful warning for SOURCE
-- ones that are unnecessary
rnImports :: [LImportDecl RdrName]
          -> RnM ([LImportDecl Name], GlobalRdrEnv, ImportAvails, AnyHpcUsage)
rnImports imports = do
    this_mod <- getModule
    let (source, ordinary) = partition is_source_import imports
        is_source_import d = ideclSource (unLoc d)
    stuff1 <- mapAndReportM (rnImportDecl this_mod) ordinary
    stuff2 <- mapAndReportM (rnImportDecl this_mod) source
    -- Safe Haskell: See Note [Tracking Trust Transitively]
    let (decls, rdr_env, imp_avails, hpc_usage) = combine (stuff1 ++ stuff2)
    return (decls, rdr_env, imp_avails, hpc_usage)

  where
    combine :: [(LImportDecl Name,  GlobalRdrEnv, ImportAvails, AnyHpcUsage)]
            -> ([LImportDecl Name], GlobalRdrEnv, ImportAvails, AnyHpcUsage)
    combine = foldr plus ([], emptyGlobalRdrEnv, emptyImportAvails, False)

    plus (decl,  gbl_env1, imp_avails1,hpc_usage1)
         (decls, gbl_env2, imp_avails2,hpc_usage2)
      = ( decl:decls,
          gbl_env1 `plusGlobalRdrEnv` gbl_env2,
          imp_avails1 `plusImportAvails` imp_avails2,
          hpc_usage1 || hpc_usage2 )

rnImportDecl  :: Module -> LImportDecl RdrName
              -> RnM (LImportDecl Name, GlobalRdrEnv, ImportAvails, AnyHpcUsage)
rnImportDecl this_mod
             (L loc decl@(ImportDecl { ideclName = loc_imp_mod_name, ideclPkgQual = mb_pkg
                                     , ideclSource = want_boot, ideclSafe = mod_safe
                                     , ideclQualified = qual_only, ideclImplicit = implicit
                                     , ideclAs = as_mod, ideclHiding = imp_details }))
  = setSrcSpan loc $ do

    when (isJust mb_pkg) $ do
        pkg_imports <- xoptM Opt_PackageImports
        when (not pkg_imports) $ addErr packageImportErr

    -- If there's an error in loadInterface, (e.g. interface
    -- file not found) we get lots of spurious errors from 'filterImports'
    let imp_mod_name = unLoc loc_imp_mod_name
        doc = ppr imp_mod_name <+> ptext (sLit "is directly imported")

    -- Check for a missing import list (Opt_WarnMissingImportList also
    -- checks for T(..) items but that is done in checkDodgyImport below)
    case imp_details of
        Just (False, _) -> return () -- Explicit import list
        _  | implicit   -> return () -- Do not bleat for implicit imports
           | qual_only  -> return ()
           | otherwise  -> whenWOptM Opt_WarnMissingImportList $
                           addWarn (missingImportListWarn imp_mod_name)

    iface <- loadSrcInterface doc imp_mod_name want_boot mb_pkg

    -- Compiler sanity check: if the import didn't say
    -- {-# SOURCE #-} we should not get a hi-boot file
    WARN( not want_boot && mi_boot iface, ppr imp_mod_name ) do

    -- Issue a user warning for a redundant {- SOURCE -} import
    -- NB that we arrange to read all the ordinary imports before
    -- any of the {- SOURCE -} imports.
    --
    -- in --make and GHCi, the compilation manager checks for this,
    -- and indeed we shouldn't do it here because the existence of
    -- the non-boot module depends on the compilation order, which
    -- is not deterministic.  The hs-boot test can show this up.
    dflags <- getDynFlags
    warnIf (want_boot && not (mi_boot iface) && isOneShot (ghcMode dflags))
           (warnRedundantSourceImport imp_mod_name)
    when (mod_safe && not (safeImportsOn dflags)) $
        addErrAt loc (ptext (sLit "safe import can't be used as Safe Haskell isn't on!")
                  $+$ ptext (sLit $ "please enable Safe Haskell through either "
                                 ++ "Safe, Trustworthy or Unsafe"))

    let imp_mod    = mi_module iface
        warns      = mi_warns iface
        orph_iface = mi_orphan iface
        has_finsts = mi_finsts iface
        deps       = mi_deps iface
        trust      = getSafeMode $ mi_trust iface
        trust_pkg  = mi_trust_pkg iface

        qual_mod_name = as_mod `orElse` imp_mod_name
        imp_spec  = ImpDeclSpec { is_mod = imp_mod_name, is_qual = qual_only,
                                  is_dloc = loc, is_as = qual_mod_name }

    -- filter the imports according to the import declaration
    (new_imp_details, gres) <- filterImports iface imp_spec imp_details

    let gbl_env = mkGlobalRdrEnv (filterOut from_this_mod gres)
        from_this_mod gre = nameModule (gre_name gre) == this_mod
        -- If the module exports anything defined in this module, just
        -- ignore it.  Reason: otherwise it looks as if there are two
        -- local definition sites for the thing, and an error gets
        -- reported.  Easiest thing is just to filter them out up
        -- front. This situation only arises if a module imports
        -- itself, or another module that imported it.  (Necessarily,
        -- this invoves a loop.)
        --
        -- We do this *after* filterImports, so that if you say
        --      module A where
        --         import B( AType )
        --         type AType = ...
        --
        --      module B( AType ) where
        --         import {-# SOURCE #-} A( AType )
        --
        -- then you won't get a 'B does not export AType' message.


        -- Compute new transitive dependencies

        orphans | orph_iface = ASSERT( not (imp_mod `elem` dep_orphs deps) )
                               imp_mod : dep_orphs deps
                | otherwise  = dep_orphs deps

        finsts | has_finsts = ASSERT( not (imp_mod `elem` dep_finsts deps) )
                              imp_mod : dep_finsts deps
               | otherwise  = dep_finsts deps

        pkg = modulePackageId (mi_module iface)

        -- Does this import mean we now require our own pkg
        -- to be trusted? See Note [Trust Own Package]
        ptrust = trust == Sf_Trustworthy || trust_pkg

        (dependent_mods, dependent_pkgs, pkg_trust_req)
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
                ((imp_mod_name, want_boot) : dep_mods deps, dep_pkgs deps, ptrust)

           | otherwise =
                -- Imported module is from another package
                -- Dump the dependent modules
                -- Add the package imp_mod comes from to the dependent packages
                ASSERT2( not (pkg `elem` (map fst $ dep_pkgs deps))
                       , ppr pkg <+> ppr (dep_pkgs deps) )
                ([], (pkg, False) : dep_pkgs deps, False)

        -- True <=> import M ()
        import_all = case imp_details of
                        Just (is_hiding, ls) -> not is_hiding && null ls
                        _                    -> False

        -- should the import be safe?
        mod_safe' = mod_safe
                    || (not implicit && safeDirectImpsReq dflags)
                    || (implicit && safeImplicitImpsReq dflags)

        imports   = ImportAvails {
                        imp_mods       = unitModuleEnv imp_mod
                                        [(qual_mod_name, import_all, loc, mod_safe')],
                        imp_orphs      = orphans,
                        imp_finsts     = finsts,
                        imp_dep_mods   = mkModDeps dependent_mods,
                        imp_dep_pkgs   = map fst $ dependent_pkgs,
                        -- Add in the imported modules trusted package
                        -- requirements. ONLY do this though if we import the
                        -- module as a safe import.
                        -- See Note [Tracking Trust Transitively]
                        -- and Note [Trust Transitive Property]
                        imp_trust_pkgs = if mod_safe'
                                             then map fst $ filter snd dependent_pkgs
                                             else [],
                        -- Do we require our own pkg to be trusted?
                        -- See Note [Trust Own Package]
                        imp_trust_own_pkg = pkg_trust_req
                   }

    -- Complain if we import a deprecated module
    whenWOptM Opt_WarnWarningsDeprecations (
       case warns of
          WarnAll txt -> addWarn $ moduleWarn imp_mod_name txt
          _           -> return ()
     )

    let new_imp_decl = L loc (decl { ideclSafe = mod_safe'
                                   , ideclHiding = new_imp_details })

    return (new_imp_decl, gbl_env, imports, mi_hpc iface)

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
See also: Note [Interactively-bound Ids in GHCi] in HscTypes

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
        ; isGHCi <- getIsGHCi
        ; let rdr_env  = tcg_rdr_env gbl_env
              fix_env  = tcg_fix_env gbl_env
              th_bndrs = tcl_th_bndrs lcl_env
              th_lvl   = thLevel stage

              -- Delete new_occs from global and local envs
              -- If we are in a TemplateHaskell decl bracket,
              --    we are going to shadow them
              -- See Note [Top-level Names in Template Haskell decl quotes]
              inBracket = isBrackStage stage
              lcl_env_TH = lcl_env { tcl_rdr = delLocalRdrEnvList (tcl_rdr lcl_env) new_occs }

              lcl_env2 | inBracket = lcl_env_TH
                       | otherwise = lcl_env

              rdr_env2  = extendGlobalRdrEnv (isGHCi && not inBracket) rdr_env avails
                 -- Shadowing only applies for GHCi decls outside brackets
                 -- e.g. (Trac #4127a)
                 --   ghci> runQ [d| class C a where f :: a
                 --                  f = True
                 --                  instance C Int where f = 2 |]
                 --   We don't want the f=True to shadow the f class-op

              lcl_env3 = lcl_env2 { tcl_th_bndrs = extendNameEnvList th_bndrs
                                                       [ (n, (TopLevel, th_lvl))
                                                       | n <- new_names ] }
              fix_env' = foldl extend_fix_env fix_env new_names
              dups = findLocalDupsRdrEnv rdr_env2 new_names

              gbl_env' = gbl_env { tcg_rdr_env = rdr_env2, tcg_fix_env = fix_env' }

        ; traceRn (text "extendGlobalRdrEnvRn 1" <+> (ppr avails $$ (ppr dups)))
        ; mapM_ (addDupDeclErr . map gre_name) dups

        ; traceRn (text "extendGlobalRdrEnvRn 2" <+> (pprGlobalRdrEnv True rdr_env2))
        ; return (gbl_env', lcl_env3) }
  where
    new_names = concatMap availNames avails
    new_occs  = map nameOccName new_names

    -- If there is a fixity decl for the gre, add it to the fixity env
    extend_fix_env fix_env name
      | Just (L _ fi) <- lookupFsEnv new_fixities (occNameFS occ)
      = extendNameEnv fix_env name (FixItem occ fi)
      | otherwise
      = fix_env
      where
        occ  = nameOccName name
\end{code}

@getLocalDeclBinders@ returns the names for an @HsDecl@.  It's
used for source code.

        *** See "THE NAMING STORY" in HsDecls ****

\begin{code}
getLocalNonValBinders :: MiniFixityEnv -> HsGroup RdrName
                      -> RnM ((TcGblEnv, TcLclEnv), NameSet)
-- Get all the top-level binders bound the group *except*
-- for value bindings, which are treated separately
-- Specifically we return AvailInfo for
--      type decls (incl constructors and record selectors)
--      class decls (including class ops)
--      associated types
--      foreign imports
--      (in hs-boot files) value signatures

getLocalNonValBinders fixity_env
     (HsGroup { hs_valds  = val_binds,
                hs_tyclds = tycl_decls,
                hs_instds = inst_decls,
                hs_fords  = foreign_decls })
  = do  { -- Process all type/class decls *except* family instances
        ; tc_avails <- mapM new_tc (tyClGroupConcat tycl_decls)
        ; traceRn (text "getLocalNonValBinders 1" <+> ppr tc_avails)
        ; envs <- extendGlobalRdrEnvRn tc_avails fixity_env
        ; setEnvs envs $ do {
            -- Bring these things into scope first
            -- See Note [Looking up family names in family instances]

          -- Process all family instances
          -- to bring new data constructors into scope
        ; nti_avails <- concatMapM new_assoc inst_decls

          -- Finish off with value binders:
          --    foreign decls for an ordinary module
          --    type sigs in case of a hs-boot file only
        ; is_boot <- tcIsHsBoot
        ; let val_bndrs | is_boot   = hs_boot_sig_bndrs
                        | otherwise = for_hs_bndrs
        ; val_avails <- mapM new_simple val_bndrs

        ; let avails    = nti_avails ++ val_avails
              new_bndrs = availsToNameSet avails `unionNameSets`
                          availsToNameSet tc_avails
        ; traceRn (text "getLocalNonValBinders 2" <+> ppr avails)
        ; envs <- extendGlobalRdrEnvRn avails fixity_env
        ; return (envs, new_bndrs) } }
  where
    for_hs_bndrs :: [Located RdrName]
    for_hs_bndrs = [ L decl_loc (unLoc nm)
                   | L decl_loc (ForeignImport nm _ _ _) <- foreign_decls]

    -- In a hs-boot file, the value binders come from the
    --  *signatures*, and there should be no foreign binders
    hs_boot_sig_bndrs = [ L decl_loc (unLoc n)
                        | L decl_loc (TypeSig ns _) <- val_sigs, n <- ns]
    ValBindsIn _ val_sigs = val_binds

      -- the SrcSpan attached to the input should be the span of the
      -- declaration, not just the name
    new_simple :: Located RdrName -> RnM AvailInfo
    new_simple rdr_name = do{ nm <- newTopSrcBinder rdr_name
                            ; return (Avail nm) }

    new_tc tc_decl              -- NOT for type/data instances
        = do { let bndrs = hsLTyClDeclBinders tc_decl
             ; names@(main_name : _) <- mapM newTopSrcBinder bndrs
             ; return (AvailTC main_name names) }

    new_assoc :: LInstDecl RdrName -> RnM [AvailInfo]
    new_assoc (L _ (TyFamInstD {})) = return []
      -- type instances don't bind new names

    new_assoc (L _ (DataFamInstD { dfid_inst = d }))
      = do { avail <- new_di Nothing d
           ; return [avail] }
    new_assoc (L _ (ClsInstD { cid_inst = ClsInstDecl
                             { cid_poly_ty = inst_ty
                             , cid_datafam_insts = adts } }))
      | Just (_, _, L loc cls_rdr, _) <- splitLHsInstDeclTy_maybe inst_ty
      = do { cls_nm <- setSrcSpan loc $ lookupGlobalOccRn cls_rdr
           ; mapM (new_di (Just cls_nm) . unLoc) adts }
      | otherwise
      = return []     -- Do not crash on ill-formed instances
                      -- Eg   instance !Show Int   Trac #3811c

    new_di :: Maybe Name -> DataFamInstDecl RdrName -> RnM AvailInfo
    new_di mb_cls ti_decl
        = do { main_name <- lookupFamInstName mb_cls (dfid_tycon ti_decl)
             ; sub_names <- mapM newTopSrcBinder (hsDataFamInstBinders ti_decl)
             ; return (AvailTC (unLoc main_name) sub_names) }
                        -- main_name is not bound here!
\end{code}

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

Solution is simple: process the type family declarations first, extend
the environment, and then process the type instances.


%************************************************************************
%*                                                                      *
\subsection{Filtering imports}
%*                                                                      *
%************************************************************************

@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

Note [Dealing with imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
For import M( ies ), we take the mi_exports of M, and make 
   imp_occ_env :: OccEnv (Name, AvailInfo, Maybe Name) 
One entry for each Name that M exports; the AvailInfo describes just
that Name.   

The situation is made more complicated by associated types. E.g.
   module M where
     class    C a    where { data T a }
     instance C Int  where { data T Int = T1 | T2 }
     instance C Bool where { data T Int = T3 }
Then M's export_avails are (recall the AvailTC invariant from Avails.hs)
  C(C,T), T(T,T1,T2,T3)
Notice that T appears *twice*, once as a child and once as a parent.
From this we construct the imp_occ_env
   C  -> (C,  C(C,T),        Nothing
   T  -> (T,  T(T,T1,T2,T3), Just C)
   T1 -> (T1, T(T1,T2,T3),   Nothing)   -- similarly T2,T3

Note that the imp_occ_env will have entries for data constructors too,
although we never look up data constructors.

\begin{code}
filterImports :: ModIface
              -> ImpDeclSpec                    -- The span for the entire import decl
              -> Maybe (Bool, [LIE RdrName])    -- Import spec; True => hiding
              -> RnM (Maybe (Bool, [LIE Name]), -- Import spec w/ Names
                      [GlobalRdrElt])           -- Same again, but in GRE form
filterImports iface decl_spec Nothing
  = return (Nothing, gresFromAvails prov (mi_exports iface))
  where
    prov = Imported [ImpSpec { is_decl = decl_spec, is_item = ImpAll }]


filterImports iface decl_spec (Just (want_hiding, import_items))
  = do  -- check for errors, convert RdrNames to Names
        items1 <- mapM lookup_lie import_items

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

        return (Just (want_hiding, map fst items2), gres)
  where
    all_avails = mi_exports iface

        -- See Note [Dealing with imports]
    imp_occ_env :: OccEnv (Name,        -- the name
                           AvailInfo,   -- the export item providing the name
                           Maybe Name)  -- the parent of associated types
    imp_occ_env = mkOccEnv_C combine [ (nameOccName n, (n, a, Nothing))
                                     | a <- all_avails, n <- availNames a]
      where
        -- See example in Note [Dealing with imports]
        -- 'combine' is only called for associated types which appear twice
        -- in the all_avails. In the example, we combine
        --    T(T,T1,T2,T3) and C(C,T)  to give   (T, T(T,T1,T2,T3), Just C)
        combine (name1, a1@(AvailTC p1 _), mp1)
                (name2, a2@(AvailTC p2 _), mp2)
          = ASSERT( name1 == name2 && isNothing mp1 && isNothing mp2 )
            if p1 == name1 then (name1, a1, Just p2)
                           else (name1, a2, Just p1)
        combine x y = pprPanic "filterImports/combine" (ppr x $$ ppr y)

    lookup_name :: RdrName -> IELookupM (Name, AvailInfo, Maybe Name)
    lookup_name rdr | isQual rdr              = failLookupWith (QualImportError rdr)
                    | Just succ <- mb_success = return succ
                    | otherwise               = failLookupWith BadImport
      where
        mb_success = lookupOccEnv imp_occ_env (rdrNameOcc rdr)

    lookup_lie :: LIE RdrName -> TcRn [(LIE Name, AvailInfo)]
    lookup_lie (L loc ieRdr)
        = do (stuff, warns) <- setSrcSpan loc $
                               liftM (fromMaybe ([],[])) $
                               run_lookup (lookup_ie ieRdr)
             mapM_ emit_warning warns
             return [ (L loc ie, avail) | (ie,avail) <- stuff ]
        where
            -- Warn when importing T(..) if T was exported abstractly
            emit_warning (DodgyImport n) = whenWOptM Opt_WarnDodgyImports $
              addWarn (dodgyImportWarn n)
            emit_warning MissingImportList = whenWOptM Opt_WarnMissingImportList $
              addWarn (missingImportListItem ieRdr)
            emit_warning BadImportW = whenWOptM Opt_WarnDodgyImports $
              addWarn (lookup_err_msg BadImport)

            run_lookup :: IELookupM a -> TcRn (Maybe a)
            run_lookup m = case m of
              Failed err -> addErr (lookup_err_msg err) >> return Nothing
              Succeeded a -> return (Just a)

            lookup_err_msg err = case err of
              BadImport -> badImportItemErr iface decl_spec ieRdr all_avails
              IllegalImport -> illegalImportItemErr
              QualImportError rdr -> qualImportItemErr rdr

        -- For each import item, we convert its RdrNames to Names,
        -- and at the same time construct an AvailInfo corresponding
        -- to what is actually imported by this item.
        -- Returns Nothing on error.
        -- We return a list here, because in the case of an import
        -- item like C, if we are hiding, then C refers to *both* a
        -- type/class and a data constructor.  Moreover, when we import
        -- data constructors of an associated family, we need separate
        -- AvailInfos for the data constructors and the family (as they have
        -- different parents).  See Note [Dealing with imports]
    lookup_ie :: IE RdrName -> IELookupM ([(IE Name, AvailInfo)], [IELookupWarning])
    lookup_ie ie = handle_bad_import $ do
      case ie of
        IEVar n -> do
            (name, avail, _) <- lookup_name n
            return ([(IEVar name, trimAvail avail name)], [])

        IEThingAll tc -> do
            (name, avail@(AvailTC name2 subs), mb_parent) <- lookup_name tc
            let warns | null (drop 1 subs)      = [DodgyImport tc]
                      | not (is_qual decl_spec) = [MissingImportList]
                      | otherwise               = []
            case mb_parent of
              -- non-associated ty/cls
              Nothing     -> return ([(IEThingAll name, avail)], warns)
              -- associated ty
              Just parent -> return ([(IEThingAll name,
                                       AvailTC name2 (subs \\ [name])),
                                      (IEThingAll name, AvailTC parent [name])],
                                     warns)

        IEThingAbs tc
            | want_hiding   -- hiding ( C )
                       -- Here the 'C' can be a data constructor
                       --  *or* a type/class, or even both
            -> let tc_name = lookup_name tc
                   dc_name = lookup_name (setRdrNameSpace tc srcDataName)
               in
               case catIELookupM [ tc_name, dc_name ] of
                 []    -> failLookupWith BadImport
                 names -> return ([mkIEThingAbs name | name <- names], [])
            | otherwise
            -> do nameAvail <- lookup_name tc
                  return ([mkIEThingAbs nameAvail], [])

        IEThingWith rdr_tc rdr_ns -> do
           (name, AvailTC _ ns, mb_parent) <- lookup_name rdr_tc

           -- Look up the children in the sub-names of the parent
           let subnames = case ns of   -- The tc is first in ns, 
                            [] -> []   -- if it is there at all
                                       -- See the AvailTC Invariant in Avail.hs
                            (n1:ns1) | n1 == name -> ns1
                                     | otherwise  -> ns
               mb_children = lookupChildren subnames rdr_ns

           children <- if any isNothing mb_children
                       then failLookupWith BadImport
                       else return (catMaybes mb_children)

           case mb_parent of
             -- non-associated ty/cls
             Nothing     -> return ([(IEThingWith name children,
                                      AvailTC name (name:children))],
                                    [])
             -- associated ty
             Just parent -> return ([(IEThingWith name children,
                                      AvailTC name children),
                                     (IEThingWith name children,
                                      AvailTC parent [name])],
                                    [])

        _other -> failLookupWith IllegalImport
        -- could be IEModuleContents, IEGroup, IEDoc, IEDocNamed
        -- all errors.

      where
        mkIEThingAbs (n, av, Nothing    ) = (IEThingAbs n, trimAvail av n)
        mkIEThingAbs (n, _,  Just parent) = (IEThingAbs n, AvailTC parent [n])

        handle_bad_import m = catchIELookup m $ \err -> case err of
          BadImport | want_hiding -> return ([], [BadImportW])
          _                       -> failLookupWith err

type IELookupM = MaybeErr IELookupError

data IELookupWarning
  = BadImportW
  | MissingImportList
  | DodgyImport RdrName
  -- NB. use the RdrName for reporting a "dodgy" import

data IELookupError
  = QualImportError RdrName
  | BadImport
  | IllegalImport

failLookupWith :: IELookupError -> IELookupM a
failLookupWith err = Failed err

catchIELookup :: IELookupM a -> (IELookupError -> IELookupM a) -> IELookupM a
catchIELookup m h = case m of
  Succeeded r -> return r
  Failed err  -> h err

catIELookupM :: [IELookupM a] -> [a]
catIELookupM ms = [ a | Succeeded a <- ms ]
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Import/Export Utils}
%*                                                                      *
%************************************************************************

\begin{code}
greExportAvail :: GlobalRdrElt -> AvailInfo
greExportAvail gre
  = case gre_par gre of
      ParentIs p                  -> AvailTC p [me]
      NoParent   | isTyConName me -> AvailTC me [me]
                 | otherwise      -> Avail   me
  where
    me = gre_name gre

plusAvail :: AvailInfo -> AvailInfo -> AvailInfo
plusAvail a1 a2
  | debugIsOn && availName a1 /= availName a2
  = pprPanic "RnEnv.plusAvail names differ" (hsep [ppr a1,ppr a2])
plusAvail a1@(Avail {})         (Avail {})      = a1
plusAvail (AvailTC _ [])        a2@(AvailTC {}) = a2
plusAvail a1@(AvailTC {})       (AvailTC _ [])  = a1
plusAvail (AvailTC n1 (s1:ss1)) (AvailTC n2 (s2:ss2))
  = case (n1==s1, n2==s2) of  -- Maintain invariant the parent is first
       (True,True)   -> AvailTC n1 (s1 : (ss1 `unionLists` ss2))
       (True,False)  -> AvailTC n1 (s1 : (ss1 `unionLists` (s2:ss2)))
       (False,True)  -> AvailTC n1 (s2 : ((s1:ss1) `unionLists` ss2))
       (False,False) -> AvailTC n1 ((s1:ss1) `unionLists` (s2:ss2))
plusAvail a1 a2 = pprPanic "RnEnv.plusAvail" (hsep [ppr a1,ppr a2])

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

lookupChildren :: [Name] -> [RdrName] -> [Maybe Name]
-- (lookupChildren all_kids rdr_items) maps each rdr_item to its
-- corresponding Name all_kids, if the former exists
-- The matching is done by FastString, not OccName, so that
--    Cls( meth, AssocTy )
-- will correctly find AssocTy among the all_kids of Cls, even though
-- the RdrName for AssocTy may have a (bogus) DataName namespace
-- (Really the rdr_items should be FastStrings in the first place.)
lookupChildren all_kids rdr_items
  = map (lookupFsEnv kid_env . occNameFS . rdrNameOcc) rdr_items
  where
    kid_env = mkFsEnv [(occNameFS (nameOccName n), n) | n <- all_kids]

-- | Combines 'AvailInfo's from the same family
-- 'avails' may have several items with the same availName
-- E.g  import Ix( Ix(..), index )
-- will give Ix(Ix,index,range) and Ix(index)
-- We want to combine these; addAvail does that
nubAvails :: [AvailInfo] -> [AvailInfo]
nubAvails avails = nameEnvElts (foldl add emptyNameEnv avails)
  where
    add env avail = extendNameEnv_C plusAvail env (availName avail) avail
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

Note [Exports of data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose you see (Trac #5306)
        module M where
          import X( F )
          data instance F Int = FInt
What does M export?  AvailTC F [FInt]
                  or AvailTC F [F,FInt]?
The former is strictly right because F isn't defined in this module.
But then you can never do an explicit import of M, thus
    import M( F( FInt ) )
because F isn't exported by M.  Nor can you import FInt alone from here
    import M( FInt )
because we don't have syntax to support that.  (It looks like an import of
the type FInt.)

At one point I implemented a compromise:
  * When constructing exports with no export list, or with module M(
    module M ), we add the parent to the exports as well.
  * But not when you see module M( f ), even if f is a
    class method with a parent.
  * Nor when you see module M( module N ), with N /= M.

But the compromise seemed too much of a hack, so we backed it out.
You just have to use an explicit export list:
    module M( F(..) ) where ...

\begin{code}
type ExportAccum        -- The type of the accumulating parameter of
                        -- the main worker function in rnExports
     = ([LIE Name],             -- Export items with Names
        ExportOccMap,           -- Tracks exported occurrence names
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
 = unsetWOptM Opt_WarnWarningsDeprecations $
       -- Do not report deprecations arising from the export
       -- list, to avoid bleating about re-exporting a deprecated
       -- thing (especially via 'module Foo' export item)
   do   {
        -- If the module header is omitted altogether, then behave
        -- as if the user had written "module Main(main) where..."
        -- EXCEPT in interactive mode, when we behave as if he had
        -- written "module Main where ..."
        -- Reason: don't want to complain about 'main' not in scope
        --         in interactive mode
        ; dflags <- getDynFlags
        ; let real_exports
                 | explicit_mod = exports
                 | ghcLink dflags == LinkInMemory = Nothing
                 | otherwise = Just [noLoc (IEVar main_RDR_Unqual)]
                        -- ToDo: the 'noLoc' here is unhelpful if 'main'
                        --       turns out to be out of scope

        ; (rn_exports, avails) <- exports_from_avail real_exports rdr_env imports this_mod
        ; let final_avails = nubAvails avails    -- Combine families

        ; traceRn (text "rnExports: Exports:" <+> ppr final_avails)

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
       avails = [ greExportAvail gre
                | gre <- globalRdrEnvElts rdr_env
                , isLocalGRE gre ]
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
                         (qual_name, _, _, _) <- xs ]

    exports_from_item :: ExportAccum -> LIE RdrName -> RnM ExportAccum
    exports_from_item acc@(ie_names, occs, exports)
                      (L loc (IEModuleContents mod))
        | let earlier_mods = [ mod | (L _ (IEModuleContents mod)) <- ie_names ]
        , mod `elem` earlier_mods    -- Duplicate export of M
        = do { warn_dup_exports <- woptM Opt_WarnDuplicateExports ;
               warnIf warn_dup_exports (dupModuleExport mod) ;
               return acc }

        | otherwise
        = do { implicit_prelude <- xoptM Opt_ImplicitPrelude
             ; warnDodgyExports <- woptM Opt_WarnDodgyExports
             ; let { exportValid = (mod `elem` imported_modules)
                                || (moduleName this_mod == mod)
                   ; gres = filter (isModuleExported implicit_prelude mod)
                                   (globalRdrEnvElts rdr_env)
                   ; new_exports = map greExportAvail gres
                   ; names       = map gre_name gres }

             ; checkErr exportValid (moduleNotImported mod)
             ; warnIf (warnDodgyExports && exportValid && null names)
                      (nullModuleExport mod)

             ; addUsedRdrNames (concat [ [mkRdrQual mod occ, mkRdrUnqual occ]
                                       | occ <- map nameOccName names ])
                        -- The qualified and unqualified version of all of
                        -- these names are, in effect, used by this export

             ; occs' <- check_occs (IEModuleContents mod) occs names
                      -- This check_occs not only finds conflicts
                      -- between this item and others, but also
                      -- internally within this item.  That is, if
                      -- 'M.x' is in scope in several ways, we'll have
                      -- several members of mod_avails with the same
                      -- OccName.
             ; traceRn (vcat [ text "export mod" <+> ppr mod
                             , ppr new_exports ])
             ; return (L loc (IEModuleContents mod) : ie_names,
                       occs', new_exports ++ exports) }

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
             return (IEVar (gre_name gre), greExportAvail gre)

    lookup_ie (IEThingAbs rdr)
        = do gre <- lookupGreRn rdr
             let name = gre_name gre
                 avail = greExportAvail gre
             return (IEThingAbs name, avail)

    lookup_ie ie@(IEThingAll rdr)
        = do name <- lookupGlobalOccRn rdr
             let kids = findChildren kids_env name
             addUsedKids rdr kids
             warnDodgyExports <- woptM Opt_WarnDodgyExports
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
             let mb_names = lookupChildren (findChildren kids_env name) sub_rdrs
             if any isNothing mb_names
                then do addErr (exportItemErr ie)
                        return (IEThingWith name [], AvailTC name [name])
                else do let names = catMaybes mb_names
                        addUsedKids rdr names
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

    -- In an export item M.T(A,B,C), we want to treat the uses of
    -- A,B,C as if they were M.A, M.B, M.C
    addUsedKids parent_rdr kid_names
       = addUsedRdrNames $ map (mk_kid_rdr . nameOccName) kid_names
       where
         mk_kid_rdr = case isQual_maybe parent_rdr of
                         Nothing           -> mkRdrUnqual
                         Just (modName, _) -> mkRdrQual modName

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
check_occs ie occs names  -- 'names' are the entities specifed by 'ie'
  = foldlM check occs names
  where
    check occs name
      = case lookupOccEnv occs name_occ of
          Nothing -> return (extendOccEnv occs name_occ (name, ie))

          Just (name', ie')
            | name == name'   -- Duplicate export
            -- But we don't want to warn if the same thing is exported
            -- by two different module exports. See ticket #4478.
            -> do unless (dupExport_ok name ie ie') $ do
                      warn_dup_exports <- woptM Opt_WarnDuplicateExports
                      warnIf warn_dup_exports (dupExportWarn name_occ ie ie')
                  return occs

            | otherwise    -- Same occ name but different names: an error
            ->  do { global_env <- getGlobalRdrEnv ;
                     addErr (exportClashErr global_env name' name ie' ie) ;
                     return occs }
      where
        name_occ = nameOccName name


dupExport_ok :: Name -> IE RdrName -> IE RdrName -> Bool
-- The Name is exported by both IEs. Is that ok?
-- "No"  iff the name is mentioned explicitly in both IEs
--        or one of the IEs mentions the name *alone*
-- "Yes" otherwise
--
-- Examples of "no":  module M( f, f )
--                    module M( fmap, Functor(..) )
--                    module M( module Data.List, head )
--
-- Example of "yes"
--    module M( module A, module B ) where
--        import A( f )
--        import B( f )
--
-- Example of "yes" (Trac #2436)
--    module M( C(..), T(..) ) where
--         class C a where { data T a }
--         instace C Int where { data T Int = TInt }
--
-- Example of "yes" (Trac #2436)
--    module Foo ( T ) where
--      data family T a
--    module Bar ( T(..), module Foo ) where
--        import Foo
--        data instance T Int = TInt

dupExport_ok n ie1 ie2
  = not (  single ie1 || single ie2
        || (explicit_in ie1 && explicit_in ie2) )
  where
    explicit_in (IEModuleContents _) = False                -- module M
    explicit_in (IEThingAll r) = nameOccName n == rdrNameOcc r  -- T(..)
    explicit_in _              = True

    single (IEVar {})      = True
    single (IEThingAbs {}) = True
    single _               = False
\end{code}


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
  http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/UnusedImports

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
       ; let imports = filterOut un_warnable_import (tcg_rn_imports gbl_env)
             rdr_env = tcg_rdr_env gbl_env

       ; let usage :: [ImportDeclUsage]
             usage = findImportUsage imports rdr_env (Set.elems uses)

       ; traceRn (vcat [ ptext (sLit "Uses:") <+> ppr (Set.elems uses)
                       , ptext (sLit "Import usage") <+> ppr usage])
       ; whenWOptM Opt_WarnUnusedImports $
         mapM_ warnUnusedImport usage

       ; whenGOptM Opt_D_dump_minimal_imports $
         printMinimalImports usage }
  where
    un_warnable_import (L _ decl)  -- See Note [Un-warnable import decls]
       | ideclImplicit decl
       = True
       | Just (True, hides) <- ideclHiding decl
       , not (null hides)
       , pRELUDE_NAME == unLoc (ideclName decl)
       = True
       | otherwise
       = False
\end{code}

Note [Un-warnable import decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not warn about the implicit import of Prelude, since the user can't remove it

We do not warn about
   import Prelude hiding( x, y )
because even if nothing else from Prelude is used, it may be essential to hide
x,y to avoid name-shadowing warnings.  Example (Trac #9061)
   import Prelude hiding( log )
   f x = log where log = ()

Note [The ImportMap]
~~~~~~~~~~~~~~~~~~~~
The ImportMap is a short-lived intermediate data struture records, for
each import declaration, what stuff brought into scope by that
declaration is actually used in the module.

The SrcLoc is the location of the END of a particular 'import'
declaration.  Why *END*?  Because we don't want to get confused
by the implicit Prelude import. Consider (Trac #7476) the module
    import Foo( foo )
    main = print foo
There is an implicit 'import Prelude(print)', and it gets a SrcSpan
of line 1:1 (just the point, not a span). If we use the *START* of
the SrcSpan to identify the import decl, we'll confuse the implicit
import Prelude with the explicit 'import Foo'.  So we use the END.
It's just a cheap hack; we could equally well use the Span too.

The AvailInfos are the things imported from that decl (just a list,
not normalised).

\begin{code}
type ImportMap = Map SrcLoc [AvailInfo]  -- See [The ImportMap]

findImportUsage :: [LImportDecl Name]
                -> GlobalRdrEnv
                -> [RdrName]
                -> [ImportDeclUsage]

findImportUsage imports rdr_env rdrs
  = map unused_decl imports
  where
    import_usage :: ImportMap
    import_usage = foldr (extendImportMap rdr_env) Map.empty rdrs

    unused_decl decl@(L loc (ImportDecl { ideclHiding = imps }))
      = (decl, nubAvails used_avails, nameSetToList unused_imps)
      where
        used_avails = Map.lookup (srcSpanEnd loc) import_usage `orElse` []
                      -- srcSpanEnd: see Note [The ImportMap]
        used_names   = availsToNameSet used_avails
        used_parents = mkNameSet [n | AvailTC n _ <- used_avails]

        unused_imps   -- Not trivial; see eg Trac #7454
          = case imps of
              Just (False, imp_ies) -> foldr (add_unused . unLoc) emptyNameSet imp_ies
              _other -> emptyNameSet -- No explicit import list => no unused-name list

        add_unused :: IE Name -> NameSet -> NameSet
        add_unused (IEVar n)          acc = add_unused_name n acc
        add_unused (IEThingAbs n)     acc = add_unused_name n acc
        add_unused (IEThingAll n)     acc = add_unused_all  n acc
        add_unused (IEThingWith p ns) acc = add_unused_with p ns acc
        add_unused _                  acc = acc

        add_unused_name n acc
          | n `elemNameSet` used_names = acc
          | otherwise                  = acc `addOneToNameSet` n
        add_unused_all n acc
          | n `elemNameSet` used_names   = acc
          | n `elemNameSet` used_parents = acc
          | otherwise                    = acc `addOneToNameSet` n
        add_unused_with p ns acc
          | all (`elemNameSet` acc1) ns = add_unused_name p acc1
          | otherwise = acc1
          where
            acc1 = foldr add_unused_name acc ns
       -- If you use 'signum' from Num, then the user may well have
       -- imported Num(signum).  We don't want to complain that
       -- Num is not itself mentioned.  Hence the two cases in add_unused_with.


extendImportMap :: GlobalRdrEnv -> RdrName -> ImportMap -> ImportMap
-- For a used RdrName, find all the import decls that brought
-- it into scope; choose one of them (bestImport), and record
-- the RdrName in that import decl's entry in the ImportMap
extendImportMap rdr_env rdr imp_map
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
        decl_loc = srcSpanEnd (is_dloc imp_decl_spec)
                   -- For srcSpanEnd see Note [The ImportMap]
        avail    = greExportAvail gre

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
    pp_herald  = text "The" <+> pp_qual <+> text "import of"
    pp_qual
      | ideclQualified decl = text "qualified"
      | otherwise           = empty
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
       ; dflags   <- getDynFlags
       ; liftIO $
         do { h <- openFile (mkFilename dflags this_mod) WriteMode
            ; printForUser dflags h neverQualify (vcat (map ppr imports')) }
              -- The neverQualify is important.  We are printing Names
              -- but they are in the context of an 'import' decl, and
              -- we never qualify things inside there
              -- E.g.   import Blag( f, b )
              -- not    import Blag( Blag.f, Blag.g )!
       }
  where
    mkFilename dflags this_mod
      | Just d <- dumpDir dflags = d </> basefn
      | otherwise                = basefn
      where
        basefn = moduleNameString (moduleName this_mod) ++ ".imports"

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
      = case [xs | AvailTC x xs <- mi_exports iface
                 , x == n
                 , x `elem` xs    -- Note [Partial export]
                 ] of
           [xs] | all_used xs -> [IEThingAll n]
                | otherwise   -> [IEThingWith n (filter (/= n) ns)]
           _other             -> map IEVar ns
        where
          all_used avail_occs = all (`elem` ns) avail_occs
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
         , nest 2 $ quotes (ptext (sLit "import"))
             <+> ppr (is_mod decl_spec)
             <> parens_sp (ppr dataType <> parens_sp datacon)
         , ptext (sLit "or")
         , nest 2 $ quotes (ptext (sLit "import"))
             <+> ppr (is_mod decl_spec)
             <> parens_sp (ppr dataType <> ptext (sLit "(..)"))
         ]
  where
    datacon_occ = rdrNameOcc $ ieName ie
    datacon = parenSymOcc datacon_occ (ppr datacon_occ)
    source_import | mi_boot iface = ptext (sLit "(hi-boot interface)")
                  | otherwise     = empty
    parens_sp d = parens (space <> d <> space)  -- T( f,g )

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

dodgyMsg :: (OutputableBndr n, HasOccName n) => SDoc -> n -> SDoc
dodgyMsg kind tc
  = sep [ ptext (sLit "The") <+> kind <+> ptext (sLit "item") <+> quotes (ppr (IEThingAll tc))
                <+> ptext (sLit "suggests that"),
          quotes (ppr tc) <+> ptext (sLit "has (in-scope) constructors or class methods,"),
          ptext (sLit "but it has none") ]

exportItemErr :: IE RdrName -> SDoc
exportItemErr export_item
  = sep [ ptext (sLit "The export item") <+> quotes (ppr export_item),
          ptext (sLit "attempts to export constructors or class methods that are not visible here") ]

exportClashErr :: GlobalRdrEnv -> Name -> Name -> IE RdrName -> IE RdrName
               -> MsgDoc
exportClashErr global_env name1 name2 ie1 ie2
  = vcat [ ptext (sLit "Conflicting exports for") <+> quotes (ppr occ) <> colon
         , ppr_export ie1' name1'
         , ppr_export ie2' name2' ]
  where
    occ = nameOccName name1
    ppr_export ie name = nest 3 (hang (quotes (ppr ie) <+> ptext (sLit "exports") <+>
                                       quotes (ppr name))
                                    2 (pprNameProvenance (get_gre name)))

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
    vcat [ptext (sLit "Multiple declarations of") <+>
             quotes (ppr (nameOccName name)),
             -- NB. print the OccName, not the Name, because the
             -- latter might not be in scope in the RdrEnv and so will
             -- be printed qualified.
          ptext (sLit "Declared at:") <+>
                   vcat (map (ppr . nameSrcLoc) sorted_names)]
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

packageImportErr :: SDoc
packageImportErr
  = ptext (sLit "Package-qualified imports are not enabled; use PackageImports")

-- This data decl will parse OK
--      data T = a Int
-- treating "a" as the constructor.
-- It is really hard to make the parser spot this malformation.
-- So the renamer has to check that the constructor is legal
--
-- We can get an operator as the constructor, even in the prefix form:
--      data T = :% Int Int
-- from interface files, which always print in prefix form

checkConName :: RdrName -> TcRn ()
checkConName name = checkErr (isRdrDataCon name) (badDataCon name)

badDataCon :: RdrName -> SDoc
badDataCon name
   = hsep [ptext (sLit "Illegal data constructor name"), quotes (ppr name)]
\end{code}
