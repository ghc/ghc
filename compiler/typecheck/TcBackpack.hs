{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module TcBackpack (
    findExtraSigImports',
    findExtraSigImports,
    implicitRequirements',
    implicitRequirements,
    checkUnitId,
    tcRnCheckUnitId,
    tcRnMergeSignatures,
    mergeSignatures,
    tcRnInstantiateSignature,
    instantiateSignature,
) where

import BasicTypes (defaultFixity)
import Packages
import TcRnExports
import DynFlags
import HsSyn
import RdrName
import TcRnMonad
import TcTyDecls
import InstEnv
import FamInstEnv
import Inst
import TcIface
import TcMType
import TcType
import TcSimplify
import LoadIface
import RnNames
import ErrUtils
import Id
import Module
import Name
import NameEnv
import NameSet
import Avail
import SrcLoc
import HscTypes
import Outputable
import Type
import FastString
import RnFixity ( lookupFixityRn )
import Maybes
import TcEnv
import Var
import IfaceSyn
import PrelNames
import qualified Data.Map as Map

import Finder
import UniqDSet
import NameShape
import TcErrors
import TcUnify
import RnModIface
import Util

import Control.Monad
import Data.List (find, foldl')

import {-# SOURCE #-} TcRnDriver

#include "HsVersions.h"

fixityMisMatch :: TyThing -> Fixity -> Fixity -> SDoc
fixityMisMatch real_thing real_fixity sig_fixity =
    vcat [ppr real_thing <+> text "has conflicting fixities in the module",
          text "and its hsig file",
          text "Main module:" <+> ppr_fix real_fixity,
          text "Hsig file:" <+> ppr_fix sig_fixity]
  where
    ppr_fix f =
        ppr f <+>
        (if f == defaultFixity
            then parens (text "default")
            else empty)

checkHsigDeclM :: ModIface -> TyThing -> TyThing -> TcRn ()
checkHsigDeclM sig_iface sig_thing real_thing = do
    let name = getName real_thing
    -- TODO: Distinguish between signature merging and signature
    -- implementation cases.
    checkBootDeclM False sig_thing real_thing
    real_fixity <- lookupFixityRn name
    let sig_fixity = case mi_fix_fn sig_iface (occName name) of
                        Nothing -> defaultFixity
                        Just f -> f
    when (real_fixity /= sig_fixity) $
      addErrAt (nameSrcSpan name)
        (fixityMisMatch real_thing real_fixity sig_fixity)

-- | Given a 'ModDetails' of an instantiated signature (note that the
-- 'ModDetails' must be knot-tied consistently with the actual implementation)
-- and a 'GlobalRdrEnv' constructed from the implementor of this interface,
-- verify that the actual implementation actually matches the original
-- interface.
--
-- Note that it is already assumed that the implementation *exports*
-- a sufficient set of entities, since otherwise the renaming and then
-- typechecking of the signature 'ModIface' would have failed.
checkHsigIface :: TcGblEnv -> GlobalRdrEnv -> ModIface -> ModDetails -> TcRn ()
checkHsigIface tcg_env gr sig_iface
  ModDetails { md_insts = sig_insts, md_fam_insts = sig_fam_insts,
               md_types = sig_type_env, md_exports = sig_exports   } = do
    traceTc "checkHsigIface" $ vcat
        [ ppr sig_type_env, ppr sig_insts, ppr sig_exports ]
    mapM_ check_export (map availName sig_exports)
    unless (null sig_fam_insts) $
        panic ("TcRnDriver.checkHsigIface: Cannot handle family " ++
               "instances in hsig files yet...")
    -- Delete instances so we don't look them up when
    -- checking instance satisfiability
    -- TODO: this should not be necessary
    tcg_env <- getGblEnv
    setGblEnv tcg_env { tcg_inst_env = emptyInstEnv,
                        tcg_fam_inst_env = emptyFamInstEnv,
                        tcg_insts = [],
                        tcg_fam_insts = [] } $ do
    mapM_ check_inst sig_insts
    failIfErrsM
  where
    -- NB: the Names in sig_type_env are bogus.  Let's say we have H.hsig
    -- in package p that defines T; and we implement with himpl:H.  Then the
    -- Name is p[himpl:H]:H.T, NOT himplH:H.T.  That's OK but we just
    -- have to look up the right name.
    sig_type_occ_env = mkOccEnv
                     . map (\t -> (nameOccName (getName t), t))
                     $ nameEnvElts sig_type_env
    dfun_names = map getName sig_insts
    check_export name
      -- Skip instances, we'll check them later
      -- TODO: Actually this should never happen, because DFuns are
      -- never exported...
      | name `elem` dfun_names = return ()
      -- See if we can find the type directly in the hsig ModDetails
      -- TODO: need to special case wired in names
      | Just sig_thing <- lookupOccEnv sig_type_occ_env (nameOccName name) = do
        -- NB: We use tcLookupImported_maybe because we want to EXCLUDE
        -- tcg_env (TODO: but maybe this isn't relevant anymore).
        r <- tcLookupImported_maybe name
        case r of
          Failed err -> addErr err
          Succeeded real_thing -> checkHsigDeclM sig_iface sig_thing real_thing

      -- The hsig did NOT define this function; that means it must
      -- be a reexport.  In this case, make sure the 'Name' of the
      -- reexport matches the 'Name exported here.
      | [GRE { gre_name = name' }] <- lookupGlobalRdrEnv gr (nameOccName name) =
        when (name /= name') $ do
            -- See Note [Error reporting bad reexport]
            -- TODO: Actually this error swizzle doesn't work
            let p (L _ ie) = name `elem` ieNames ie
                loc = case tcg_rn_exports tcg_env of
                       Just es | Just e <- find p es
                         -- TODO: maybe we can be a little more
                         -- precise here and use the Located
                         -- info for the *specific* name we matched.
                         -> getLoc e
                       _ -> nameSrcSpan name
            dflags <- getDynFlags
            addErrAt loc
                (badReexportedBootThing dflags False name name')
      -- This should actually never happen, but whatever...
      | otherwise =
        addErrAt (nameSrcSpan name)
            (missingBootThing False name "exported by")

-- Note [Error reporting bad reexport]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- NB: You want to be a bit careful about what location you report on reexports.
-- If the name was declared in the hsig file, 'nameSrcSpan name' is indeed the
-- correct source location.  However, if it was *reexported*, obviously the name
-- is not going to have the right location.  In this case, we need to grovel in
-- tcg_rn_exports to figure out where the reexport came from.



-- | Checks if a 'ClsInst' is "defined". In general, for hsig files we can't
-- assume that the implementing file actually implemented the instances (they
-- may be reexported from elsewhere).  Where should we look for the instances?
-- We do the same as we would otherwise: consult the EPS.  This isn't perfect
-- (we might conclude the module exports an instance when it doesn't, see
-- #9422), but we will never refuse to compile something.
check_inst :: ClsInst -> TcM ()
check_inst sig_inst = do
    -- TODO: This could be very well generalized to support instance
    -- declarations in boot files.
    tcg_env <- getGblEnv
    -- NB: Have to tug on the interface, not necessarily
    -- tugged... but it didn't work?
    mapM_ tcLookupImported_maybe (nameSetElemsStable (orphNamesOfClsInst sig_inst))
    -- Based off of 'simplifyDeriv'
    let ty = idType (instanceDFunId sig_inst)
        skol_info = InstSkol
        -- Based off of tcSplitDFunTy
        (tvs, theta, pred) =
           case tcSplitForAllTys ty of { (tvs, rho)   ->
           case splitFunTys rho     of { (theta, pred) ->
           (tvs, theta, pred) }}
        origin = InstProvidedOrigin (tcg_semantic_mod tcg_env) sig_inst
    (skol_subst, tvs_skols) <- tcInstSkolTyVars tvs -- Skolemize
    (cts, tclvl) <- pushTcLevelM $ do
       wanted <- newWanted origin
                           (Just TypeLevel)
                           (substTy skol_subst pred)
       givens <- forM theta $ \given -> do
           loc <- getCtLocM origin (Just TypeLevel)
           let given_pred = substTy skol_subst given
           new_ev <- newEvVar given_pred
           return CtGiven { ctev_pred = given_pred
                          -- Doesn't matter, make something up
                          , ctev_evar = new_ev
                          , ctev_loc = loc
                          }
       return $ wanted : givens
    unsolved <- simplifyWantedsTcM cts

    (implic, _) <- buildImplicationFor tclvl skol_info tvs_skols [] unsolved
    reportAllUnsolved (mkImplicWC implic)

-- | Return this list of requirement interfaces that need to be merged
-- to form @mod_name@, or @[]@ if this is not a requirement.
requirementMerges :: DynFlags -> ModuleName -> [IndefModule]
requirementMerges dflags mod_name =
    fromMaybe [] (Map.lookup mod_name (requirementContext (pkgState dflags)))

-- | For a module @modname@ of type 'HscSource', determine the list
-- of extra "imports" of other requirements which should be considered part of
-- the import of the requirement, because it transitively depends on those
-- requirements by imports of modules from other packages.  The situation
-- is something like this:
--
--      unit p where
--          signature A
--          signature B
--              import A
--
--      unit q where
--          dependency p[A=<A>,B=<B>]
--          signature A
--          signature B
--
-- Although q's B does not directly import A, we still have to make sure we
-- process A first, because the merging process will cause B to indirectly
-- import A.  This function finds the TRANSITIVE closure of all such imports
-- we need to make.
findExtraSigImports' :: HscEnv
                     -> HscSource
                     -> ModuleName
                     -> IO (UniqDSet ModuleName)
findExtraSigImports' hsc_env HsigFile modname =
    fmap unionManyUniqDSets (forM reqs $ \(IndefModule iuid mod_name) ->
        (initIfaceLoad hsc_env
            . withException
            $ moduleFreeHolesPrecise (text "findExtraSigImports")
                (mkModule (IndefiniteUnitId iuid) mod_name)))
  where
    reqs = requirementMerges (hsc_dflags hsc_env) modname

findExtraSigImports' _ _ _ = return emptyUniqDSet

-- | 'findExtraSigImports', but in a convenient form for "GhcMake" and
-- "TcRnDriver".
findExtraSigImports :: HscEnv -> HscSource -> ModuleName
                    -> IO [(Maybe FastString, Located ModuleName)]
findExtraSigImports hsc_env hsc_src modname = do
    extra_requirements <- findExtraSigImports' hsc_env hsc_src modname
    return [ (Nothing, noLoc mod_name)
           | mod_name <- uniqDSetToList extra_requirements ]

-- A version of 'implicitRequirements'' which is more friendly
-- for "GhcMake" and "TcRnDriver".
implicitRequirements :: HscEnv
                     -> [(Maybe FastString, Located ModuleName)]
                     -> IO [(Maybe FastString, Located ModuleName)]
implicitRequirements hsc_env normal_imports
  = do mns <- implicitRequirements' hsc_env normal_imports
       return [ (Nothing, noLoc mn) | mn <- mns ]

-- Given a list of 'import M' statements in a module, figure out
-- any extra implicit requirement imports they may have.  For
-- example, if they 'import M' and M resolves to p[A=<B>], then
-- they actually also import the local requirement B.
implicitRequirements' :: HscEnv
                     -> [(Maybe FastString, Located ModuleName)]
                     -> IO [ModuleName]
implicitRequirements' hsc_env normal_imports
  = fmap concat $
    forM normal_imports $ \(mb_pkg, L _ imp) -> do
        found <- findImportedModule hsc_env imp mb_pkg
        case found of
            Found _ mod | thisPackage dflags /= moduleUnitId mod ->
                return (uniqDSetToList (moduleFreeHoles mod))
            _ -> return []
  where dflags = hsc_dflags hsc_env

-- | Given a 'UnitId', make sure it is well typed.  This is because
-- unit IDs come from Cabal, which does not know if things are well-typed or
-- not; a component may have been filled with implementations for the holes
-- that don't actually fulfill the requirements.
--
-- INVARIANT: the UnitId is NOT a InstalledUnitId
checkUnitId :: UnitId -> TcM ()
checkUnitId uid = do
    case splitUnitIdInsts uid of
      (_, Just indef) ->
        let insts = indefUnitIdInsts indef in
        forM_ insts $ \(mod_name, mod) ->
            -- NB: direct hole instantiations are well-typed by construction
            -- (because we FORCE things to be merged in), so don't check them
            when (not (isHoleModule mod)) $ do
                checkUnitId (moduleUnitId mod)
                _ <- mod `checkImplements` IndefModule indef mod_name
                return ()
      _ -> return () -- if it's hashed, must be well-typed

-- | Top-level driver for signature instantiation (run when compiling
-- an @hsig@ file.)
tcRnCheckUnitId ::
    HscEnv -> UnitId ->
    IO (Messages, Maybe ())
tcRnCheckUnitId hsc_env uid =
   withTiming (pure dflags)
              (text "Check unit id" <+> ppr uid)
              (const ()) $
   initTc hsc_env
          HsigFile -- bogus
          False
          mAIN -- bogus
          (realSrcLocSpan (mkRealSrcLoc (fsLit loc_str) 0 0)) -- bogus
    $ checkUnitId uid
  where
   dflags = hsc_dflags hsc_env
   loc_str = "Command line argument: -unit-id " ++ showSDoc dflags (ppr uid)

-- TODO: Maybe lcl_iface0 should be pre-renamed to the right thing? Unclear...

-- | Top-level driver for signature merging (run after typechecking
-- an @hsig@ file).
tcRnMergeSignatures :: HscEnv -> HsParsedModule -> TcGblEnv {- from local sig -} -> ModIface
                    -> IO (Messages, Maybe TcGblEnv)
tcRnMergeSignatures hsc_env hpm orig_tcg_env iface =
  withTiming (pure dflags)
             (text "Signature merging" <+> brackets (ppr this_mod))
             (const ()) $
  initTc hsc_env HsigFile False this_mod real_loc $
    mergeSignatures hpm orig_tcg_env iface
 where
  dflags   = hsc_dflags hsc_env
  this_mod = mi_module iface
  real_loc = tcg_top_loc orig_tcg_env

thinModIface :: [AvailInfo] -> ModIface -> ModIface
thinModIface avails iface =
    iface {
        mi_exports = avails,
        -- mi_fixities = ...,
        -- mi_warns = ...,
        -- mi_anns = ...,
        -- TODO: The use of nameOccName here is a bit dodgy, because
        -- perhaps there might be two IfaceTopBndr that are the same
        -- OccName but different Name.  Requires better understanding
        -- of invariants here.
        mi_decls = exported_decls ++ non_exported_decls ++ dfun_decls
        -- mi_insts = ...,
        -- mi_fam_insts = ...,
    }
  where
    decl_pred occs decl = nameOccName (ifName decl) `elemOccSet` occs
    filter_decls occs = filter (decl_pred occs . snd) (mi_decls iface)

    exported_occs = mkOccSet [ occName n
                             | a <- avails
                             , n <- availNames a ]
    exported_decls = filter_decls exported_occs

    non_exported_occs = mkOccSet [ occName n
                                 | (_, d) <- exported_decls
                                 , n <- ifaceDeclNeverExportedRefs d ]
    non_exported_decls = filter_decls non_exported_occs

    dfun_pred IfaceId{ ifIdDetails = IfDFunId } = True
    dfun_pred _ = False
    dfun_decls = filter (dfun_pred . snd) (mi_decls iface)

-- | The list of 'Name's of *non-exported* 'IfaceDecl's which this
-- 'IfaceDecl' may refer to.  A non-exported 'IfaceDecl' should be kept
-- after thinning if an *exported* 'IfaceDecl' (or 'mi_insts', perhaps)
-- refers to it; we can't decide to keep it by looking at the exports
-- of a module after thinning.  Keep this synchronized with
-- 'rnIfaceDecl'.
ifaceDeclNeverExportedRefs :: IfaceDecl -> [Name]
ifaceDeclNeverExportedRefs d@IfaceFamily{} =
    case ifFamFlav d of
        IfaceClosedSynFamilyTyCon (Just (n, _))
            -> [n]
        _   -> []
ifaceDeclNeverExportedRefs _ = []


-- Note [Blank hsigs for all requirements]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- One invariant that a client of GHC must uphold is that there
-- must be an hsig file for every requirement (according to
-- @-this-unit-id@); this ensures that for every interface
-- file (hi), there is a source file (hsig), which helps grease
-- the wheels of recompilation avoidance which assumes that
-- source files always exist.

{-
inheritedSigPvpWarning :: WarningTxt
inheritedSigPvpWarning =
    WarningTxt (noLoc NoSourceText) [noLoc (StringLiteral NoSourceText (fsLit msg))]
  where
    msg = "Inherited requirements from non-signature libraries (libraries " ++
          "with modules) should not be used, as this mode of use is not " ++
          "compatible with PVP-style version bounds.  Instead, copy the " ++
          "declaration to the local hsig file or move the signature to a " ++
          "library of its own and add that library as a dependency."
-}

-- Note [Handling never-exported TyThings under Backpack]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--   DEFINITION: A "never-exported TyThing" is a TyThing whose 'Name' will
--   never be mentioned in the export list of a module (mi_avails).
--   Unlike implicit TyThings (Note [Implicit TyThings]), non-exported
--   TyThings DO have a standalone IfaceDecl declaration in their
--   interface file.
--
-- Originally, Backpack was designed under the assumption that anything
-- you could declare in a module could also be exported; thus, merging
-- the export lists of two signatures is just merging the declarations
-- of two signatures writ small.  Of course, in GHC Haskell, there are a
-- few important things which are not explicitly exported but still can
-- be used:  in particular, dictionary functions for instances, Typeable
-- TyCon bindings, and coercion axioms for type families also count.
--
-- When handling these non-exported things, there two primary things
-- we need to watch out for:
--
--  * Signature matching/merging is done by comparing each
--    of the exported entities of a signature and a module.  These exported
--    entities may refer to non-exported TyThings which must be tested for
--    consistency.  For example, an instance (ClsInst) will refer to a
--    non-exported DFunId.  In this case, 'checkBootDeclM' directly compares the
--    embedded 'DFunId' in 'is_dfun'.
--
--    For this to work at all, we must ensure that pointers in 'is_dfun' refer
--    to DISTINCT 'DFunId's, even though the 'Name's (may) be the same.
--    Unfortunately, this is the OPPOSITE of how we treat most other references
--    to 'Name's, so this case needs to be handled specially.
--
--    The details are in the documentation for 'typecheckIfacesForMerging'.
--    and the Note [Resolving never-exported Names in TcIface].
--
--  * When we rename modules and signatures, we use the export lists to
--    decide how the declarations should be renamed.  However, this
--    means we don't get any guidance for how to rename non-exported
--    entities.  Fortunately, we only need to rename these entities
--    *consistently*, so that 'typecheckIfacesForMerging' can wire them
--    up as needed.
--
--    The details are in Note [rnIfaceNeverExported] in 'RnModIface'.
--
-- The root cause for all of these complications is the fact that these
-- logically "implicit" entities are defined indirectly in an interface
-- file.  #13151 gives a proposal to make these *truly* implicit.

merge_msg :: ModuleName -> [IndefModule] -> SDoc
merge_msg mod_name [] =
    text "while checking the local signature" <+> ppr mod_name <+>
    text "for consistency"
merge_msg mod_name reqs =
  hang (text "while merging the signatures from" <> colon)
   2 (vcat [ bullet <+> ppr req | req <- reqs ] $$
      bullet <+> text "...and the local signature for" <+> ppr mod_name)

-- | Given a local 'ModIface', merge all inherited requirements
-- from 'requirementMerges' into this signature, producing
-- a final 'TcGblEnv' that matches the local signature and
-- all required signatures.
mergeSignatures :: HsParsedModule -> TcGblEnv -> ModIface -> TcRn TcGblEnv
mergeSignatures
  (HsParsedModule { hpm_module = L loc (HsModule { hsmodExports = mb_exports }),
                    hpm_src_files = src_files })
  orig_tcg_env lcl_iface0 = setSrcSpan loc $ do
    -- The lcl_iface0 is the ModIface for the local hsig
    -- file, which is guaranteed to exist, see
    -- Note [Blank hsigs for all requirements]
    hsc_env <- getTopEnv
    dflags  <- getDynFlags

    -- Copy over some things from the original TcGblEnv that
    -- we want to preserve
    updGblEnv (\env -> env {
        -- Renamed imports/declarations are often used
        -- by programs that use the GHC API, e.g., Haddock.
        -- These won't get filled by the merging process (since
        -- we don't actually rename the parsed module again) so
        -- we need to take them directly from the previous
        -- typechecking.
        --
        -- NB: the export declarations aren't in their final
        -- form yet.  We'll fill those in when we reprocess
        -- the export declarations.
        tcg_rn_imports = tcg_rn_imports orig_tcg_env,
        tcg_rn_decls   = tcg_rn_decls   orig_tcg_env,
        -- Annotations
        tcg_ann_env    = tcg_ann_env    orig_tcg_env,
        -- Documentation header
        tcg_doc_hdr    = tcg_doc_hdr orig_tcg_env
        -- tcg_dus?
        -- tcg_th_used           = tcg_th_used orig_tcg_env,
        -- tcg_th_splice_used    = tcg_th_splice_used orig_tcg_env
        -- tcg_th_top_level_locs = tcg_th_top_level_locs orig_tcg_env
       }) $ do
    tcg_env <- getGblEnv

    let outer_mod = tcg_mod tcg_env
        inner_mod = tcg_semantic_mod tcg_env
        mod_name = moduleName (tcg_mod tcg_env)

    -- STEP 1: Figure out all of the external signature interfaces
    -- we are going to merge in.
    let reqs = requirementMerges dflags mod_name

    addErrCtxt (merge_msg mod_name reqs) $ do

    -- STEP 2: Read in the RAW forms of all of these interfaces
    ireq_ifaces0 <- forM reqs $ \(IndefModule iuid mod_name) ->
        let m = mkModule (IndefiniteUnitId iuid) mod_name
            im = fst (splitModuleInsts m)
        in fmap fst
         . withException
         $ findAndReadIface (text "mergeSignatures") im m False

    -- STEP 3: Get the unrenamed exports of all these interfaces,
    -- thin it according to the export list, and do shaping on them.
    let extend_ns nsubst as = liftIO $ extendNameShape hsc_env nsubst as
        -- This function gets run on every inherited interface, and
        -- it's responsible for:
        --
        --  1. Merging the exports of the interface into @nsubst@,
        --  2. Adding these exports to the "OK to import" set (@oks@)
        --  if they came from a package with no exposed modules
        --  (this means we won't report a PVP error in this case), and
        --  3. Thinning the interface according to an explicit export
        --  list.
        --
        gen_subst (nsubst,oks,ifaces) (imod@(IndefModule iuid _), ireq_iface) = do
            let insts = indefUnitIdInsts iuid
                isFromSignaturePackage =
                    let inst_uid = fst (splitUnitIdInsts (IndefiniteUnitId iuid))
                        pkg = getInstalledPackageDetails dflags inst_uid
                    in null (exposedModules pkg)
            -- 3(a). Rename the exports according to how the dependency
            -- was instantiated.  The resulting export list will be accurate
            -- except for exports *from the signature itself* (which may
            -- be subsequently updated by exports from other signatures in
            -- the merge.
            as1 <- tcRnModExports insts ireq_iface
            -- 3(b). Thin the interface if it comes from a signature package.
            (thinned_iface, as2) <- case mb_exports of
                    Just (L loc _)
                      -- Check if the package containing this signature is
                      -- a signature package (i.e., does not expose any
                      -- modules.)  If so, we can thin it.
                      | isFromSignaturePackage
                      -> setSrcSpan loc $ do
                        -- Suppress missing errors; they might be used to refer
                        -- to entities from other signatures we are merging in.
                        -- If an identifier truly doesn't exist in any of the
                        -- signatures that are merged in, we will discover this
                        -- when we run exports_from_avail on the final merged
                        -- export list.
                        (msgs, mb_r) <- tryTc $ do
                            -- Suppose that we have written in a signature:
                            --  signature A ( module A ) where {- empty -}
                            -- If I am also inheriting a signature from a
                            -- signature package, does 'module A' scope over
                            -- all of its exports?
                            --
                            -- There are two possible interpretations:
                            --
                            --  1. For non self-reexports, a module reexport
                            --  is interpreted only in terms of the local
                            --  signature module, and not any of the inherited
                            --  ones.  The reason for this is because after
                            --  typechecking, module exports are completely
                            --  erased from the interface of a file, so we
                            --  have no way of "interpreting" a module reexport.
                            --  Thus, it's only useful for the local signature
                            --  module (where we have a useful GlobalRdrEnv.)
                            --
                            --  2. On the other hand, a common idiom when
                            --  you want to "export everything, plus a reexport"
                            --  in modules is to say module A ( module A, reex ).
                            --  This applies to signature modules too; and in
                            --  particular, you probably still want the entities
                            --  from the inherited signatures to be preserved
                            --  too.
                            --
                            -- We think it's worth making a special case for
                            -- self reexports to make use case (2) work.  To
                            -- do this, we take the exports of the inherited
                            -- signature @as1@, and bundle them into a
                            -- GlobalRdrEnv where we treat them as having come
                            -- from the import @import A@.  Thus, we will
                            -- pick them up if they are referenced explicitly
                            -- (@foo@) or even if we do a module reexport
                            -- (@module A@).
                            let ispec = ImpSpec ImpDeclSpec{
                                            -- NB: This needs to be mod name
                                            -- of the local signature, not
                                            -- the (original) module name of
                                            -- the inherited signature,
                                            -- because we need module
                                            -- LocalSig (from the local
                                            -- export list) to match it!
                                            is_mod  = mod_name,
                                            is_as   = mod_name,
                                            is_qual = False,
                                            is_dloc = loc
                                          } ImpAll
                                rdr_env = mkGlobalRdrEnv (gresFromAvails (Just ispec) as1)
                            setGblEnv tcg_env {
                                tcg_rdr_env = rdr_env
                            } $ exports_from_avail mb_exports rdr_env
                                    -- NB: tcg_imports is also empty!
                                    emptyImportAvails
                                    (tcg_semantic_mod tcg_env)
                        case mb_r of
                            Just (_, as2) -> return (thinModIface as2 ireq_iface, as2)
                            Nothing -> addMessages msgs >> failM
                    -- We can't think signatures from non signature packages
                    _ -> return (ireq_iface, as1)
            -- 3(c). Only identifiers from signature packages are "ok" to
            -- import (that is, they are safe from a PVP perspective.)
            -- (NB: This code is actually dead right now.)
            let oks' | isFromSignaturePackage
                     = extendOccSetList oks (exportOccs as2)
                     | otherwise
                     = oks
            -- 3(d). Extend the name substitution (performing shaping)
            mb_r <- extend_ns nsubst as2
            case mb_r of
                Left err -> failWithTc err
                Right nsubst' -> return (nsubst',oks',(imod, thinned_iface):ifaces)
        nsubst0 = mkNameShape (moduleName inner_mod) (mi_exports lcl_iface0)
        ok_to_use0 = mkOccSet (exportOccs (mi_exports lcl_iface0))
    -- Process each interface, getting the thinned interfaces as well as
    -- the final, full set of exports @nsubst@ and the exports which are
    -- "ok to use" (we won't attach 'inheritedSigPvpWarning' to them.)
    (nsubst, ok_to_use, rev_thinned_ifaces)
        <- foldM gen_subst (nsubst0, ok_to_use0, []) (zip reqs ireq_ifaces0)
    let thinned_ifaces = reverse rev_thinned_ifaces
        exports        = nameShapeExports nsubst
        rdr_env        = mkGlobalRdrEnv (gresFromAvails Nothing exports)
        _warn_occs     = filter (not . (`elemOccSet` ok_to_use)) (exportOccs exports)
        warns          = NoWarnings
        {-
        -- TODO: Warnings are transitive, but this is not what we want here:
        -- if a module reexports an entity from a signature, that should be OK.
        -- Not supported in current warning framework
        warns | null warn_occs = NoWarnings
              | otherwise = WarnSome $ map (\o -> (o, inheritedSigPvpWarning)) warn_occs
        -}
    setGblEnv tcg_env {
        -- The top-level GlobalRdrEnv is quite interesting.  It consists
        -- of two components:
        --  1. First, we reuse the GlobalRdrEnv of the local signature.
        --     This is very useful, because it means that if we have
        --     to print a message involving some entity that the local
        --     signature imported, we'll qualify it accordingly.
        --  2. Second, we need to add all of the declarations we are
        --     going to merge in (as they need to be in scope for the
        --     final test of the export list.)
        tcg_rdr_env = rdr_env `plusGlobalRdrEnv` tcg_rdr_env orig_tcg_env,
        -- Inherit imports from the local signature, so that module
        -- rexports are picked up correctly
        tcg_imports = tcg_imports orig_tcg_env,
        tcg_exports = exports,
        tcg_dus     = usesOnly (availsToNameSetWithSelectors exports),
        tcg_warns   = warns
        } $ do
    tcg_env <- getGblEnv

    -- Make sure we didn't refer to anything that doesn't actually exist
    -- pprTrace "mergeSignatures: exports_from_avail" (ppr exports) $ return ()
    (mb_lies, _) <- exports_from_avail mb_exports rdr_env
                        (tcg_imports tcg_env) (tcg_semantic_mod tcg_env)

    {- -- NB: This is commented out, because warns above is disabled.
    -- If you tried to explicitly export an identifier that has a warning
    -- attached to it, that's probably a mistake.  Warn about it.
    case mb_lies of
      Nothing -> return ()
      Just lies ->
        forM_ (concatMap (\(L loc x) -> map (L loc) (ieNames x)) lies) $ \(L loc n) ->
          setSrcSpan loc $
            unless (nameOccName n `elemOccSet` ok_to_use) $
                addWarn NoReason $ vcat [
                    text "Exported identifier" <+> quotes (ppr n) <+> text "will cause warnings if used.",
                    parens (text "To suppress this warning, remove" <+> quotes (ppr n) <+> text "from the export list of this signature.")
                    ]
    -}

    failIfErrsM

    -- Save the exports
    setGblEnv tcg_env { tcg_rn_exports = mb_lies } $ do
    tcg_env <- getGblEnv

    -- STEP 4: Rename the interfaces
    ext_ifaces <- forM thinned_ifaces $ \((IndefModule iuid _), ireq_iface) ->
        tcRnModIface (indefUnitIdInsts iuid) (Just nsubst) ireq_iface
    lcl_iface <- tcRnModIface (thisUnitIdInsts dflags) (Just nsubst) lcl_iface0
    let ifaces = lcl_iface : ext_ifaces

    -- STEP 4.1: Merge fixities (we'll verify shortly) tcg_fix_env
    let fix_env = mkNameEnv [ (gre_name rdr_elt, FixItem occ f)
                            | (occ, f) <- concatMap mi_fixities ifaces
                            , rdr_elt <- lookupGlobalRdrEnv rdr_env occ ]

    -- STEP 5: Typecheck the interfaces
    let type_env_var = tcg_type_env_var tcg_env

    -- typecheckIfacesForMerging does two things:
    --      1. It merges the all of the ifaces together, and typechecks the
    --      result to type_env.
    --      2. It typechecks each iface individually, but with their 'Name's
    --      resolving to the merged type_env from (1).
    -- See typecheckIfacesForMerging for more details.
    (type_env, detailss) <- initIfaceTcRn $
                            typecheckIfacesForMerging inner_mod ifaces type_env_var
    let infos = zip ifaces detailss

    -- Test for cycles
    checkSynCycles (thisPackage dflags) (typeEnvTyCons type_env) []

    -- NB on type_env: it contains NO dfuns.  DFuns are recorded inside
    -- detailss, and given a Name that doesn't correspond to anything real.  See
    -- also Note [Signature merging DFuns]

    -- Add the merged type_env to TcGblEnv, so that it gets serialized
    -- out when we finally write out the interface.
    --
    -- NB: Why do we set tcg_tcs/tcg_patsyns/tcg_type_env directly,
    -- rather than use tcExtendGlobalEnv (the normal method to add newly
    -- defined types to TcGblEnv?)  tcExtendGlobalEnv adds these
    -- TyThings to 'tcg_type_env_var', which is consulted when
    -- we read in interfaces to tie the knot.  But *these TyThings themselves
    -- come from interface*, so that would result in deadlock.  Don't
    -- update it!
    setGblEnv tcg_env {
        tcg_tcs = typeEnvTyCons type_env,
        tcg_patsyns = typeEnvPatSyns type_env,
        tcg_type_env = type_env,
        tcg_fix_env = fix_env
        } $ do
    tcg_env <- getGblEnv

    -- STEP 6: Check for compatibility/merge things
    tcg_env <- (\x -> foldM x tcg_env infos)
             $ \tcg_env (iface, details) -> do

        let check_export name
              | Just sig_thing <- lookupTypeEnv (md_types details) name
              = case lookupTypeEnv type_env (getName sig_thing) of
                  Just thing -> checkHsigDeclM iface sig_thing thing
                  Nothing -> panic "mergeSignatures: check_export"
              -- Oops! We're looking for this export but it's
              -- not actually in the type environment of the signature's
              -- ModDetails.
              --
              -- NB: This case happens because the we're iterating
              -- over the union of all exports, so some interfaces
              -- won't have everything.  Note that md_exports is nonsense
              -- (it's the same as exports); maybe we should fix this
              -- eventually.
              | otherwise
              = return ()
        mapM_ check_export (map availName exports)

        -- Note [Signature merging instances]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Merge instances into the global environment.  The algorithm here is
        -- dumb and simple: if an instance has exactly the same DFun type
        -- (tested by 'memberInstEnv') as an existing instance, we drop it;
        -- otherwise, we add it even, even if this would cause overlap.
        --
        -- Why don't we deduplicate instances with identical heads?  There's no
        -- good choice if they have premises:
        --
        --      instance K1 a => K (T a)
        --      instance K2 a => K (T a)
        --
        -- Why not eagerly error in this case?  The overlapping head does not
        -- necessarily mean that the instances are unimplementable: in fact,
        -- they may be implemented without overlap (if, for example, the
        -- implementing module has 'instance K (T a)'; both are implemented in
        -- this case.)  The implements test just checks that the wanteds are
        -- derivable assuming the givens.
        --
        -- Still, overlapping instances with hypotheses like above are going
        -- to be a bad deal, because instance resolution when we're typechecking
        -- against the merged signature is going to have a bad time when
        -- there are overlapping heads like this: we never backtrack, so it
        -- may be difficult to see that a wanted is derivable.  For now,
        -- we hope that we get lucky / the overlapping instances never
        -- get used, but it is not a very good situation to be in.
        --
        let merge_inst (insts, inst_env) inst
                | memberInstEnv inst_env inst -- test DFun Type equality
                = (insts, inst_env)
                | otherwise
                -- NB: is_dfun_name inst is still nonsense here,
                -- see Note [Signature merging DFuns]
                = (inst:insts, extendInstEnv inst_env inst)
            (insts, inst_env) = foldl' merge_inst
                                    (tcg_insts tcg_env, tcg_inst_env tcg_env)
                                    (md_insts details)
            -- This is a HACK to prevent calculateAvails from including imp_mod
            -- in the listing.  We don't want it because a module is NOT
            -- supposed to include itself in its dep_orphs/dep_finsts.  See #13214
            iface' = iface { mi_orphan = False, mi_finsts = False }
            avails = plusImportAvails (tcg_imports tcg_env) $
                        calculateAvails dflags iface' False False ImportedBySystem
        return tcg_env {
            tcg_inst_env = inst_env,
            tcg_insts    = insts,
            tcg_imports  = avails,
            tcg_merged   =
                if outer_mod == mi_module iface
                    -- Don't add ourselves!
                    then tcg_merged tcg_env
                    else (mi_module iface, mi_mod_hash iface) : tcg_merged tcg_env
            }

    -- Note [Signature merging DFuns]
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Once we know all of instances which will be defined by this merged
    -- signature, we go through each of the DFuns and rename them with a fresh,
    -- new, unique DFun Name, and add these DFuns to tcg_type_env (thus fixing
    -- up the "bogus" names that were setup in 'typecheckIfacesForMerging'.
    --
    -- We can't do this fixup earlier, because we need a way to identify each
    -- source DFun (from each of the signatures we are merging in) so that
    -- when we have a ClsInst, we can pull up the correct DFun to check if
    -- the types match.
    --
    -- See also Note [rnIfaceNeverExported] in RnModIface
    dfun_insts <- forM (tcg_insts tcg_env) $ \inst -> do
        n <- newDFunName (is_cls inst) (is_tys inst) (nameSrcSpan (is_dfun_name inst))
        let dfun = setVarName (is_dfun inst) n
        return (dfun, inst { is_dfun_name = n, is_dfun = dfun })
    tcg_env <- return tcg_env {
            tcg_insts = map snd dfun_insts,
            tcg_type_env = extendTypeEnvWithIds (tcg_type_env tcg_env) (map fst dfun_insts)
        }

    addDependentFiles src_files

    return tcg_env

-- | Top-level driver for signature instantiation (run when compiling
-- an @hsig@ file.)
tcRnInstantiateSignature ::
    HscEnv -> Module -> RealSrcSpan ->
    IO (Messages, Maybe TcGblEnv)
tcRnInstantiateSignature hsc_env this_mod real_loc =
   withTiming (pure dflags)
              (text "Signature instantiation"<+>brackets (ppr this_mod))
              (const ()) $
   initTc hsc_env HsigFile False this_mod real_loc $ instantiateSignature
  where
   dflags = hsc_dflags hsc_env

exportOccs :: [AvailInfo] -> [OccName]
exportOccs = concatMap (map occName . availNames)

impl_msg :: Module -> IndefModule -> SDoc
impl_msg impl_mod (IndefModule req_uid req_mod_name) =
  text "while checking that" <+> ppr impl_mod <+>
  text "implements signature" <+> ppr req_mod_name <+>
  text "in" <+> ppr req_uid

-- | Check if module implements a signature.  (The signature is
-- always un-hashed, which is why its components are specified
-- explicitly.)
checkImplements :: Module -> IndefModule -> TcRn TcGblEnv
checkImplements impl_mod req_mod@(IndefModule uid mod_name) =
  addErrCtxt (impl_msg impl_mod req_mod) $ do
    let insts = indefUnitIdInsts uid

    -- STEP 1: Load the implementing interface, and make a RdrEnv
    -- for its exports.  Also, add its 'ImportAvails' to 'tcg_imports',
    -- so that we treat all orphan instances it provides as visible
    -- when we verify that all instances are checked (see #12945), and so that
    -- when we eventually write out the interface we record appropriate
    -- dependency information.
    impl_iface <- initIfaceTcRn $
        loadSysInterface (text "checkImplements 1") impl_mod
    let impl_gr = mkGlobalRdrEnv
                    (gresFromAvails Nothing (mi_exports impl_iface))
        nsubst = mkNameShape (moduleName impl_mod) (mi_exports impl_iface)

    -- Load all the orphans, so the subsequent 'checkHsigIface' sees
    -- all the instances it needs to
    loadModuleInterfaces (text "Loading orphan modules (from implementor of hsig)")
                         (dep_orphs (mi_deps impl_iface))

    dflags <- getDynFlags
    let avails = calculateAvails dflags
                    impl_iface False{- safe -} False{- boot -} ImportedBySystem
        fix_env = mkNameEnv [ (gre_name rdr_elt, FixItem occ f)
                            | (occ, f) <- mi_fixities impl_iface
                            , rdr_elt <- lookupGlobalRdrEnv impl_gr occ ]
    updGblEnv (\tcg_env -> tcg_env {
        -- Setting tcg_rdr_env to treat all exported entities from
        -- the implementing module as in scope improves error messages,
        -- as it reduces the amount of qualification we need.  Unfortunately,
        -- we still end up qualifying references to external modules
        -- (see bkpfail07 for an example); we'd need to record more
        -- information in ModIface to solve this.
        tcg_rdr_env = tcg_rdr_env tcg_env `plusGlobalRdrEnv` impl_gr,
        tcg_imports = tcg_imports tcg_env `plusImportAvails` avails,
        -- This is here so that when we call 'lookupFixityRn' for something
        -- directly implemented by the module, we grab the right thing
        tcg_fix_env = fix_env
        }) $ do

    -- STEP 2: Load the *unrenamed, uninstantiated* interface for
    -- the ORIGINAL signature.  We are going to eventually rename it,
    -- but we must proceed slowly, because it is NOT known if the
    -- instantiation is correct.
    let sig_mod = mkModule (IndefiniteUnitId uid) mod_name
        isig_mod = fst (splitModuleInsts sig_mod)
    mb_isig_iface <- findAndReadIface (text "checkImplements 2") isig_mod sig_mod False
    isig_iface <- case mb_isig_iface of
        Succeeded (iface, _) -> return iface
        Failed err -> failWithTc $
            hang (text "Could not find hi interface for signature" <+>
                  quotes (ppr isig_mod) <> colon) 4 err

    -- STEP 3: Check that the implementing interface exports everything
    -- we need.  (Notice we IGNORE the Modules in the AvailInfos.)
    forM_ (exportOccs (mi_exports isig_iface)) $ \occ ->
        case lookupGlobalRdrEnv impl_gr occ of
            [] -> addErr $ quotes (ppr occ)
                    <+> text "is exported by the hsig file, but not"
                    <+> text "exported by the implementing module"
                    <+> quotes (ppr impl_mod)
            _ -> return ()
    failIfErrsM

    -- STEP 4: Now that the export is complete, rename the interface...
    sig_iface <- tcRnModIface insts (Just nsubst) isig_iface

    -- STEP 5: ...and typecheck it.  (Note that in both cases, the nsubst
    -- lets us determine how top-level identifiers should be handled.)
    sig_details <- initIfaceTcRn $ typecheckIfaceForInstantiate nsubst sig_iface

    -- STEP 6: Check that it's sufficient
    tcg_env <- getGblEnv
    checkHsigIface tcg_env impl_gr sig_iface sig_details

    -- STEP 7: Return the updated 'TcGblEnv' with the signature exports,
    -- so we write them out.
    return tcg_env {
        tcg_exports = mi_exports sig_iface
        }

-- | Given 'tcg_mod', instantiate a 'ModIface' from the indefinite
-- library to use the actual implementations of the relevant entities,
-- checking that the implementation matches the signature.
instantiateSignature :: TcRn TcGblEnv
instantiateSignature = do
    tcg_env <- getGblEnv
    dflags <- getDynFlags
    let outer_mod = tcg_mod tcg_env
        inner_mod = tcg_semantic_mod tcg_env
    -- TODO: setup the local RdrEnv so the error messages look a little better.
    -- But this information isn't stored anywhere. Should we RETYPECHECK
    -- the local one just to get the information?  Hmm...
    MASSERT( moduleUnitId outer_mod == thisPackage dflags )
    inner_mod `checkImplements`
        IndefModule
            (newIndefUnitId (thisComponentId dflags)
                            (thisUnitIdInsts dflags))
            (moduleName outer_mod)
