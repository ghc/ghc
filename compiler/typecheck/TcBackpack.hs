{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Packages
import DynFlags
import HsSyn
import RdrName
import TcRnMonad
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
import Maybes
import TcEnv
import Var
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

-- | Given a 'ModDetails' of an instantiated signature (note that the
-- 'ModDetails' must be knot-tied consistently with the actual implementation)
-- and a 'GlobalRdrEnv' constructed from the implementor of this interface,
-- verify that the actual implementation actually matches the original
-- interface.
--
-- Note that it is already assumed that the implementation *exports*
-- a sufficient set of entities, since otherwise the renaming and then
-- typechecking of the signature 'ModIface' would have failed.
checkHsigIface :: TcGblEnv -> GlobalRdrEnv -> ModDetails -> TcRn ()
checkHsigIface tcg_env gr
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
      | name `elem` dfun_names = return ()
      -- See if we can find the type directly in the hsig ModDetails
      -- TODO: need to special case wired in names
      | Just sig_thing <- lookupOccEnv sig_type_occ_env (nameOccName name) = do
        -- NB: We use tcLookupImported_maybe because we want to EXCLUDE
        -- tcg_env (TODO: but maybe this isn't relevant anymore).
        r <- tcLookupImported_maybe name
        case r of
          Failed err -> addErr err
          Succeeded real_thing -> checkBootDeclM False sig_thing real_thing
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
            addErrAt loc
                (badReexportedBootThing False name name')
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
--      package p where
--          signature A
--          signature B
--              import A
--
--      package q where
--          include p
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
                _ <- addErrCtxt (text "while checking that" <+> ppr mod
                        <+> text "implements signature" <+> ppr mod_name <+> text "in"
                        <+> ppr uid) $
                    mod `checkImplements` IndefModule indef mod_name
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
tcRnMergeSignatures :: HscEnv -> RealSrcSpan -> ModIface
                    -> IO (Messages, Maybe TcGblEnv)
tcRnMergeSignatures hsc_env real_loc iface =
  withTiming (pure dflags)
             (text "Signature merging" <+> brackets (ppr this_mod))
             (const ()) $
  initTc hsc_env HsigFile False this_mod real_loc $
    mergeSignatures iface
 where
  dflags   = hsc_dflags hsc_env
  this_mod = mi_module iface

-- Note [Blank hsigs for all requirements]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- One invariant that a client of GHC must uphold is that there
-- must be an hsig file for every requirement (according to
-- @-this-unit-id@); this ensures that for every interface
-- file (hi), there is a source file (hsig), which helps grease
-- the wheels of recompilation avoidance which assumes that
-- source files always exist.

-- | Given a local 'ModIface', merge all inherited requirements
-- from 'requirementMerges' into this signature, producing
-- a final 'TcGblEnv' that matches the local signature and
-- all required signatures.
mergeSignatures :: ModIface -> TcRn TcGblEnv
mergeSignatures lcl_iface0 = do
    -- The lcl_iface0 is the ModIface for the local hsig
    -- file, which is guaranteed to exist, see
    -- Note [Blank hsigs for all requirements]
    hsc_env <- getTopEnv
    dflags  <- getDynFlags
    tcg_env <- getGblEnv
    let outer_mod = tcg_mod tcg_env
        inner_mod = tcg_semantic_mod tcg_env

    -- STEP 1: Figure out all of the external signature interfaces
    -- we are going to merge in.
    let reqs = requirementMerges dflags (moduleName (tcg_mod tcg_env))

    -- STEP 2: Read in the RAW forms of all of these interfaces
    ireq_ifaces <- forM reqs $ \(IndefModule iuid mod_name) ->
           fmap fst
         . withException
         . flip (findAndReadIface (text "mergeSignatures")) False
         $ fst (splitModuleInsts (mkModule (IndefiniteUnitId iuid) mod_name))

    -- STEP 3: Get the unrenamed exports of all these interfaces, and
    -- dO shaping on them.
    let extend_ns nsubst as = liftIO $ extendNameShape hsc_env nsubst as
        gen_subst nsubst ((IndefModule iuid _), ireq_iface) = do
            let insts = indefUnitIdInsts iuid
            as1 <- liftIO $ rnModExports hsc_env insts ireq_iface
            mb_r <- extend_ns nsubst as1
            case mb_r of
                Left err -> failWithTc err
                Right nsubst' -> return nsubst'
        nsubst0 = mkNameShape (moduleName inner_mod) (mi_exports lcl_iface0)
    nsubst <- foldM gen_subst nsubst0 (zip reqs ireq_ifaces)
    let exports = nameShapeExports nsubst
    tcg_env <- return tcg_env {
        tcg_rdr_env = mkGlobalRdrEnv (gresFromAvails Nothing exports),
        tcg_exports = exports,
        tcg_dus     = usesOnly (availsToNameSetWithSelectors exports)
        }

    -- STEP 4: Rename the interfaces
    ext_ifaces <- forM (zip reqs ireq_ifaces) $ \((IndefModule iuid _), ireq_iface) ->
        liftIO (rnModIface hsc_env (indefUnitIdInsts iuid) (Just nsubst) ireq_iface)
    lcl_iface <- liftIO $ rnModIface hsc_env (thisUnitIdInsts dflags) (Just nsubst) lcl_iface0
    let ifaces = lcl_iface : ext_ifaces

    -- STEP 5: Typecheck the interfaces
    let type_env_var = tcg_type_env_var tcg_env
    -- NB: This is a bit tricky.  Ordinarily, the way we would do this is
    -- use tcExtendGlobalEnv to put all of the things that we believe are
    -- going to be "the real TyThings" (type_env) into the type environment, so that
    -- when we typecheck the rest of the interfaces they get knot-tied
    -- to those.  But tcExtendGlobalEnv is a bit too strict, and forces thunks
    -- before they are ready.
    (type_env, detailss) <- initIfaceTcRn $
                            typecheckIfacesForMerging inner_mod ifaces type_env_var
    -- Something very subtle but important about type_env:
    -- it contains NO dfuns.  The dfuns are inside detailss,
    -- and the names are complete nonsense.  We'll unwind this
    -- in the rest of this function.
    let infos = zip ifaces detailss
    -- Make sure we serialize these out!
    setGblEnv tcg_env {
        tcg_tcs = typeEnvTyCons type_env,
        tcg_patsyns = typeEnvPatSyns type_env,
        tcg_type_env = type_env
        } $ do
    tcg_env <- getGblEnv

    -- STEP 6: Check for compatibility/merge things
    tcg_env <- (\x -> foldM x tcg_env infos)
             $ \tcg_env (iface, details) -> do
        let check_ty sig_thing
              -- We'll check these with the parent
              | isImplicitTyThing sig_thing
              = return ()
              -- These aren't in the type environment; checked
              -- when merging instances
              | AnId id <- sig_thing
              , isDFunId id
              = return ()
              | Just thing <- lookupTypeEnv type_env (getName sig_thing)
              = checkBootDeclM False sig_thing thing
              | otherwise
              = panic "mergeSignatures check_ty"
        mapM_ check_ty (typeEnvElts (md_types details))
        -- DFunId
        let merge_inst (insts, inst_env) inst
                -- TODO: It would be good if, when there IS an
                -- existing interface, we check that the types
                -- match up.
                | memberInstEnv inst_env inst
                = (insts, inst_env)
                | otherwise
                = (inst:insts, extendInstEnv inst_env inst)
            (insts, inst_env) = foldl' merge_inst
                                    (tcg_insts tcg_env, tcg_inst_env tcg_env)
                                    (md_insts details)
            avails = plusImportAvails (tcg_imports tcg_env)
                                      (calculateAvails dflags iface False False)
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

    -- Rename and add dfuns to type_env
    dfun_insts <- forM (tcg_insts tcg_env) $ \inst -> do
        n <- newDFunName (is_cls inst) (is_tys inst) (nameSrcSpan (is_dfun_name inst))
        let dfun = setVarName (is_dfun inst) n
        return (dfun, inst { is_dfun_name = n, is_dfun = dfun })
    tcg_env <- return tcg_env {
            tcg_insts = map snd dfun_insts,
            tcg_type_env = extendTypeEnvWithIds (tcg_type_env tcg_env) (map fst dfun_insts)
        }

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

-- | Check if module implements a signature.  (The signature is
-- always un-hashed, which is why its components are specified
-- explicitly.)
checkImplements :: Module -> IndefModule -> TcRn TcGblEnv
checkImplements impl_mod (IndefModule uid mod_name) = do
    let insts = indefUnitIdInsts uid

    -- STEP 1: Load the implementing interface, and make a RdrEnv
    -- for its exports
    impl_iface <- initIfaceTcRn $
        loadSysInterface (text "checkImplements 1") impl_mod
    let impl_gr = mkGlobalRdrEnv
                    (gresFromAvails Nothing (mi_exports impl_iface))
        nsubst = mkNameShape (moduleName impl_mod) (mi_exports impl_iface)

    -- STEP 2: Load the *unrenamed, uninstantiated* interface for
    -- the ORIGINAL signature.  We are going to eventually rename it,
    -- but we must proceed slowly, because it is NOT known if the
    -- instantiation is correct.
    let isig_mod = fst (splitModuleInsts (mkModule (IndefiniteUnitId uid) mod_name))
    mb_isig_iface <- findAndReadIface (text "checkImplements 2") isig_mod False
    isig_iface <- case mb_isig_iface of
        Succeeded (iface, _) -> return iface
        Failed err -> failWithTc $
            hang (text "Could not find hi interface for signature" <+>
                  quotes (ppr isig_mod) <> colon) 4 err

    -- STEP 3: Check that the implementing interface exports everything
    -- we need.  (Notice we IGNORE the Modules in the AvailInfos.)
    forM_ (concatMap (map occName . availNames) (mi_exports isig_iface)) $ \occ ->
        case lookupGlobalRdrEnv impl_gr occ of
            [] -> addErr $ quotes (ppr occ)
                    <+> text "is exported by the hsig file, but not exported the module"
                    <+> quotes (ppr impl_mod)
            _ -> return ()
    failIfErrsM

    -- STEP 4: Now that the export is complete, rename the interface...
    hsc_env <- getTopEnv
    sig_iface <- liftIO $ rnModIface hsc_env insts (Just nsubst) isig_iface

    -- STEP 5: ...and typecheck it.  (Note that in both cases, the nsubst
    -- lets us determine how top-level identifiers should be handled.)
    sig_details <- initIfaceTcRn $ typecheckIfaceForInstantiate nsubst sig_iface

    -- STEP 6: Check that it's sufficient
    tcg_env <- getGblEnv
    checkHsigIface tcg_env impl_gr sig_details

    -- STEP 7: Make sure we have the right exports and imports,
    -- in case we're going to serialize this out (only relevant
    -- if we're actually instantiating).
    dflags <- getDynFlags
    let avails = calculateAvails dflags
                    impl_iface False{- safe -} False{- boot -}
    return tcg_env {
        tcg_exports = mi_exports sig_iface,
        tcg_imports = tcg_imports tcg_env `plusImportAvails` avails
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
