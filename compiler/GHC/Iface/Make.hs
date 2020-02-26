{-
(c) The University of Glasgow 2006-2008
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998
-}

{-# LANGUAGE CPP, NondecreasingIndentation #-}
{-# LANGUAGE MultiWayIf #-}

-- | Module for constructing @ModIface@ values (interface files),
-- writing them to disk and comparing two versions to see if
-- recompilation is required.
module GHC.Iface.Make
   ( mkPartialIface
   , mkFullIface
   , mkIfaceTc
   , mkIfaceExports
   , coAxiomToIfaceDecl
   , tyThingToIfaceDecl -- Converting things to their Iface equivalents
   )
where

#include "HsVersions.h"

import GhcPrelude

import GHC.Iface.Syntax
import GHC.Iface.Recomp
import GHC.Iface.Load
import GHC.CoreToIface

import GHC.HsToCore.Usage ( mkUsageInfo, mkUsedNames, mkDependencies )
import GHC.Types.Id
import GHC.Types.Annotations
import GHC.Core
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.StgToCmm.Types (CgInfos (..))
import TcType
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import TcRnMonad
import GHC.Hs
import GHC.Driver.Types
import GHC.Driver.Session
import GHC.Types.Var.Env
import GHC.Types.Var
import GHC.Types.Name
import GHC.Types.Avail
import GHC.Types.Name.Reader
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Module
import ErrUtils
import Outputable
import GHC.Types.Basic  hiding ( SuccessFlag(..) )
import Util             hiding ( eqListBy )
import FastString
import Maybes
import GHC.HsToCore.Docs

import Data.Function
import Data.List ( findIndex, mapAccumL, sortBy )
import Data.Ord
import Data.IORef
import GHC.Driver.Plugins (LoadedPlugin(..))

{-
************************************************************************
*                                                                      *
\subsection{Completing an interface}
*                                                                      *
************************************************************************
-}

mkPartialIface :: HscEnv
               -> ModDetails
               -> ModGuts
               -> PartialModIface
mkPartialIface hsc_env mod_details
  ModGuts{ mg_module       = this_mod
         , mg_hsc_src      = hsc_src
         , mg_usages       = usages
         , mg_used_th      = used_th
         , mg_deps         = deps
         , mg_rdr_env      = rdr_env
         , mg_fix_env      = fix_env
         , mg_warns        = warns
         , mg_hpc_info     = hpc_info
         , mg_safe_haskell = safe_mode
         , mg_trust_pkg    = self_trust
         , mg_doc_hdr      = doc_hdr
         , mg_decl_docs    = decl_docs
         , mg_arg_docs     = arg_docs
         }
  = mkIface_ hsc_env this_mod hsc_src used_th deps rdr_env fix_env warns hpc_info self_trust
             safe_mode usages doc_hdr decl_docs arg_docs mod_details

-- | Fully instantiate a interface
-- Adds fingerprints and potentially code generator produced information.
mkFullIface :: HscEnv -> PartialModIface -> Maybe CgInfos -> IO ModIface
mkFullIface hsc_env partial_iface mb_cg_infos = do
    let decls
          | gopt Opt_OmitInterfacePragmas (hsc_dflags hsc_env)
          = mi_decls partial_iface
          | otherwise
          = updateDecl (mi_decls partial_iface) mb_cg_infos

    full_iface <-
      {-# SCC "addFingerprints" #-}
      addFingerprints hsc_env partial_iface{ mi_decls = decls }

    -- Debug printing
    dumpIfSet_dyn (hsc_dflags hsc_env) Opt_D_dump_hi "FINAL INTERFACE" FormatText (pprModIface full_iface)

    return full_iface

updateDecl :: [IfaceDecl] -> Maybe CgInfos -> [IfaceDecl]
updateDecl decls Nothing = decls
updateDecl decls (Just CgInfos{ cgNonCafs = non_cafs, cgLFInfos = lf_infos }) = map update_decl decls
  where
    update_decl (IfaceId nm ty details infos)
      | let not_caffy = elemNameSet nm non_cafs
      , let mb_lf_info = lookupNameEnv lf_infos nm
      , WARN( isNothing mb_lf_info, text "Name without LFInfo:" <+> ppr nm ) True
        -- Only allocate a new IfaceId if we're going to update the infos
      , isJust mb_lf_info || not_caffy
      = IfaceId nm ty details $
          (if not_caffy then (HsNoCafRefs :) else id)
          (case mb_lf_info of
             Nothing -> infos
             Just lf_info -> HsLFInfo (toIfaceLFInfo lf_info) : infos)

    update_decl decl
      = decl

-- | Make an interface from the results of typechecking only.  Useful
-- for non-optimising compilation, or where we aren't generating any
-- object code at all ('HscNothing').
mkIfaceTc :: HscEnv
          -> SafeHaskellMode    -- The safe haskell mode
          -> ModDetails         -- gotten from mkBootModDetails, probably
          -> TcGblEnv           -- Usages, deprecations, etc
          -> IO ModIface
mkIfaceTc hsc_env safe_mode mod_details
  tc_result@TcGblEnv{ tcg_mod = this_mod,
                      tcg_src = hsc_src,
                      tcg_imports = imports,
                      tcg_rdr_env = rdr_env,
                      tcg_fix_env = fix_env,
                      tcg_merged = merged,
                      tcg_warns = warns,
                      tcg_hpc = other_hpc_info,
                      tcg_th_splice_used = tc_splice_used,
                      tcg_dependent_files = dependent_files
                    }
  = do
          let used_names = mkUsedNames tc_result
          let pluginModules =
                map lpModule (cachedPlugins (hsc_dflags hsc_env))
          deps <- mkDependencies
                    (thisInstalledUnitId (hsc_dflags hsc_env))
                    (map mi_module pluginModules) tc_result
          let hpc_info = emptyHpcInfo other_hpc_info
          used_th <- readIORef tc_splice_used
          dep_files <- (readIORef dependent_files)
          -- Do NOT use semantic module here; this_mod in mkUsageInfo
          -- is used solely to decide if we should record a dependency
          -- or not.  When we instantiate a signature, the semantic
          -- module is something we want to record dependencies for,
          -- but if you pass that in here, we'll decide it's the local
          -- module and does not need to be recorded as a dependency.
          -- See Note [Identity versus semantic module]
          usages <- mkUsageInfo hsc_env this_mod (imp_mods imports) used_names
                      dep_files merged pluginModules

          let (doc_hdr', doc_map, arg_map) = extractDocs tc_result

          let partial_iface = mkIface_ hsc_env
                   this_mod hsc_src
                   used_th deps rdr_env
                   fix_env warns hpc_info
                   (imp_trust_own_pkg imports) safe_mode usages
                   doc_hdr' doc_map arg_map
                   mod_details

          mkFullIface hsc_env partial_iface Nothing

mkIface_ :: HscEnv -> Module -> HscSource
         -> Bool -> Dependencies -> GlobalRdrEnv
         -> NameEnv FixItem -> Warnings -> HpcInfo
         -> Bool
         -> SafeHaskellMode
         -> [Usage]
         -> Maybe HsDocString
         -> DeclDocMap
         -> ArgDocMap
         -> ModDetails
         -> PartialModIface
mkIface_ hsc_env
         this_mod hsc_src used_th deps rdr_env fix_env src_warns
         hpc_info pkg_trust_req safe_mode usages
         doc_hdr decl_docs arg_docs
         ModDetails{  md_insts     = insts,
                      md_fam_insts = fam_insts,
                      md_rules     = rules,
                      md_anns      = anns,
                      md_types     = type_env,
                      md_exports   = exports,
                      md_complete_sigs = complete_sigs }
-- NB:  notice that mkIface does not look at the bindings
--      only at the TypeEnv.  The previous Tidy phase has
--      put exactly the info into the TypeEnv that we want
--      to expose in the interface

  = do
    let semantic_mod = canonicalizeHomeModule (hsc_dflags hsc_env) (moduleName this_mod)
        entities = typeEnvElts type_env
        decls  = [ tyThingToIfaceDecl entity
                 | entity <- entities,
                   let name = getName entity,
                   not (isImplicitTyThing entity),
                      -- No implicit Ids and class tycons in the interface file
                   not (isWiredInName name),
                      -- Nor wired-in things; the compiler knows about them anyhow
                   nameIsLocalOrFrom semantic_mod name  ]
                      -- Sigh: see Note [Root-main Id] in TcRnDriver
                      -- NB: ABSOLUTELY need to check against semantic_mod,
                      -- because all of the names in an hsig p[H=<H>]:H
                      -- are going to be for <H>, not the former id!
                      -- See Note [Identity versus semantic module]

        fixities    = sortBy (comparing fst)
          [(occ,fix) | FixItem occ fix <- nameEnvElts fix_env]
          -- The order of fixities returned from nameEnvElts is not
          -- deterministic, so we sort by OccName to canonicalize it.
          -- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for more details.
        warns       = src_warns
        iface_rules = map coreRuleToIfaceRule rules
        iface_insts = map instanceToIfaceInst $ fixSafeInstances safe_mode insts
        iface_fam_insts = map famInstToIfaceFamInst fam_insts
        trust_info  = setSafeMode safe_mode
        annotations = map mkIfaceAnnotation anns
        icomplete_sigs = map mkIfaceCompleteSig complete_sigs

    ModIface {
          mi_module      = this_mod,
          -- Need to record this because it depends on the -instantiated-with flag
          -- which could change
          mi_sig_of      = if semantic_mod == this_mod
                            then Nothing
                            else Just semantic_mod,
          mi_hsc_src     = hsc_src,
          mi_deps        = deps,
          mi_usages      = usages,
          mi_exports     = mkIfaceExports exports,

          -- Sort these lexicographically, so that
          -- the result is stable across compilations
          mi_insts       = sortBy cmp_inst     iface_insts,
          mi_fam_insts   = sortBy cmp_fam_inst iface_fam_insts,
          mi_rules       = sortBy cmp_rule     iface_rules,

          mi_fixities    = fixities,
          mi_warns       = warns,
          mi_anns        = annotations,
          mi_globals     = maybeGlobalRdrEnv rdr_env,
          mi_used_th     = used_th,
          mi_decls       = decls,
          mi_hpc         = isHpcUsed hpc_info,
          mi_trust       = trust_info,
          mi_trust_pkg   = pkg_trust_req,
          mi_complete_sigs = icomplete_sigs,
          mi_doc_hdr     = doc_hdr,
          mi_decl_docs   = decl_docs,
          mi_arg_docs    = arg_docs,
          mi_final_exts  = () }
  where
     cmp_rule     = comparing ifRuleName
     -- Compare these lexicographically by OccName, *not* by unique,
     -- because the latter is not stable across compilations:
     cmp_inst     = comparing (nameOccName . ifDFun)
     cmp_fam_inst = comparing (nameOccName . ifFamInstTcName)

     dflags = hsc_dflags hsc_env

     -- We only fill in mi_globals if the module was compiled to byte
     -- code.  Otherwise, the compiler may not have retained all the
     -- top-level bindings and they won't be in the TypeEnv (see
     -- Desugar.addExportFlagsAndRules).  The mi_globals field is used
     -- by GHCi to decide whether the module has its full top-level
     -- scope available. (#5534)
     maybeGlobalRdrEnv :: GlobalRdrEnv -> Maybe GlobalRdrEnv
     maybeGlobalRdrEnv rdr_env
         | targetRetainsAllBindings (hscTarget dflags) = Just rdr_env
         | otherwise                                   = Nothing

     ifFamInstTcName = ifFamInstFam


{-
************************************************************************
*                                                                      *
       COMPLETE Pragmas
*                                                                      *
************************************************************************
-}

mkIfaceCompleteSig :: CompleteMatch -> IfaceCompleteMatch
mkIfaceCompleteSig (CompleteMatch cls tc) = IfaceCompleteMatch cls tc


{-
************************************************************************
*                                                                      *
       Keeping track of what we've slurped, and fingerprints
*                                                                      *
************************************************************************
-}


mkIfaceAnnotation :: Annotation -> IfaceAnnotation
mkIfaceAnnotation (Annotation { ann_target = target, ann_value = payload })
  = IfaceAnnotation {
        ifAnnotatedTarget = fmap nameOccName target,
        ifAnnotatedValue = payload
    }

mkIfaceExports :: [AvailInfo] -> [IfaceExport]  -- Sort to make canonical
mkIfaceExports exports
  = sortBy stableAvailCmp (map sort_subs exports)
  where
    sort_subs :: AvailInfo -> AvailInfo
    sort_subs (Avail n) = Avail n
    sort_subs (AvailTC n [] fs) = AvailTC n [] (sort_flds fs)
    sort_subs (AvailTC n (m:ms) fs)
       | n==m      = AvailTC n (m:sortBy stableNameCmp ms) (sort_flds fs)
       | otherwise = AvailTC n (sortBy stableNameCmp (m:ms)) (sort_flds fs)
       -- Maintain the AvailTC Invariant

    sort_flds = sortBy (stableNameCmp `on` flSelector)

{-
Note [Original module]
~~~~~~~~~~~~~~~~~~~~~
Consider this:
        module X where { data family T }
        module Y( T(..) ) where { import X; data instance T Int = MkT Int }
The exported Avail from Y will look like
        X.T{X.T, Y.MkT}
That is, in Y,
  - only MkT is brought into scope by the data instance;
  - but the parent (used for grouping and naming in T(..) exports) is X.T
  - and in this case we export X.T too

In the result of mkIfaceExports, the names are grouped by defining module,
so we may need to split up a single Avail into multiple ones.

Note [Internal used_names]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Most of the used_names are External Names, but we can have Internal
Names too: see Note [Binders in Template Haskell] in Convert, and
#5362 for an example.  Such Names are always
  - Such Names are always for locally-defined things, for which we
    don't gather usage info, so we can just ignore them in ent_map
  - They are always System Names, hence the assert, just as a double check.

-}


{-
************************************************************************
*                                                                      *
                Converting things to their Iface equivalents
*                                                                      *
************************************************************************
-}

tyThingToIfaceDecl :: TyThing -> IfaceDecl
tyThingToIfaceDecl (AnId id)      = idToIfaceDecl id
tyThingToIfaceDecl (ATyCon tycon) = snd (tyConToIfaceDecl emptyTidyEnv tycon)
tyThingToIfaceDecl (ACoAxiom ax)  = coAxiomToIfaceDecl ax
tyThingToIfaceDecl (AConLike cl)  = case cl of
    RealDataCon dc -> dataConToIfaceDecl dc -- for ppr purposes only
    PatSynCon ps   -> patSynToIfaceDecl ps

--------------------------
idToIfaceDecl :: Id -> IfaceDecl
-- The Id is already tidied, so that locally-bound names
-- (lambdas, for-alls) already have non-clashing OccNames
-- We can't tidy it here, locally, because it may have
-- free variables in its type or IdInfo
idToIfaceDecl id
  = IfaceId { ifName      = getName id,
              ifType      = toIfaceType (idType id),
              ifIdDetails = toIfaceIdDetails (idDetails id),
              ifIdInfo    = toIfaceIdInfo (idInfo id) }

--------------------------
dataConToIfaceDecl :: DataCon -> IfaceDecl
dataConToIfaceDecl dataCon
  = IfaceId { ifName      = getName dataCon,
              ifType      = toIfaceType (dataConUserType dataCon),
              ifIdDetails = IfVanillaId,
              ifIdInfo    = [] }

--------------------------
coAxiomToIfaceDecl :: CoAxiom br -> IfaceDecl
-- We *do* tidy Axioms, because they are not (and cannot
-- conveniently be) built in tidy form
coAxiomToIfaceDecl ax@(CoAxiom { co_ax_tc = tycon, co_ax_branches = branches
                               , co_ax_role = role })
 = IfaceAxiom { ifName       = getName ax
              , ifTyCon      = toIfaceTyCon tycon
              , ifRole       = role
              , ifAxBranches = map (coAxBranchToIfaceBranch tycon
                                     (map coAxBranchLHS branch_list))
                                   branch_list }
 where
   branch_list = fromBranches branches

-- 2nd parameter is the list of branch LHSs, in case of a closed type family,
-- for conversion from incompatible branches to incompatible indices.
-- For an open type family the list should be empty.
-- See Note [Storing compatibility] in GHC.Core.Coercion.Axiom
coAxBranchToIfaceBranch :: TyCon -> [[Type]] -> CoAxBranch -> IfaceAxBranch
coAxBranchToIfaceBranch tc lhs_s
                        (CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                                    , cab_eta_tvs = eta_tvs
                                    , cab_lhs = lhs, cab_roles = roles
                                    , cab_rhs = rhs, cab_incomps = incomps })

  = IfaceAxBranch { ifaxbTyVars  = toIfaceTvBndrs tvs
                  , ifaxbCoVars  = map toIfaceIdBndr cvs
                  , ifaxbEtaTyVars = toIfaceTvBndrs eta_tvs
                  , ifaxbLHS     = toIfaceTcArgs tc lhs
                  , ifaxbRoles   = roles
                  , ifaxbRHS     = toIfaceType rhs
                  , ifaxbIncomps = iface_incomps }
  where
    iface_incomps = map (expectJust "iface_incomps"
                        . flip findIndex lhs_s
                        . eqTypes
                        . coAxBranchLHS) incomps

-----------------
tyConToIfaceDecl :: TidyEnv -> TyCon -> (TidyEnv, IfaceDecl)
-- We *do* tidy TyCons, because they are not (and cannot
-- conveniently be) built in tidy form
-- The returned TidyEnv is the one after tidying the tyConTyVars
tyConToIfaceDecl env tycon
  | Just clas <- tyConClass_maybe tycon
  = classToIfaceDecl env clas

  | Just syn_rhs <- synTyConRhs_maybe tycon
  = ( tc_env1
    , IfaceSynonym { ifName    = getName tycon,
                     ifRoles   = tyConRoles tycon,
                     ifSynRhs  = if_syn_type syn_rhs,
                     ifBinders = if_binders,
                     ifResKind = if_res_kind
                   })

  | Just fam_flav <- famTyConFlav_maybe tycon
  = ( tc_env1
    , IfaceFamily { ifName    = getName tycon,
                    ifResVar  = if_res_var,
                    ifFamFlav = to_if_fam_flav fam_flav,
                    ifBinders = if_binders,
                    ifResKind = if_res_kind,
                    ifFamInj  = tyConInjectivityInfo tycon
                  })

  | isAlgTyCon tycon
  = ( tc_env1
    , IfaceData { ifName    = getName tycon,
                  ifBinders = if_binders,
                  ifResKind = if_res_kind,
                  ifCType   = tyConCType tycon,
                  ifRoles   = tyConRoles tycon,
                  ifCtxt    = tidyToIfaceContext tc_env1 (tyConStupidTheta tycon),
                  ifCons    = ifaceConDecls (algTyConRhs tycon),
                  ifGadtSyntax = isGadtSyntaxTyCon tycon,
                  ifParent  = parent })

  | otherwise  -- FunTyCon, PrimTyCon, promoted TyCon/DataCon
  -- We only convert these TyCons to IfaceTyCons when we are
  -- just about to pretty-print them, not because we are going
  -- to put them into interface files
  = ( env
    , IfaceData { ifName       = getName tycon,
                  ifBinders    = if_binders,
                  ifResKind    = if_res_kind,
                  ifCType      = Nothing,
                  ifRoles      = tyConRoles tycon,
                  ifCtxt       = [],
                  ifCons       = IfDataTyCon [],
                  ifGadtSyntax = False,
                  ifParent     = IfNoParent })
  where
    -- NOTE: Not all TyCons have `tyConTyVars` field. Forcing this when `tycon`
    -- is one of these TyCons (FunTyCon, PrimTyCon, PromotedDataCon) will cause
    -- an error.
    (tc_env1, tc_binders) = tidyTyConBinders env (tyConBinders tycon)
    tc_tyvars      = binderVars tc_binders
    if_binders     = toIfaceTyCoVarBinders tc_binders
                     -- No tidying of the binders; they are already tidy
    if_res_kind    = tidyToIfaceType tc_env1 (tyConResKind tycon)
    if_syn_type ty = tidyToIfaceType tc_env1 ty
    if_res_var     = getOccFS `fmap` tyConFamilyResVar_maybe tycon

    parent = case tyConFamInstSig_maybe tycon of
               Just (tc, ty, ax) -> IfDataInstance (coAxiomName ax)
                                                   (toIfaceTyCon tc)
                                                   (tidyToIfaceTcArgs tc_env1 tc ty)
               Nothing           -> IfNoParent

    to_if_fam_flav OpenSynFamilyTyCon             = IfaceOpenSynFamilyTyCon
    to_if_fam_flav AbstractClosedSynFamilyTyCon   = IfaceAbstractClosedSynFamilyTyCon
    to_if_fam_flav (DataFamilyTyCon {})           = IfaceDataFamilyTyCon
    to_if_fam_flav (BuiltInSynFamTyCon {})        = IfaceBuiltInSynFamTyCon
    to_if_fam_flav (ClosedSynFamilyTyCon Nothing) = IfaceClosedSynFamilyTyCon Nothing
    to_if_fam_flav (ClosedSynFamilyTyCon (Just ax))
      = IfaceClosedSynFamilyTyCon (Just (axn, ibr))
      where defs = fromBranches $ coAxiomBranches ax
            lhss = map coAxBranchLHS defs
            ibr  = map (coAxBranchToIfaceBranch tycon lhss) defs
            axn  = coAxiomName ax

    ifaceConDecls (NewTyCon { data_con = con })    = IfNewTyCon  (ifaceConDecl con)
    ifaceConDecls (DataTyCon { data_cons = cons }) = IfDataTyCon (map ifaceConDecl cons)
    ifaceConDecls (TupleTyCon { data_con = con })  = IfDataTyCon [ifaceConDecl con]
    ifaceConDecls (SumTyCon { data_cons = cons })  = IfDataTyCon (map ifaceConDecl cons)
    ifaceConDecls AbstractTyCon                    = IfAbstractTyCon
        -- The AbstractTyCon case happens when a TyCon has been trimmed
        -- during tidying.
        -- Furthermore, tyThingToIfaceDecl is also used in TcRnDriver
        -- for GHCi, when browsing a module, in which case the
        -- AbstractTyCon and TupleTyCon cases are perfectly sensible.
        -- (Tuple declarations are not serialised into interface files.)

    ifaceConDecl data_con
        = IfCon   { ifConName    = dataConName data_con,
                    ifConInfix   = dataConIsInfix data_con,
                    ifConWrapper = isJust (dataConWrapId_maybe data_con),
                    ifConExTCvs  = map toIfaceBndr ex_tvs',
                    ifConUserTvBinders = map toIfaceForAllBndr user_bndrs',
                    ifConEqSpec  = map (to_eq_spec . eqSpecPair) eq_spec,
                    ifConCtxt    = tidyToIfaceContext con_env2 theta,
                    ifConArgTys  = map (tidyToIfaceType con_env2) arg_tys,
                    ifConFields  = dataConFieldLabels data_con,
                    ifConStricts = map (toIfaceBang con_env2)
                                       (dataConImplBangs data_con),
                    ifConSrcStricts = map toIfaceSrcBang
                                          (dataConSrcBangs data_con)}
        where
          (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _)
            = dataConFullSig data_con
          user_bndrs = dataConUserTyVarBinders data_con

          -- Tidy the univ_tvs of the data constructor to be identical
          -- to the tyConTyVars of the type constructor.  This means
          -- (a) we don't need to redundantly put them into the interface file
          -- (b) when pretty-printing an Iface data declaration in H98-style syntax,
          --     we know that the type variables will line up
          -- The latter (b) is important because we pretty-print type constructors
          -- by converting to Iface syntax and pretty-printing that
          con_env1 = (fst tc_env1, mkVarEnv (zipEqual "ifaceConDecl" univ_tvs tc_tyvars))
                     -- A bit grimy, perhaps, but it's simple!

          (con_env2, ex_tvs') = tidyVarBndrs con_env1 ex_tvs
          user_bndrs' = map (tidyUserTyCoVarBinder con_env2) user_bndrs
          to_eq_spec (tv,ty) = (tidyTyVar con_env2 tv, tidyToIfaceType con_env2 ty)

          -- By this point, we have tidied every universal and existential
          -- tyvar. Because of the dcUserTyCoVarBinders invariant
          -- (see Note [DataCon user type variable binders]), *every*
          -- user-written tyvar must be contained in the substitution that
          -- tidying produced. Therefore, tidying the user-written tyvars is a
          -- simple matter of looking up each variable in the substitution,
          -- which tidyTyCoVarOcc accomplishes.
          tidyUserTyCoVarBinder :: TidyEnv -> TyCoVarBinder -> TyCoVarBinder
          tidyUserTyCoVarBinder env (Bndr tv vis) =
            Bndr (tidyTyCoVarOcc env tv) vis

classToIfaceDecl :: TidyEnv -> Class -> (TidyEnv, IfaceDecl)
classToIfaceDecl env clas
  = ( env1
    , IfaceClass { ifName   = getName tycon,
                   ifRoles  = tyConRoles (classTyCon clas),
                   ifBinders = toIfaceTyCoVarBinders tc_binders,
                   ifBody   = body,
                   ifFDs    = map toIfaceFD clas_fds })
  where
    (_, clas_fds, sc_theta, _, clas_ats, op_stuff)
      = classExtraBigSig clas
    tycon = classTyCon clas

    body | isAbstractTyCon tycon = IfAbstractClass
         | otherwise
         = IfConcreteClass {
                ifClassCtxt   = tidyToIfaceContext env1 sc_theta,
                ifATs    = map toIfaceAT clas_ats,
                ifSigs   = map toIfaceClassOp op_stuff,
                ifMinDef = fmap getOccFS (classMinimalDef clas)
            }

    (env1, tc_binders) = tidyTyConBinders env (tyConBinders tycon)

    toIfaceAT :: ClassATItem -> IfaceAT
    toIfaceAT (ATI tc def)
      = IfaceAT if_decl (fmap (tidyToIfaceType env2 . fst) def)
      where
        (env2, if_decl) = tyConToIfaceDecl env1 tc

    toIfaceClassOp (sel_id, def_meth)
        = ASSERT( sel_tyvars == binderVars tc_binders )
          IfaceClassOp (getName sel_id)
                       (tidyToIfaceType env1 op_ty)
                       (fmap toDmSpec def_meth)
        where
                -- Be careful when splitting the type, because of things
                -- like         class Foo a where
                --                op :: (?x :: String) => a -> a
                -- and          class Baz a where
                --                op :: (Ord a) => a -> a
          (sel_tyvars, rho_ty) = splitForAllTys (idType sel_id)
          op_ty                = funResultTy rho_ty

    toDmSpec :: (Name, DefMethSpec Type) -> DefMethSpec IfaceType
    toDmSpec (_, VanillaDM)       = VanillaDM
    toDmSpec (_, GenericDM dm_ty) = GenericDM (tidyToIfaceType env1 dm_ty)

    toIfaceFD (tvs1, tvs2) = (map (tidyTyVar env1) tvs1
                             ,map (tidyTyVar env1) tvs2)

--------------------------

tidyTyConBinder :: TidyEnv -> TyConBinder -> (TidyEnv, TyConBinder)
-- If the type variable "binder" is in scope, don't re-bind it
-- In a class decl, for example, the ATD binders mention
-- (amd must mention) the class tyvars
tidyTyConBinder env@(_, subst) tvb@(Bndr tv vis)
 = case lookupVarEnv subst tv of
     Just tv' -> (env,  Bndr tv' vis)
     Nothing  -> tidyTyCoVarBinder env tvb

tidyTyConBinders :: TidyEnv -> [TyConBinder] -> (TidyEnv, [TyConBinder])
tidyTyConBinders = mapAccumL tidyTyConBinder

tidyTyVar :: TidyEnv -> TyVar -> FastString
tidyTyVar (_, subst) tv = toIfaceTyVar (lookupVarEnv subst tv `orElse` tv)

--------------------------
instanceToIfaceInst :: ClsInst -> IfaceClsInst
instanceToIfaceInst (ClsInst { is_dfun = dfun_id, is_flag = oflag
                             , is_cls_nm = cls_name, is_cls = cls
                             , is_tcs = mb_tcs
                             , is_orphan = orph })
  = ASSERT( cls_name == className cls )
    IfaceClsInst { ifDFun    = dfun_name,
                ifOFlag   = oflag,
                ifInstCls = cls_name,
                ifInstTys = map do_rough mb_tcs,
                ifInstOrph = orph }
  where
    do_rough Nothing  = Nothing
    do_rough (Just n) = Just (toIfaceTyCon_name n)

    dfun_name = idName dfun_id


--------------------------
famInstToIfaceFamInst :: FamInst -> IfaceFamInst
famInstToIfaceFamInst (FamInst { fi_axiom    = axiom,
                                 fi_fam      = fam,
                                 fi_tcs      = roughs })
  = IfaceFamInst { ifFamInstAxiom    = coAxiomName axiom
                 , ifFamInstFam      = fam
                 , ifFamInstTys      = map do_rough roughs
                 , ifFamInstOrph     = orph }
  where
    do_rough Nothing  = Nothing
    do_rough (Just n) = Just (toIfaceTyCon_name n)

    fam_decl = tyConName $ coAxiomTyCon axiom
    mod = ASSERT( isExternalName (coAxiomName axiom) )
          nameModule (coAxiomName axiom)
    is_local name = nameIsLocalOrFrom mod name

    lhs_names = filterNameSet is_local (orphNamesOfCoCon axiom)

    orph | is_local fam_decl
         = NotOrphan (nameOccName fam_decl)
         | otherwise
         = chooseOrphanAnchor lhs_names

--------------------------
coreRuleToIfaceRule :: CoreRule -> IfaceRule
coreRuleToIfaceRule (BuiltinRule { ru_fn = fn})
  = pprTrace "toHsRule: builtin" (ppr fn) $
    bogusIfaceRule fn

coreRuleToIfaceRule (Rule { ru_name = name, ru_fn = fn,
                            ru_act = act, ru_bndrs = bndrs,
                            ru_args = args, ru_rhs = rhs,
                            ru_orphan = orph, ru_auto = auto })
  = IfaceRule { ifRuleName  = name, ifActivation = act,
                ifRuleBndrs = map toIfaceBndr bndrs,
                ifRuleHead  = fn,
                ifRuleArgs  = map do_arg args,
                ifRuleRhs   = toIfaceExpr rhs,
                ifRuleAuto  = auto,
                ifRuleOrph  = orph }
  where
        -- For type args we must remove synonyms from the outermost
        -- level.  Reason: so that when we read it back in we'll
        -- construct the same ru_rough field as we have right now;
        -- see tcIfaceRule
    do_arg (Type ty)     = IfaceType (toIfaceType (deNoteType ty))
    do_arg (Coercion co) = IfaceCo   (toIfaceCoercion co)
    do_arg arg           = toIfaceExpr arg

bogusIfaceRule :: Name -> IfaceRule
bogusIfaceRule id_name
  = IfaceRule { ifRuleName = fsLit "bogus", ifActivation = NeverActive,
        ifRuleBndrs = [], ifRuleHead = id_name, ifRuleArgs = [],
        ifRuleRhs = IfaceExt id_name, ifRuleOrph = IsOrphan,
        ifRuleAuto = True }
