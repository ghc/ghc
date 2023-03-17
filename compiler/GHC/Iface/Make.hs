
{-# LANGUAGE NondecreasingIndentation #-}

{-
(c) The University of Glasgow 2006-2008
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998
-}

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

import GHC.Prelude

import GHC.Hs

import GHC.Stg.InferTags.TagSig (StgCgInfos)
import GHC.StgToCmm.Types (CmmCgInfos (..))

import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Monad

import GHC.Iface.Syntax
import GHC.Iface.Recomp
import GHC.Iface.Load
import GHC.Iface.Ext.Fields

import GHC.CoreToIface

import qualified GHC.LanguageExtensions as LangExt
import GHC.Core
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core.Ppr
import GHC.Core.RoughMap( RoughMatchTc(..) )

import GHC.Driver.Config.HsToCore.Usage
import GHC.Driver.Env
import GHC.Driver.Backend
import GHC.Driver.Session
import GHC.Driver.Plugins

import GHC.Types.Id
import GHC.Types.Fixity.Env
import GHC.Types.SafeHaskell
import GHC.Types.Annotations
import GHC.Types.Var.Env
import GHC.Types.Var
import GHC.Types.Name
import GHC.Types.Avail
import GHC.Types.Name.Reader
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Unique.DSet
import GHC.Types.Basic hiding ( SuccessFlag(..) )
import GHC.Types.TypeEnv
import GHC.Types.SourceFile
import GHC.Types.TyThing
import GHC.Types.HpcInfo
import GHC.Types.CompleteMatch

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc
import GHC.Utils.Logger

import GHC.Data.FastString
import GHC.Data.Maybe

import GHC.HsToCore.Docs
import GHC.HsToCore.Usage

import GHC.Unit
import GHC.Unit.Module.Warnings
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Deps

import Data.Function
import Data.List ( findIndex, mapAccumL, sortBy )
import Data.Ord
import Data.IORef


{-
************************************************************************
*                                                                      *
\subsection{Completing an interface}
*                                                                      *
************************************************************************
-}

mkPartialIface :: HscEnv
               -> CoreProgram
               -> ModDetails
               -> ModSummary
               -> ModGuts
               -> PartialModIface
mkPartialIface hsc_env core_prog mod_details mod_summary
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
         , mg_docs         = docs
         }
  = mkIface_ hsc_env this_mod core_prog hsc_src used_th deps rdr_env fix_env warns hpc_info self_trust
             safe_mode usages docs mod_summary mod_details

-- | Fully instantiate an interface. Adds fingerprints and potentially code
-- generator produced information.
--
-- CmmCgInfos is not available when not generating code (-fno-code), or when not
-- generating interface pragmas (-fomit-interface-pragmas). See also
-- Note [Conveying CAF-info and LFInfo between modules] in GHC.StgToCmm.Types.
mkFullIface :: HscEnv -> PartialModIface -> Maybe StgCgInfos -> Maybe CmmCgInfos -> IO ModIface
mkFullIface hsc_env partial_iface mb_stg_infos mb_cmm_infos = do
    let decls
          | gopt Opt_OmitInterfacePragmas (hsc_dflags hsc_env)
          = mi_decls partial_iface
          | otherwise
          = updateDecl (mi_decls partial_iface) mb_stg_infos mb_cmm_infos

    full_iface <-
      {-# SCC "addFingerprints" #-}
      addFingerprints hsc_env partial_iface{ mi_decls = decls }

    -- Debug printing
    let unit_state = hsc_units hsc_env
    putDumpFileMaybe (hsc_logger hsc_env) Opt_D_dump_hi "FINAL INTERFACE" FormatText
      (pprModIface unit_state full_iface)

    return full_iface

updateDecl :: [IfaceDecl] -> Maybe StgCgInfos -> Maybe CmmCgInfos -> [IfaceDecl]
updateDecl decls Nothing Nothing = decls
updateDecl decls m_stg_infos m_cmm_infos
  = map update_decl decls
  where
    (non_cafs,lf_infos) = maybe (mempty, mempty)
                                (\cmm_info -> (ncs_nameSet (cgNonCafs cmm_info), cgLFInfos cmm_info))
                                m_cmm_infos
    tag_sigs = fromMaybe mempty m_stg_infos

    update_decl (IfaceId nm ty details infos)
      | let not_caffy = elemNameSet nm non_cafs
      , let mb_lf_info = lookupNameEnv lf_infos nm
      , let sig = lookupNameEnv tag_sigs nm
      , warnPprTrace (isNothing mb_lf_info) "updateDecl" (text "Name without LFInfo:" <+> ppr nm) True
        -- Only allocate a new IfaceId if we're going to update the infos
      , isJust mb_lf_info || not_caffy || isJust sig
      = IfaceId nm ty details $
          (if not_caffy then (HsNoCafRefs :) else id) $
          (if isJust sig then (HsTagSig (fromJust sig):) else id) $
          (case mb_lf_info of
             Nothing -> infos -- LFInfos not available when building .cmm files
             Just lf_info -> HsLFInfo (toIfaceLFInfo nm lf_info) : infos)

    update_decl decl
      = decl




-- | Make an interface from the results of typechecking only.  Useful
-- for non-optimising compilation, or where we aren't generating any
-- object code at all ('NoBackend').
mkIfaceTc :: HscEnv
          -> SafeHaskellMode    -- The safe haskell mode
          -> ModDetails         -- gotten from mkBootModDetails, probably
          -> ModSummary
          -> Maybe CoreProgram
          -> TcGblEnv           -- Usages, deprecations, etc
          -> IO ModIface
mkIfaceTc hsc_env safe_mode mod_details mod_summary mb_program
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
          let pluginModules = map lpModule (loadedPlugins (hsc_plugins hsc_env))
          let home_unit = hsc_home_unit hsc_env
          let deps = mkDependencies home_unit
                                    (tcg_mod tc_result)
                                    (tcg_imports tc_result)
                                    (map mi_module pluginModules)
          let hpc_info = emptyHpcInfo other_hpc_info
          used_th <- readIORef tc_splice_used
          dep_files <- (readIORef dependent_files)
          (needed_links, needed_pkgs) <- readIORef (tcg_th_needed_deps tc_result)
          let uc = initUsageConfig hsc_env
              plugins = hsc_plugins hsc_env
              fc = hsc_FC hsc_env
              unit_env = hsc_unit_env hsc_env
          -- Do NOT use semantic module here; this_mod in mkUsageInfo
          -- is used solely to decide if we should record a dependency
          -- or not.  When we instantiate a signature, the semantic
          -- module is something we want to record dependencies for,
          -- but if you pass that in here, we'll decide it's the local
          -- module and does not need to be recorded as a dependency.
          -- See Note [Identity versus semantic module]
          usages <- initIfaceLoad hsc_env $ mkUsageInfo uc plugins fc unit_env this_mod (imp_mods imports) used_names
                      dep_files merged needed_links needed_pkgs

          docs <- extractDocs (ms_hspp_opts mod_summary) tc_result

          let partial_iface = mkIface_ hsc_env
                   this_mod (fromMaybe [] mb_program) hsc_src
                   used_th deps rdr_env
                   fix_env warns hpc_info
                   (imp_trust_own_pkg imports) safe_mode usages
                   docs mod_summary
                   mod_details

          mkFullIface hsc_env partial_iface Nothing Nothing

mkIface_ :: HscEnv -> Module -> CoreProgram -> HscSource
         -> Bool -> Dependencies -> GlobalRdrEnv
         -> NameEnv FixItem -> Warnings GhcRn -> HpcInfo
         -> Bool
         -> SafeHaskellMode
         -> [Usage]
         -> Maybe Docs
         -> ModSummary
         -> ModDetails
         -> PartialModIface
mkIface_ hsc_env
         this_mod core_prog hsc_src used_th deps rdr_env fix_env src_warns
         hpc_info pkg_trust_req safe_mode usages
         docs mod_summary
         ModDetails{  md_insts     = insts,
                      md_fam_insts = fam_insts,
                      md_rules     = rules,
                      md_anns      = anns,
                      md_types     = type_env,
                      md_exports   = exports,
                      md_complete_matches = complete_matches }
-- NB:  notice that mkIface does not look at the bindings
--      only at the TypeEnv.  The previous Tidy phase has
--      put exactly the info into the TypeEnv that we want
--      to expose in the interface

  = do
    let home_unit    = hsc_home_unit hsc_env
        semantic_mod = homeModuleNameInstantiation home_unit (moduleName this_mod)
        entities = typeEnvElts type_env
        show_linear_types = xopt LangExt.LinearTypes (hsc_dflags hsc_env)

        extra_decls = if gopt Opt_WriteIfSimplifiedCore dflags then Just [ toIfaceTopBind b | b <- core_prog ]
                                                               else Nothing
        decls  = [ tyThingToIfaceDecl show_linear_types entity
                 | entity <- entities,
                   let name = getName entity,
                   not (isImplicitTyThing entity),
                      -- No implicit Ids and class tycons in the interface file
                   not (isWiredInName name),
                      -- Nor wired-in things; the compiler knows about them anyhow
                   nameIsLocalOrFrom semantic_mod name  ]
                      -- Sigh: see Note [Root-main Id] in GHC.Tc.Module
                      -- NB: ABSOLUTELY need to check against semantic_mod,
                      -- because all of the names in an hsig p[H=<H>]:H
                      -- are going to be for <H>, not the former id!
                      -- See Note [Identity versus semantic module]

        fixities    = sortBy (comparing fst)
          [(occ,fix) | FixItem occ fix <- nonDetNameEnvElts fix_env]
          -- The order of fixities returned from nonDetNameEnvElts is not
          -- deterministic, so we sort by OccName to canonicalize it.
          -- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for more details.
        warns       = src_warns
        iface_rules = map coreRuleToIfaceRule rules
        iface_insts = map instanceToIfaceInst $ fixSafeInstances safe_mode (instEnvElts insts)
        iface_fam_insts = map famInstToIfaceFamInst fam_insts
        trust_info  = setSafeMode safe_mode
        annotations = map mkIfaceAnnotation anns
        icomplete_matches = map mkIfaceCompleteMatch complete_matches
        !rdrs = maybeGlobalRdrEnv rdr_env

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
          mi_globals     = rdrs,
          mi_used_th     = used_th,
          mi_decls       = decls,
          mi_extra_decls = extra_decls,
          mi_hpc         = isHpcUsed hpc_info,
          mi_trust       = trust_info,
          mi_trust_pkg   = pkg_trust_req,
          mi_complete_matches = icomplete_matches,
          mi_docs        = docs,
          mi_final_exts  = (),
          mi_ext_fields  = emptyExtensibleFields,
          mi_src_hash = ms_hs_hash mod_summary
          }
  where
     cmp_rule     = lexicalCompareFS `on` ifRuleName
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
     maybeGlobalRdrEnv :: GlobalRdrEnv -> Maybe IfGlobalRdrEnv
     maybeGlobalRdrEnv rdr_env
        | backendWantsGlobalBindings (backend dflags)
        = Just $! forceGlobalRdrEnv rdr_env
          -- See Note [Forcing GREInfo] in GHC.Types.GREInfo.
        | otherwise
        = Nothing

     ifFamInstTcName = ifFamInstFam


{-
************************************************************************
*                                                                      *
       COMPLETE Pragmas
*                                                                      *
************************************************************************
-}

mkIfaceCompleteMatch :: CompleteMatch -> IfaceCompleteMatch
mkIfaceCompleteMatch (CompleteMatch cls mtc) =
  IfaceCompleteMatch (map conLikeName (uniqDSetToList cls)) (toIfaceTyCon <$> mtc)


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
    sort_subs (AvailTC n []) = AvailTC n []
    sort_subs (AvailTC n (m:ms))
       | n == m
       = AvailTC n (m:sortBy stableNameCmp ms)
       | otherwise
       = AvailTC n (sortBy stableNameCmp (m:ms))
       -- Maintain the AvailTC Invariant

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
-}


{-
************************************************************************
*                                                                      *
                Converting things to their Iface equivalents
*                                                                      *
************************************************************************
-}

tyThingToIfaceDecl :: Bool -> TyThing -> IfaceDecl
tyThingToIfaceDecl _ (AnId id)      = idToIfaceDecl id
tyThingToIfaceDecl _ (ATyCon tycon) = snd (tyConToIfaceDecl emptyTidyEnv tycon)
tyThingToIfaceDecl _ (ACoAxiom ax)  = coAxiomToIfaceDecl ax
tyThingToIfaceDecl show_linear_types (AConLike cl)  = case cl of
    RealDataCon dc -> dataConToIfaceDecl show_linear_types dc -- for ppr purposes only
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
dataConToIfaceDecl :: Bool -> DataCon -> IfaceDecl
dataConToIfaceDecl show_linear_types dataCon
  = IfaceId { ifName      = getName dataCon,
              ifType      = toIfaceType (dataConDisplayType show_linear_types dataCon),
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
                  ifCType   = tyConCType_maybe tycon,
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
                  ifCons       = IfDataTyCon False [],
                  ifGadtSyntax = False,
                  ifParent     = IfNoParent })
  where
    -- NOTE: Not all TyCons have `tyConTyVars` field. Forcing this when `tycon`
    -- is one of these TyCons (FunTyCon, PrimTyCon, PromotedDataCon) will cause
    -- an error.
    (tc_env1, tc_binders) = tidyTyConBinders env (tyConBinders tycon)
    tc_tyvars      = binderVars tc_binders
    if_binders     = toIfaceForAllBndrs tc_binders
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
    ifaceConDecls (DataTyCon { data_cons = cons, is_type_data = type_data })
      = IfDataTyCon type_data (map ifaceConDecl cons)
    ifaceConDecls (TupleTyCon { data_con = con })  = IfDataTyCon False [ifaceConDecl con]
    ifaceConDecls (SumTyCon { data_cons = cons })  = IfDataTyCon False (map ifaceConDecl cons)
    ifaceConDecls AbstractTyCon                    = IfAbstractTyCon
        -- The AbstractTyCon case happens when a TyCon has been trimmed
        -- during tidying.
        -- Furthermore, tyThingToIfaceDecl is also used in GHC.Tc.Module
        -- for GHCi, when browsing a module, in which case the
        -- AbstractTyCon and TupleTyCon cases are perfectly sensible.
        -- (Tuple declarations are not serialised into interface files.)

    ifaceConDecl data_con
        = IfCon   { ifConName    = dataConName data_con,
                    ifConInfix   = dataConIsInfix data_con,
                    ifConWrapper = isJust (dataConWrapId_maybe data_con),
                    ifConExTCvs  = map toIfaceBndr ex_tvs',
                    ifConUserTvBinders = toIfaceForAllBndrs user_bndrs',
                    ifConEqSpec  = map (to_eq_spec . eqSpecPair) eq_spec,
                    ifConCtxt    = tidyToIfaceContext con_env2 theta,
                    ifConArgTys  =
                      map (\(Scaled w t) -> (tidyToIfaceType con_env2 w
                                          , (tidyToIfaceType con_env2 t))) arg_tys,
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
          user_bndrs' = map (tidyUserForAllTyBinder con_env2) user_bndrs
          to_eq_spec (tv,ty) = (tidyTyVar con_env2 tv, tidyToIfaceType con_env2 ty)

          -- By this point, we have tidied every universal and existential
          -- tyvar. Because of the dcUserForAllTyBinders invariant
          -- (see Note [DataCon user type variable binders]), *every*
          -- user-written tyvar must be contained in the substitution that
          -- tidying produced. Therefore, tidying the user-written tyvars is a
          -- simple matter of looking up each variable in the substitution,
          -- which tidyTyCoVarOcc accomplishes.
          tidyUserForAllTyBinder :: TidyEnv -> InvisTVBinder -> InvisTVBinder
          tidyUserForAllTyBinder env (Bndr tv vis) =
            Bndr (tidyTyCoVarOcc env tv) vis

classToIfaceDecl :: TidyEnv -> Class -> (TidyEnv, IfaceDecl)
classToIfaceDecl env clas
  = ( env1
    , IfaceClass { ifName   = getName tycon,
                   ifRoles  = tyConRoles (classTyCon clas),
                   ifBinders = toIfaceForAllBndrs tc_binders,
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
        = assert (sel_tyvars == binderVars tc_binders) $
          IfaceClassOp (getName sel_id)
                       (tidyToIfaceType env1 op_ty)
                       (fmap toDmSpec def_meth)
        where
                -- Be careful when splitting the type, because of things
                -- like         class Foo a where
                --                op :: (?x :: String) => a -> a
                -- and          class Baz a where
                --                op :: (Ord a) => a -> a
          (sel_tyvars, rho_ty) = splitForAllTyCoVars (idType sel_id)
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
     Nothing  -> tidyForAllTyBinder env tvb

tidyTyConBinders :: TidyEnv -> [TyConBinder] -> (TidyEnv, [TyConBinder])
tidyTyConBinders = mapAccumL tidyTyConBinder

tidyTyVar :: TidyEnv -> TyVar -> FastString
tidyTyVar (_, subst) tv = toIfaceTyVar (lookupVarEnv subst tv `orElse` tv)

--------------------------
instanceToIfaceInst :: ClsInst -> IfaceClsInst
instanceToIfaceInst (ClsInst { is_dfun = dfun_id, is_flag = oflag
                             , is_cls_nm = cls_name, is_cls = cls
                             , is_tcs = rough_tcs
                             , is_orphan = orph })
  = assert (cls_name == className cls) $
    IfaceClsInst { ifDFun     = idName dfun_id
                 , ifOFlag    = oflag
                 , ifInstCls  = cls_name
                 , ifInstTys  = ifaceRoughMatchTcs $ tail rough_tcs
                   -- N.B. Drop the class name from the rough match template
                   --      It is put back by GHC.Core.InstEnv.mkImportedClsInst
                 , ifInstOrph = orph }

--------------------------
famInstToIfaceFamInst :: FamInst -> IfaceFamInst
famInstToIfaceFamInst (FamInst { fi_axiom    = axiom
                               , fi_fam      = fam
                               , fi_tcs      = rough_tcs
                               , fi_orphan   = orphan })
  = IfaceFamInst { ifFamInstAxiom    = coAxiomName axiom
                 , ifFamInstFam      = fam
                 , ifFamInstTys      = ifaceRoughMatchTcs rough_tcs
                 , ifFamInstOrph     = orphan }

ifaceRoughMatchTcs :: [RoughMatchTc] -> [Maybe IfaceTyCon]
ifaceRoughMatchTcs tcs = map do_rough tcs
  where
    do_rough RM_WildCard     = Nothing
    do_rough (RM_KnownTc n) = Just (toIfaceTyCon_name n)

--------------------------
coreRuleToIfaceRule :: CoreRule -> IfaceRule
-- A plugin that installs a BuiltinRule in a CoreDoPluginPass should
-- ensure that there's another CoreDoPluginPass that removes the rule.
-- Otherwise a module using the plugin and compiled with -fno-omit-interface-pragmas
-- would cause panic when the rule is attempted to be written to the interface file.
coreRuleToIfaceRule rule@(BuiltinRule {})
  = pprPanic "toHsRule:" (pprRule rule)

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
