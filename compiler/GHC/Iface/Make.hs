
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
   , mkRecompUsageInfo
   , mkIfaceExports
   )
where

import GHC.Prelude

import GHC.Hs

import GHC.Stg.EnforceEpt.TagSig (StgCgInfos)
import GHC.StgToCmm.Types (CmmCgInfos (..))

import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Monad

import GHC.Iface.Warnings
import GHC.Iface.Decl
import GHC.Iface.Syntax
import GHC.Iface.Recomp
import GHC.Iface.Load
import GHC.Iface.Ext.Fields

import GHC.CoreToIface

import qualified GHC.LanguageExtensions as LangExt
import GHC.Core
import GHC.Core.Class
import GHC.Core.Coercion.Axiom
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core.Ppr
import GHC.Core.RoughMap ( RoughMatchTc(..) )

import GHC.Driver.Config.HsToCore.Usage
import GHC.Driver.Env
import GHC.Driver.DynFlags
import GHC.Driver.Plugins

import GHC.Types.Id
import GHC.Types.Fixity.Env
import GHC.Types.ForeignStubs (ForeignStubs (NoStubs))
import GHC.Types.SafeHaskell
import GHC.Types.Annotations
import GHC.Types.Name
import GHC.Types.Avail
import GHC.Types.Name.Reader
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.DefaultEnv ( ClassDefaults (..), DefaultEnv, defaultList )
import GHC.Types.Unique.DSet
import GHC.Types.TypeEnv
import GHC.Types.SourceFile
import GHC.Types.TyThing
import GHC.Types.CompleteMatch
import GHC.Types.Name.Cache

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Logger
import GHC.Utils.Binary
import GHC.Iface.Binary

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
import GHC.Unit.Module.WholeCoreBindings (encodeIfaceForeign, emptyIfaceForeign)

import Data.Function
import Data.List ( sortBy )
import Data.Ord
import Data.IORef
import Data.Traversable

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
               -> [ImportUserSpec]
               -> ModGuts
               -> IO PartialModIface
mkPartialIface hsc_env core_prog mod_details mod_summary import_decls
  ModGuts{ mg_module       = this_mod
         , mg_hsc_src      = hsc_src
         , mg_usages       = usages
         , mg_deps         = deps
         , mg_rdr_env      = rdr_env
         , mg_fix_env      = fix_env
         , mg_warns        = warns
         , mg_safe_haskell = safe_mode
         , mg_trust_pkg    = self_trust
         , mg_docs         = docs
         }
  = do
      self_recomp <- traverse (mkSelfRecomp hsc_env this_mod (ms_hs_hash mod_summary)) usages
      return $ mkIface_ hsc_env this_mod core_prog hsc_src deps rdr_env import_decls fix_env warns self_trust
                safe_mode self_recomp docs mod_details

-- | Fully instantiate an interface. Adds fingerprints and potentially code
-- generator produced information.
--
-- CmmCgInfos is not available when not generating code (-fno-code), or when not
-- generating interface pragmas (-fomit-interface-pragmas). See also
-- Note [Conveying CAF-info and LFInfo between modules] in GHC.StgToCmm.Types.
mkFullIface :: HscEnv -> PartialModIface -> Maybe StgCgInfos -> Maybe CmmCgInfos -> ForeignStubs -> [(ForeignSrcLang, FilePath)] -> IO ModIface
mkFullIface hsc_env partial_iface mb_stg_infos mb_cmm_infos stubs foreign_files = do
    let decls
          | gopt Opt_OmitInterfacePragmas (hsc_dflags hsc_env)
          = mi_decls partial_iface
          | otherwise
          = updateDecl (mi_decls partial_iface) mb_stg_infos mb_cmm_infos

    -- See Note [Foreign stubs and TH bytecode linking]
    mi_simplified_core <- for (mi_simplified_core partial_iface) $ \simpl_core -> do
        fs <- encodeIfaceForeign (hsc_logger hsc_env) (hsc_dflags hsc_env) stubs foreign_files
        return $ (simpl_core { mi_sc_foreign = fs })

    full_iface <-
      {-# SCC "addFingerprints" #-}
      addFingerprints hsc_env $ set_mi_simplified_core mi_simplified_core $ set_mi_decls decls partial_iface

    -- Debug printing
    let unit_state = hsc_units hsc_env
    putDumpFileMaybe (hsc_logger hsc_env) Opt_D_dump_hi "FINAL INTERFACE" FormatText
      (pprModIface unit_state full_iface)
    final_iface <- shareIface (hsc_NC hsc_env) (flagsToIfCompression $ hsc_dflags hsc_env) full_iface
    return final_iface

-- | Compress an 'ModIface' and share as many values as possible, depending on the 'CompressionIFace' level.
-- See Note [Sharing of ModIface].
--
-- We compress the 'ModIface' by serialising the 'ModIface' to an in-memory byte array, and then deserialising it.
-- The deserialisation will deduplicate certain values depending on the 'CompressionIFace' level.
-- See Note [Deduplication during iface binary serialisation] for how we do that.
--
-- Additionally, we cache the serialised byte array, so if the 'ModIface' is not modified
-- after calling 'shareIface', 'writeBinIface' will reuse that buffer without serialising the 'ModIface' again.
-- Modifying the 'ModIface' forces us to re-serialise it again.
shareIface :: NameCache -> CompressionIFace -> ModIface -> IO ModIface
shareIface _ NormalCompression mi = do
  -- In 'NormalCompression', the sharing isn't reducing the memory usage, as 'Name's and 'FastString's are
  -- already shared, and at this compression level, we don't compress/share anything else.
  -- Thus, for a brief moment we simply double the memory residency for no reason.
  -- Therefore, we only try to share expensive values if the compression mode is higher than
  -- 'NormalCompression'
  pure mi
shareIface nc compressionLevel  mi = do
  bh <- openBinMem initBinMemSize
  start <- tellBinWriter bh
  putIfaceWithExtFields QuietBinIFace compressionLevel bh mi
  rbh <- shrinkBinBuffer bh
  seekBinReader rbh start
  res <- getIfaceWithExtFields nc rbh
  forceModIface res
  return res

-- | Initial ram buffer to allocate for writing interface files.
initBinMemSize :: Int
initBinMemSize = 1024 * 1024 -- 1 MB

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
                      tcg_import_decls = import_decls,
                      tcg_rdr_env = rdr_env,
                      tcg_fix_env = fix_env,
                      tcg_warns = warns
                    }
  = do
          let pluginModules = map lpModule (loadedPlugins (hsc_plugins hsc_env))
          let home_unit = hsc_home_unit hsc_env
          let deps = mkDependencies home_unit
                                    (tcg_mod tc_result)
                                    (tcg_imports tc_result)
                                    (map mi_module pluginModules)

          usage <- mkRecompUsageInfo hsc_env tc_result
          docs <- extractDocs (ms_hspp_opts mod_summary) tc_result
          self_recomp <- traverse (mkSelfRecomp hsc_env this_mod (ms_hs_hash mod_summary)) usage

          let partial_iface = mkIface_ hsc_env
                   this_mod (fromMaybe [] mb_program) hsc_src
                   deps rdr_env import_decls
                   fix_env warns
                   (imp_trust_own_pkg imports) safe_mode self_recomp
                   docs
                   mod_details

          mkFullIface hsc_env partial_iface Nothing Nothing NoStubs []

mkRecompUsageInfo :: HscEnv -> TcGblEnv -> IO (Maybe [Usage])
mkRecompUsageInfo hsc_env tc_result = do
  let dflags = hsc_dflags hsc_env
  if not (gopt Opt_WriteSelfRecompInfo dflags)
    then return Nothing
    else do
     let used_names = mkUsedNames tc_result
     dep_files <- (readIORef (tcg_dependent_files tc_result))
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
     usages <- initIfaceLoad hsc_env $ mkUsageInfo uc plugins fc unit_env (tcg_mod tc_result) (imp_mods (tcg_imports tc_result)) used_names
                 dep_files (tcg_merged tc_result) needed_links needed_pkgs
     return (Just usages)

mkIface_ :: HscEnv -> Module -> CoreProgram -> HscSource
         -> Dependencies -> GlobalRdrEnv -> [ImportUserSpec]
         -> NameEnv FixItem -> Warnings GhcRn
         -> Bool
         -> SafeHaskellMode
         -> Maybe IfaceSelfRecomp
         -> Maybe Docs
         -> ModDetails
         -> PartialModIface
mkIface_ hsc_env
         this_mod core_prog hsc_src deps rdr_env import_decls fix_env src_warns
         pkg_trust_req safe_mode self_recomp
         docs
         ModDetails{  md_defaults  = defaults,
                      md_insts     = insts,
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

        simplified_core = if gopt Opt_WriteIfSimplifiedCore dflags then Just (IfaceSimplifiedCore [ toIfaceTopBind b | b <- core_prog ] emptyIfaceForeign)
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
        warns       = toIfaceWarnings src_warns
        iface_rules = map coreRuleToIfaceRule rules
        iface_insts = map instanceToIfaceInst $ fixSafeInstances safe_mode (instEnvElts insts)
        iface_fam_insts = map famInstToIfaceFamInst fam_insts
        trust_info  = setSafeMode safe_mode
        annotations = map mkIfaceAnnotation anns
        icomplete_matches = map mkIfaceCompleteMatch complete_matches
        !rdrs = mkIfaceTopEnv rdr_env

    emptyPartialModIface this_mod
          -- Need to record this because it depends on the -instantiated-with flag
          -- which could change
          & set_mi_sig_of           (if semantic_mod == this_mod
                                      then Nothing
                                      else Just semantic_mod)
          & set_mi_hsc_src          hsc_src
          & set_mi_self_recomp      self_recomp
          & set_mi_deps             deps
          & set_mi_exports          (mkIfaceExports exports)

          & set_mi_defaults         (defaultsToIfaceDefaults defaults)

          -- Sort these lexicographically, so that
          -- the result is stable across compilations
          & set_mi_insts            (sortBy cmp_inst     iface_insts)
          & set_mi_fam_insts        (sortBy cmp_fam_inst iface_fam_insts)
          & set_mi_rules            (sortBy cmp_rule     iface_rules)

          & set_mi_fixities         fixities
          & set_mi_warns            warns
          & set_mi_anns             annotations
          & set_mi_top_env          rdrs
          & set_mi_decls            decls
          & set_mi_simplified_core  simplified_core
          & set_mi_trust            trust_info
          & set_mi_trust_pkg        pkg_trust_req
          & set_mi_complete_matches (icomplete_matches)
          & set_mi_docs             docs
          & set_mi_abi_hashes       ()
          & set_mi_ext_fields       emptyExtensibleFields
          & set_mi_hi_bytes         PartialIfaceBinHandle

  where
     cmp_rule     = lexicalCompareFS `on` ifRuleName
     -- Compare these lexicographically by OccName, *not* by unique,
     -- because the latter is not stable across compilations:
     cmp_inst     = comparing (nameOccName . ifDFun)
     cmp_fam_inst = comparing (nameOccName . ifFamInstTcName)

     dflags = hsc_dflags hsc_env

     -- We only fill in mi_top_env if the module was compiled to byte
     -- code.  Otherwise, the compiler may not have retained all the
     -- top-level bindings and they won't be in the TypeEnv (see
     -- Desugar.addExportFlagsAndRules).  The mi_top_env field is used
     -- by GHCi to decide whether the module has its full top-level
     -- scope available. (#5534)
     mkIfaceTopEnv :: GlobalRdrEnv -> IfaceTopEnv
     mkIfaceTopEnv rdr_env
        = let !exports = sortAvails $ gresToAvailInfo $ globalRdrEnvElts $ globalRdrEnvLocal rdr_env
              !imports = mkIfaceImports import_decls
           in IfaceTopEnv exports imports

     ifFamInstTcName = ifFamInstFam

--------------------------
defaultsToIfaceDefaults :: DefaultEnv -> [IfaceDefault]
defaultsToIfaceDefaults = map toIface . defaultList
  where
    toIface ClassDefaults { cd_class = cls
                          , cd_types = tys
                          , cd_warn = warn }
      = IfaceDefault { ifDefaultCls = className cls
                     , ifDefaultTys = map toIfaceType tys
                     , ifDefaultWarn = fmap toIfaceWarningTxt warn }

--------------------------
instanceToIfaceInst :: ClsInst -> IfaceClsInst
instanceToIfaceInst (ClsInst { is_dfun = dfun_id, is_flag = oflag
                             , is_cls_nm = cls_name, is_cls = cls
                             , is_tcs = rough_tcs
                             , is_orphan = orph
                             , is_warn = warn })
  = assert (cls_name == className cls) $
    IfaceClsInst { ifDFun     = idName dfun_id
                 , ifOFlag    = oflag
                 , ifInstCls  = cls_name
                 , ifInstTys  = ifaceRoughMatchTcs $ tail rough_tcs
                   -- N.B. Drop the class name from the rough match template
                   --      It is put back by GHC.Core.InstEnv.mkImportedClsInst
                 , ifInstOrph = orph
                 , ifInstWarn = fmap toIfaceWarningTxt warn }

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


{-
************************************************************************
*                                                                      *
       COMPLETE Pragmas
*                                                                      *
************************************************************************
-}

mkIfaceCompleteMatch :: CompleteMatch -> IfaceCompleteMatch
mkIfaceCompleteMatch (CompleteMatch cls mtc) =
  -- NB: Sorting here means that COMPLETE {P, Q} and COMPLETE {Q, P} are
  -- considered identical, in particular for recompilation checking.
  IfaceCompleteMatch (sortBy stableNameCmp $ uniqDSetToList cls) mtc


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

mkIfaceImports :: [ImportUserSpec] -> [IfaceImport]
mkIfaceImports = map go
  where
    go (ImpUserSpec decl ImpUserAll) = IfaceImport decl ImpIfaceAll
    go (ImpUserSpec decl (ImpUserExplicit env)) = IfaceImport decl (ImpIfaceExplicit (sortAvails env))
    go (ImpUserSpec decl (ImpUserEverythingBut ns)) = IfaceImport decl (ImpIfaceEverythingBut (nameSetElemsStable ns))

mkIfaceExports :: [AvailInfo] -> [IfaceExport] -- Sort to make canonical
mkIfaceExports as = case sortAvails as of DefinitelyDeterministicAvails sas -> sas

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
Note [Sharing of ModIface]
~~~~~~~~~~~~~~~~~~~~~~~~~~
A 'ModIface' contains many duplicated values such as 'Name', 'FastString' and 'IfaceType'.
'Name's and 'FastString's are already deduplicated by default using the 'NameCache' and
'FastStringTable' respectively.
However, 'IfaceType' can be quite expensive in terms of memory usage.
To improve the sharing of 'IfaceType', we introduced deduplication tables during
serialisation of 'ModIface', see Note [Deduplication during iface binary serialisation].

We can improve the sharing of 'ModIface' at run-time as well, by serialising the 'ModIface' to
an in-memory buffer, and then deserialising it again.
This implicitly shares duplicated values.

To avoid re-serialising the 'ModIface' when writing it to disk, we save the serialised 'ModIface' buffer
in 'mi_hi_bytes_' field of said 'ModIface'. This buffer is written to disk directly in 'putIfaceWithExtFields'.
If we have to modify the 'ModIface' after 'shareIface' is called, the buffer needs to be discarded.
-}
