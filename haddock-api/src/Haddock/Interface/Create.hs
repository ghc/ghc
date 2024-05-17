{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Interface.Create
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a single function 'createInterface',
-- which creates a Haddock 'Interface' from the typechecking
-- results 'TypecheckedModule' from GHC.
module Haddock.Interface.Create (IfM, runIfM, createInterface1) where

import Documentation.Haddock.Doc
import Haddock.Convert (PrintRuntimeReps (..), tyThingToLHsDecl)
import Haddock.GhcUtils
import Haddock.Interface.LexParseRn
import Haddock.Options (Flag (..), modulePackageInfo)
import Haddock.Types
import Haddock.Utils (replace)

import Control.DeepSeq
import Control.Monad.State.Strict
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Traversable (for)

import Control.Arrow (first, (&&&))
import GHC hiding (lookupName)
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Core.ConLike (ConLike (..))
import GHC.Data.FastString (FastString, bytesFS, unpackFS)
import GHC.Driver.Ppr
import GHC.HsToCore.Docs hiding (mkMaps)
import GHC.Iface.Syntax
import GHC.Types.Avail
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.SafeHaskell
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Types.Unique.Map as UniqMap
import GHC.Unit.Module.ModIface
import GHC.Unit.State (PackageName (..), UnitState)
import qualified GHC.Utils.Outputable as O
import GHC.Utils.Panic (pprPanic)

createInterface1
  :: MonadIO m
  => [Flag]
  -> UnitState
  -> ModSummary
  -> ModIface
  -> IfaceMap
  -> InstIfaceMap
  -> ([ClsInst], [FamInst])
  -> IfM m Interface
createInterface1 flags unit_state mod_sum mod_iface ifaces inst_ifaces (instances, fam_instances) = do
  let
    ModSummary
      { -- Cached flags from OPTIONS, INCLUDE and LANGUAGE
      -- pragmas in the modules source code. Used to infer
      -- safety of module.
      ms_hspp_opts
      , ms_location =
        ModLocation
          { ml_hie_file
          }
      } = mod_sum

    dflags = ms_hspp_opts
    mdl = mi_module mod_iface
    sem_mdl = mi_semantic_module mod_iface
    is_sig = isJust (mi_sig_of mod_iface)
    safety = getSafeMode (mi_trust mod_iface)

    (pkg_name_fs, _) =
      modulePackageInfo unit_state flags (Just mdl)

    pkg_name :: Maybe Package
    pkg_name =
      let
        unpack (PackageName name) = unpackFS name
       in
        fmap unpack pkg_name_fs

    warnings = mi_warns mod_iface

    -- See Note [Exporting built-in items]
    special_exports
      | mdl == gHC_PRIM = funAvail
      | otherwise = []
    !exportedNames =
      concatMap
        availNames
        (special_exports <> mi_exports mod_iface)

    fixities :: FixMap
    fixities = mkFixMap exportedNames (mi_fixities mod_iface)

    -- This is used for looking up the Name of a default method
    -- from its OccName. See Note [default method Name] in GHC.Iface.Recomp
    def_meths_env = mkOccEnv def_meths
    def_meths =
      [ (nameOccName nm, nm)
      | (_, IfaceId{ifName = nm}) <- mi_decls mod_iface
      , let occ = nameOccName nm
      , isDefaultMethodOcc occ
      ]

  mod_iface_docs <- case mi_docs mod_iface of
    Just docs -> pure docs
    Nothing -> do
      warn $ showPpr dflags mdl ++ " has no docs in its .hi file"
      pure emptyDocs
  -- Derive final options to use for haddocking this module
  doc_opts <- mkDocOpts (docs_haddock_opts mod_iface_docs) flags mdl

  let prr
        | OptPrintRuntimeRep `elem` doc_opts = ShowRuntimeRep
        | otherwise = HideRuntimeRep

  (!info, header_doc) <-
    processModuleHeader
      dflags
      pkg_name
      safety
      (docs_language mod_iface_docs)
      (docs_extensions mod_iface_docs)
      (docs_mod_hdr mod_iface_docs)
  mod_warning <- moduleWarning dflags warnings

  (docMap :: DocMap Name) <- do
    let docsDecls = Map.fromList $ UniqMap.nonDetUniqMapToList mod_iface_docs.docs_decls
    traverse (processDocStringsParas dflags pkg_name) docsDecls

  exportsSinceMap <- mkExportSinceMap dflags pkg_name mod_iface_docs

  (argMap :: Map Name (Map Int (MDoc Name))) <- do
    let docsArgs = Map.fromList $ UniqMap.nonDetUniqMapToList mod_iface_docs.docs_args
    (result :: Map Name (IntMap (MDoc Name))) <-
      traverse (traverse (processDocStringParas dflags pkg_name)) docsArgs
    let result2 = Map.map (\intMap -> Map.fromList $ IM.assocs intMap) result
    pure $ result2

  warningMap <- mkWarningMap dflags warnings exportedNames

  let local_instances =
        filter (nameIsLocalOrFrom sem_mdl) $
          map getName instances
            ++ map getName fam_instances
      instanceMap = Map.fromList [(l, n) | n <- local_instances, RealSrcSpan l _ <- [getSrcSpan n]]

  -- See Note [Exporting built-in items]
  let builtinTys = DsiSectionHeading 1 (WithHsDocIdentifiers (mkGeneratedHsDocString "Builtin syntax") [])
      bonus_ds mods
        | mdl == gHC_PRIM = [builtinTys, DsiExports funAvail] <> mods
        | otherwise = mods

  let
    -- Warnings in this module and transitive warnings from dependent modules
    transitiveWarnings :: Map Name (Doc Name)
    transitiveWarnings = Map.unions (warningMap : map ifaceWarningMap (Map.elems ifaces))

  export_items <-
    mkExportItems
      prr
      ifaces
      pkg_name
      mdl
      transitiveWarnings
      exportsSinceMap
      docMap
      argMap
      fixities
      (docs_named_chunks mod_iface_docs)
      (bonus_ds $ docs_structure mod_iface_docs)
      inst_ifaces
      dflags
      def_meths_env

  let
    visible_names :: [Name]
    visible_names = mkVisibleNames instanceMap export_items doc_opts

    -- Measure haddock documentation coverage.
    pruned_export_items :: [ExportItem GhcRn]
    pruned_export_items = pruneExportItems export_items

    !haddockable = 1 + length export_items -- module + exports
    !haddocked = (if isJust header_doc then 1 else 0) + length pruned_export_items

    coverage :: (Int, Int)
    !coverage = (haddockable, haddocked)

  return $!
    Interface
      { ifaceMod = mdl
      , ifaceIsSig = is_sig
      , ifaceHieFile = ml_hie_file
      , ifaceInfo = info
      , ifaceDoc = Documentation header_doc mod_warning
      , ifaceRnDoc = Documentation Nothing Nothing
      , ifaceOptions = doc_opts
      , ifaceDocMap = docMap
      , ifaceArgMap = argMap
      , ifaceExportItems =
          if OptPrune `elem` doc_opts
            then pruned_export_items
            else export_items
      , ifaceRnExportItems = []
      , ifaceExports = exportedNames
      , ifaceVisibleExports = visible_names
      , ifaceFixMap = fixities
      , ifaceInstances = instances
      , ifaceOrphanInstances = [] -- Filled in attachInstances
      , ifaceRnOrphanInstances = [] -- Filled in renameInterfaceRn
      , ifaceHaddockCoverage = coverage
      , ifaceWarningMap = warningMap
      , ifaceDynFlags = dflags
      , ifaceDefMeths = def_meths
      }
  where
    -- Note [Exporting built-in items]
    --
    -- @(->)@ does not show up in module exports simply because Haskell
    -- lacks the concrete syntax to represent such an export. We'd still like
    -- it to show up in docs, so we manually patch "GHC.Prim" and "Prelude"
    -- to have an extra exports for @(->)@
    --
    funAvail = [AvailTC fUNTyConName [fUNTyConName]]

-------------------------------------------------------------------------------
-- Export @since annotations
-------------------------------------------------------------------------------
mkExportSinceMap
  :: forall m
   . MonadIO m
  => DynFlags
  -> Maybe Package
  -> Docs
  -> IfM m (Map Name MetaSince)
mkExportSinceMap dflags pkg_name docs = do
  Map.unions <$> traverse processExportDoc (UniqMap.nonDetUniqMapToList (docs_exports docs))
  where
    processExportDoc :: (Name, HsDoc GhcRn) -> IfM m (Map Name MetaSince)
    processExportDoc (nm, doc) = do
      mdoc <- processDocStringsParas dflags pkg_name [doc]
      case _doc mdoc of
        DocEmpty -> return ()
        _ -> warn "Export docstrings may only contain @since annotations"
      case _metaSince (_meta mdoc) of
        Nothing -> return mempty
        Just since -> return $ Map.singleton nm since

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

mkWarningMap
  :: MonadIO m
  => DynFlags
  -> IfaceWarnings
  -> [Name]
  -> IfM m WarningMap
mkWarningMap dflags warnings exps =
  case warnings of
    IfWarnSome ws _ ->
      let expsOccEnv = mkOccEnv [(nameOccName n, n) | n <- exps]
          ws' = flip mapMaybe ws $ \(occ, w) ->
            -- Ensure we also look in the record field namespace. If the OccName
            -- resolves to multiple GREs, take the first.
            case lookupOccEnv_WithFields expsOccEnv occ of
              (n : _) -> Just (n, w)
              [] -> Nothing
       in Map.fromList <$> traverse (traverse (parseWarning dflags)) ws'
    _ -> pure Map.empty

moduleWarning
  :: MonadIO m
  => DynFlags
  -> IfaceWarnings
  -> IfM m (Maybe (Doc Name))
moduleWarning dflags (IfWarnAll w) = Just <$> parseWarning dflags w
moduleWarning _ _ = pure Nothing

parseWarning
  :: MonadIO m
  => DynFlags
  -> IfaceWarningTxt
  -> IfM m (Doc Name)
parseWarning dflags w = case w of
  IfDeprecatedTxt _ msg -> format "Deprecated: " (map dstToDoc msg)
  IfWarningTxt _ _ msg -> format "Warning: " (map dstToDoc msg)
  where
    dstToDoc :: (IfaceStringLiteral, [Name]) -> HsDoc GhcRn
    dstToDoc ((IfStringLiteral _ fs), ids) = WithHsDocIdentifiers (fsToDoc fs) (map noLoc ids)

    fsToDoc :: FastString -> HsDocString
    fsToDoc fs = GeneratedDocString $ HsDocStringChunk (bytesFS fs)

    format x bs =
      DocWarning . DocParagraph . DocAppend (DocString x)
        <$> foldrM (\doc rest -> docAppend <$> processDocString dflags doc <*> pure rest) DocEmpty bs

-------------------------------------------------------------------------------
-- Doc options
--
-- Haddock options that are embedded in the source file
-------------------------------------------------------------------------------

mkDocOpts :: MonadIO m => Maybe String -> [Flag] -> Module -> IfM m [DocOption]
mkDocOpts mbOpts flags mdl = do
  opts <- case mbOpts of
    Just opts -> case words $ replace ',' ' ' opts of
      [] -> warn "No option supplied to DOC_OPTION/doc_option" >> return []
      xs -> fmap catMaybes (mapM parseOption xs)
    Nothing -> return []
  pure (foldl go opts flags)
  where
    mdlStr = moduleString mdl

    -- Later flags override earlier ones
    go os m
      | m == Flag_HideModule mdlStr = OptHide : os
      | m == Flag_ShowModule mdlStr = filter (/= OptHide) os
      | m == Flag_ShowAllModules = filter (/= OptHide) os
      | m == Flag_ShowExtensions mdlStr = OptShowExtensions : os
      | otherwise = os

parseOption :: MonadIO m => String -> IfM m (Maybe DocOption)
parseOption "hide" = return (Just OptHide)
parseOption "prune" = return (Just OptPrune)
parseOption "not-home" = return (Just OptNotHome)
parseOption "show-extensions" = return (Just OptShowExtensions)
parseOption "print-explicit-runtime-reps" = return (Just OptPrintRuntimeRep)
parseOption other = warn ("Unrecognised option: " ++ other) >> return Nothing

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

-- | Extract a map of fixity declarations only
mkFixMap :: [Name] -> [(OccName, Fixity)] -> FixMap
mkFixMap exps occFixs =
  Map.fromList $ flip mapMaybe occFixs $ \(occ, fix_) ->
    (,fix_) <$> lookupOccEnv expsOccEnv occ
  where
    expsOccEnv = mkOccEnv (map (nameOccName &&& id) exps)

-- | Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.
--
-- We create the export items even if the module is hidden, since they
-- might be useful when creating the export items for other modules.
mkExportItems
  :: MonadIO m
  => PrintRuntimeReps
  -> IfaceMap
  -> Maybe Package -- this package
  -> Module -- this module
  -> WarningMap
  -> Map Name MetaSince
  -> DocMap Name
  -> ArgMap Name
  -> FixMap
  -> Map String (HsDoc GhcRn) -- named chunks
  -> DocStructure
  -> InstIfaceMap
  -> DynFlags
  -> OccEnv Name
  -> IfM m [ExportItem GhcRn]
mkExportItems
  prr
  modMap
  pkgName
  thisMod
  warnings
  exportSinceMap
  docMap
  argMap
  fixMap
  namedChunks
  dsItems
  instIfaceMap
  dflags
  defMeths =
    concat <$> traverse lookupExport dsItems
    where
      lookupExport :: MonadIO m => DocStructureItem -> IfM m [ExportItem GhcRn]
      lookupExport = \case
        DsiSectionHeading lev hsDoc' -> do
          doc <- processDocString dflags hsDoc'
          pure [ExportGroup lev "" doc]
        DsiDocChunk hsDoc' -> do
          doc <- processDocStringParas dflags pkgName hsDoc'
          pure [ExportDoc doc]
        DsiNamedChunkRef ref -> do
          case Map.lookup ref namedChunks of
            Nothing -> do
              warn $ "Cannot find documentation for: $" ++ ref
              pure []
            Just hsDoc' -> do
              doc <- processDocStringParas dflags pkgName hsDoc'
              pure [ExportDoc doc]
        DsiExports avails ->
          -- TODO: We probably don't need nubAvails here.
          -- mkDocStructureFromExportList already uses it.
          concat <$> traverse availExport (nubAvails avails)
        DsiModExport mod_names avails -> do
          -- only consider exporting a module if we are sure we are really
          -- exporting the whole module and not some subset.
          (unrestricted_mods, remaining_avails) <- unrestrictedModExports dflags thisMod modMap instIfaceMap avails (NE.toList mod_names)
          avail_exps <- concat <$> traverse availExport remaining_avails
          pure (map ExportModule unrestricted_mods ++ avail_exps)

      availExport :: MonadIO m => AvailInfo -> IfM m [ExportItem GhcRn]
      availExport avail =
        availExportItem
          prr
          modMap
          thisMod
          warnings
          exportSinceMap
          docMap
          argMap
          fixMap
          instIfaceMap
          dflags
          avail
          defMeths

unrestrictedModExports
  :: MonadIO m
  => DynFlags
  -> Module
  -- ^ Current Module
  -> IfaceMap
  -- ^ Already created interfaces
  -> InstIfaceMap
  -- ^ Interfaces in other packages
  -> Avails
  -> [ModuleName]
  -- ^ Modules to be exported
  -> IfM m ([Module], Avails)
  -- ^ ( modules exported without restriction
  --   , remaining exports not included in any
  --     of these modules
  --   )
unrestrictedModExports dflags thisMod ifaceMap instIfaceMap avails mod_names = do
  mods_and_exports <- fmap catMaybes $ for mod_names $ \mod_name -> do
    let m_local = mkModule (moduleUnit thisMod) mod_name
    case Map.lookup m_local ifaceMap of
      -- First lookup locally
      Just iface -> pure $ Just (ifaceMod iface, mkNameSet (ifaceExports iface))
      Nothing ->
        case Map.lookup mod_name instIfaceMap' of
          Just iface -> pure $ Just (instMod iface, mkNameSet (instExports iface))
          Nothing -> do
            warn $
              "Warning: "
                ++ pretty dflags thisMod
                ++ ": Could not find "
                ++ "documentation for exported module: "
                ++ pretty dflags mod_name
            pure Nothing
  let unrestricted = filter everythingVisible mods_and_exports
      mod_exps = unionNameSets (map snd unrestricted)
      remaining = nubAvails (filterAvails (\n -> not (n `elemNameSet` mod_exps)) avails)
  pure (map fst unrestricted, remaining)
  where
    instIfaceMap' = Map.mapKeys moduleName instIfaceMap
    all_names = availsToNameSet avails

    -- Is everything in this (supposedly re-exported) module visible?
    everythingVisible :: (Module, NameSet) -> Bool
    everythingVisible (mdl, exps)
      | not (exps `isSubsetOf` all_names) = False
      | Just iface <- Map.lookup mdl ifaceMap = OptHide `notElem` ifaceOptions iface
      | Just iface <- Map.lookup (moduleName mdl) instIfaceMap' = OptHide `notElem` instOptions iface
      | otherwise = True

    -- TODO: Add a utility based on IntMap.isSubmapOfBy
    isSubsetOf :: NameSet -> NameSet -> Bool
    isSubsetOf a b = nameSetAll (`elemNameSet` b) a

availExportItem
  :: forall m
   . MonadIO m
  => PrintRuntimeReps
  -> IfaceMap
  -> Module -- this module
  -> WarningMap
  -> Map Name MetaSince
  -- ^ export \@since declarations
  -> Map Name (MDoc Name) -- docs (keyed by 'Name's)
  -> ArgMap Name -- docs for arguments (keyed by 'Name's)
  -> FixMap
  -> InstIfaceMap
  -> DynFlags
  -> AvailInfo
  -> OccEnv Name -- Default methods
  -> IfM m [ExportItem GhcRn]
availExportItem
  prr
  modMap
  thisMod
  warnings
  exportSinceMap
  docMap
  argMap
  fixMap
  instIfaceMap
  dflags
  availInfo
  defMeths =
    declWith availInfo
    where
      declWith :: AvailInfo -> IfM m [ExportItem GhcRn]
      declWith avail = do
        let t = availName avail
        mayDecl <- hiDecl dflags prr t
        case mayDecl of
          Nothing -> return [ExportNoDecl t []]
          Just decl -> do
            availExportDecl avail decl =<< do
              -- Find docs for decl
              let tmod = nameModule t
              if tmod == thisMod
                then pure (lookupDocs avail warnings docMap argMap defMeths)
                else case Map.lookup tmod modMap of
                  Just iface ->
                    pure $
                      first (applyExportSince exportSinceMap t) $
                        lookupDocs avail warnings (ifaceDocMap iface) (ifaceArgMap iface) (mkOccEnv (ifaceDefMeths iface))
                  Nothing ->
                    -- We try to get the subs and docs
                    -- from the installed .haddock file for that package.
                    -- TODO: This needs to be more sophisticated to deal
                    -- with signature inheritance
                    case Map.lookup (nameModule t) instIfaceMap of
                      Nothing -> do
                        warn $
                          "Warning: "
                            ++ pretty dflags thisMod
                            ++ ": Couldn't find .haddock for export "
                            ++ pretty dflags t
                        let subs_ = availNoDocs avail
                        pure (noDocForDecl, subs_)
                      Just instIface ->
                        pure $
                          first (applyExportSince exportSinceMap t) $
                            lookupDocs avail warnings (instDocMap instIface) (instArgMap instIface) (mkOccEnv (instDefMeths instIface))

      -- Tries 'extractDecl' first then falls back to 'hiDecl' if that fails
      availDecl :: Name -> LHsDecl GhcRn -> IfM m (LHsDecl GhcRn)
      availDecl declName parentDecl =
        extractDecl prr dflags declName parentDecl >>= \case
          Right d -> pure d
          Left err -> do
            synifiedDeclOpt <- hiDecl dflags prr declName
            case synifiedDeclOpt of
              Just synifiedDecl -> pure synifiedDecl
              Nothing -> pprPanic "availExportItem" (O.text err)

      availExportDecl
        :: AvailInfo
        -> LHsDecl GhcRn
        -> (DocForDecl Name, [(Name, DocForDecl Name)])
        -> IfM m [ExportItem GhcRn]
      availExportDecl avail decl (doc, subs)
        | availExportsDecl avail = do
            extractedDecl <- availDecl (availName avail) decl

            -- bundled pattern synonyms only make sense if the declaration is
            -- exported (otherwise there would be nothing to bundle to)
            bundledPatSyns <- findBundledPatterns avail

            let
              !patSynNames =
                force $
                  concatMap (getMainDeclBinder emptyOccEnv . fst) bundledPatSyns

              !doc' = force doc
              !subs' = force subs

              !restrictToNames = force $ fmap fst subs'

              !fixities =
                force
                  [ (n, f)
                  | n <- availName avail : fmap fst subs' ++ patSynNames
                  , Just f <- [Map.lookup n fixMap]
                  ]

            return
              [ ExportDecl
                  ExportD
                    { expDDecl = restrictTo restrictToNames extractedDecl
                    , expDPats = bundledPatSyns
                    , expDMbDoc = doc'
                    , expDSubDocs = subs'
                    , expDInstances = []
                    , expDFixities = fixities
                    , expDSpliced = False
                    }
              ]
        | otherwise = for subs $ \(sub, sub_doc) -> do
            extractedDecl <- availDecl sub decl

            let
              !fixities = force [(sub, f) | Just f <- [Map.lookup sub fixMap]]
              !subDoc = force sub_doc

            return $
              ExportDecl
                ExportD
                  { expDDecl = extractedDecl
                  , expDPats = []
                  , expDMbDoc = subDoc
                  , expDSubDocs = []
                  , expDInstances = []
                  , expDFixities = fixities
                  , expDSpliced = False
                  }

      findBundledPatterns :: AvailInfo -> IfM m [(HsDecl GhcRn, DocForDecl Name)]
      findBundledPatterns avail = do
        patsyns <- for constructor_names $ \name -> do
          mtyThing <- lookupName name
          case mtyThing of
            Just (AConLike PatSynCon{}) -> do
              export_items <- declWith (Avail name)
              pure
                [ (unLoc patsyn_decl, patsyn_doc)
                | ExportDecl
                    ExportD
                      { expDDecl = patsyn_decl
                      , expDMbDoc = patsyn_doc
                      } <-
                    export_items
                ]
            _ -> pure []
        pure (concat patsyns)
        where
          constructor_names =
            filter isDataConName (availSubordinates avail)

availSubordinates :: AvailInfo -> [Name]
availSubordinates = availSubordinateNames

availNoDocs :: AvailInfo -> [(Name, DocForDecl Name)]
availNoDocs avail =
  zip (availSubordinates avail) (repeat noDocForDecl)

-- | Override 'MetaSince' of a declaration with that of its export if appropriate.
applyExportSince
  :: Map Name MetaSince
  -> Name
  -> DocForDecl Name
  -> DocForDecl Name
applyExportSince exportSinceMap nm (dd, argDoc)
  | Just since <- Map.lookup nm exportSinceMap =
      let dd' = dd{documentationDoc = setMDocSince (documentationDoc dd)}
          setMDocSince :: Maybe (MDoc name) -> Maybe (MDoc name)
          setMDocSince (Just (MetaDoc meta doc)) = Just $ MetaDoc (meta{_metaSince = Just since}) doc
          setMDocSince Nothing = Just $ MetaDoc (Meta{_metaSince = Just since}) DocEmpty
       in (dd', argDoc)
applyExportSince _ _ dd = dd

hiDecl
  :: MonadIO m
  => DynFlags
  -> PrintRuntimeReps
  -> Name
  -> IfM m (Maybe (LHsDecl GhcRn))
hiDecl dflags prr t = do
  mayTyThing <- lookupName t
  case mayTyThing of
    Nothing -> do
      warn $ "Warning: Not found in environment: " ++ pretty dflags t
      return Nothing
    Just x -> case tyThingToLHsDecl prr x of
      Left m -> (warn $ bugWarn m) >> return Nothing
      Right (m, t') -> mapM (warn . bugWarn) m >> return (Just $ L (noAnnSrcSpan (nameSrcSpan t)) t')
  where
    warnLine x =
      O.text "haddock-bug:"
        O.<+> O.text x
        O.<> O.comma
        O.<+> O.quotes (O.ppr t)
        O.<+> O.text "-- Please report this on Haddock issue tracker!"
    bugWarn = showSDoc dflags . warnLine

-- | Lookup docs for a declaration from maps.
lookupDocs
  :: AvailInfo
  -> WarningMap
  -> Map Name (MDoc Name)
  -> ArgMap Name
  -> OccEnv Name
  -> (DocForDecl Name, [(Name, DocForDecl Name)])
  -- ^ documentation for declaration and its subordinates
lookupDocs avail warningMap docMap argMap def_meths_env =
  let
    n = availName avail
    lookupArgDoc x = Map.findWithDefault Map.empty x argMap
    doc = (lookupDoc n, lookupArgDoc n)
    subs = availSubordinates avail
    def_meths =
      [ (meth, (lookupDoc meth, lookupArgDoc meth))
      | s <- subs
      , let dmOcc = mkDefaultMethodOcc (nameOccName s)
      , Just meth <- [lookupOccEnv def_meths_env dmOcc]
      , availExportsDecl avail
      ]
    subDocs =
      [ (s, (lookupDoc s, lookupArgDoc s))
      | s <- subs
      ]
        ++ def_meths
   in
    (doc, subDocs)
  where
    lookupDoc name = Documentation (Map.lookup name docMap) (Map.lookup name warningMap)

-- Note [1]:
------------
-- It is unnecessary to document a subordinate by itself at the top level if
-- any of its parents is also documented. Furthermore, if the subordinate is a
-- record field or a class method, documenting it under its parent
-- indicates its special status.
--
-- A user might expect that it should show up separately, so we issue a
-- warning. It's a fine opportunity to also tell the user she might want to
-- export the subordinate through the parent export item for clarity.
--
-- The code removes top-level subordinates also when the parent is exported
-- through a 'module' export. I think that is fine.
--
-- (For more information, see Trac #69)

-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble
-- together a type signature for it...).
--
-- This function looks through the declarations in this module to try to find
-- the one with the right name.
extractDecl
  :: MonadIO m
  => PrintRuntimeReps
  -> DynFlags
  -> Name
  -- ^ name of the declaration to extract
  -> LHsDecl GhcRn
  -- ^ parent declaration
  -> IfM m (Either String (LHsDecl GhcRn))
extractDecl prr dflags name decl
  | name `elem` getMainDeclBinder emptyOccEnv (unLoc decl) = pure $ Right decl
  | otherwise =
      case unLoc decl of
        TyClD
          _
          d@ClassDecl
            { tcdLName = L _ clsNm
            , tcdSigs = clsSigs
            , tcdATs = clsATs
            } ->
            let
              matchesMethod =
                [ lsig
                | lsig <- clsSigs
                , ClassOpSig _ False _ _ <- pure $ unLoc lsig
                , -- Note: exclude `default` declarations (see #505)
                name `elem` sigName lsig
                ]

              matchesAssociatedType =
                [ lfam_decl
                | lfam_decl <- clsATs
                , name == unLoc (fdLName (unLoc lfam_decl))
                ]
             in
              -- TODO: document fixity
              case (matchesMethod, matchesAssociatedType) of
                ([s0], _) ->
                  let tyvar_names = tyClDeclTyVars d
                      L pos sig = addClassContext clsNm tyvar_names s0
                   in pure (Right $ L pos (SigD noExtField sig))
                (_, [L pos fam_decl]) -> pure (Right $ L pos (TyClD noExtField (FamDecl noExtField fam_decl)))
                ([], []) -> do
                  famInstDeclOpt <- hiDecl dflags prr name
                  case famInstDeclOpt of
                    Nothing ->
                      pure $
                        Left
                          ( concat
                              [ "Ambiguous decl for "
                              , getOccString name
                              , " in class "
                              , getOccString clsNm
                              ]
                          )
                    Just famInstDecl -> extractDecl prr dflags name famInstDecl
                _ ->
                  pure $
                    Left
                      ( concat
                          [ "Ambiguous decl for "
                          , getOccString name
                          , " in class "
                          , getOccString clsNm
                          ]
                      )
        TyClD
          _
          d@DataDecl
            { tcdLName = L _ dataNm
            , tcdDataDefn = HsDataDefn{dd_cons = dataCons}
            } -> pure $ do
            let ty_args = lHsQTyVarsToTypes (tyClDeclTyVars d)
            lsig <-
              if isDataConName name
                then extractPatternSyn name dataNm ty_args (toList dataCons)
                else extractRecSel name dataNm ty_args (toList dataCons)
            pure (SigD noExtField <$> lsig)
        TyClD _ FamDecl{}
          | isValName name -> do
              famInstOpt <- hiDecl dflags prr name
              case famInstOpt of
                Just famInst -> extractDecl prr dflags name famInst
                Nothing -> pure $ Left ("extractDecl: Unhandled decl for " ++ getOccString name)
        InstD
          _
          ( DataFamInstD
              _
              ( DataFamInstDecl
                  ( FamEqn
                      { feqn_tycon = L _ n
                      , feqn_pats = tys
                      , feqn_rhs = defn
                      }
                    )
                )
            ) ->
            pure $
              if isDataConName name
                then fmap (SigD noExtField) <$> extractPatternSyn name n tys (toList $ dd_cons defn)
                else fmap (SigD noExtField) <$> extractRecSel name n tys (toList $ dd_cons defn)
        InstD _ (ClsInstD _ ClsInstDecl{cid_datafam_insts = insts})
          | isDataConName name ->
              let matches =
                    [ d' | L _ d'@(DataFamInstDecl (FamEqn{feqn_rhs = dd})) <- insts, name `elem` map unLoc (concatMap (toList . getConNames . unLoc) (dd_cons dd))
                    ]
               in case matches of
                    [d0] -> extractDecl prr dflags name (noLocA (InstD noExtField (DataFamInstD noExtField d0)))
                    _ -> pure $ Left "internal: extractDecl (ClsInstD)"
          | otherwise ->
              let matches =
                    [ d'
                    | L _ d'@(DataFamInstDecl d) <-
                        insts
                    , -- , L _ ConDecl { con_details = RecCon rec } <- toList $ dd_cons (feqn_rhs d)
                    Just rec <- toList $ getRecConArgs_maybe . unLoc <$> dd_cons (feqn_rhs d)
                    , ConDeclField{cd_fld_names = ns} <- map unLoc (unLoc rec)
                    , L _ n <- ns
                    , foExt n == name
                    ]
               in case matches of
                    [d0] -> extractDecl prr dflags name (noLocA . InstD noExtField $ DataFamInstD noExtField d0)
                    _ -> pure $ Left "internal: extractDecl (ClsInstD)"
        _ -> pure $ Left ("extractDecl: Unhandled decl for " ++ getOccString name)

extractPatternSyn
  :: Name
  -> Name
  -> [LHsTypeArg GhcRn]
  -> [LConDecl GhcRn]
  -> Either String (LSig GhcRn)
extractPatternSyn nm t tvs cons =
  case filter matches cons of
    [] ->
      Left . O.showSDocOneLine O.defaultSDocContext $
        O.text "constructor pattern " O.<+> O.ppr nm O.<+> O.text "not found in type" O.<+> O.ppr t
    con : _ -> pure (extract <$> con)
  where
    matches :: LConDecl GhcRn -> Bool
    matches (L _ con) = nm `elem` (unLoc <$> getConNames con)
    extract :: ConDecl GhcRn -> Sig GhcRn
    extract con =
      let args =
            case con of
              ConDeclH98{con_args = con_args'} -> case con_args' of
                PrefixCon _ args' -> map hsScaledThing args'
                RecCon (L _ fields) -> cd_fld_type . unLoc <$> fields
                InfixCon arg1 arg2 -> map hsScaledThing [arg1, arg2]
              ConDeclGADT{con_g_args = con_args'} -> case con_args' of
                PrefixConGADT _ args' -> map hsScaledThing args'
                RecConGADT _ (L _ fields) -> cd_fld_type . unLoc <$> fields
          typ = longArrow args (data_ty con)
          typ' =
            case con of
              ConDeclH98{con_mb_cxt = Just cxt} -> noLocA (HsQualTy noExtField cxt typ)
              _ -> typ
          typ'' = noLocA (HsQualTy noExtField (noLocA []) typ')
       in PatSynSig noAnn [noLocA nm] (mkEmptySigType typ'')

    longArrow :: [LHsType GhcRn] -> LHsType GhcRn -> LHsType GhcRn
    longArrow inputs output = foldr (\x y -> noLocA (HsFunTy noExtField (HsUnrestrictedArrow noExtField) x y)) output inputs

    data_ty con
      | ConDeclGADT{} <- con = con_res_ty con
      | otherwise = foldl' (\x y -> noLocA (mkAppTyArg x y)) (noLocA (HsTyVar noAnn NotPromoted (noLocA t))) tvs
      where
        mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
        mkAppTyArg f (HsValArg _ ty) = HsAppTy noExtField f ty
        mkAppTyArg f (HsTypeArg _ ki) = HsAppKindTy noExtField f ki
        mkAppTyArg f (HsArgPar _) = HsParTy noAnn f

extractRecSel
  :: Name
  -> Name
  -> [LHsTypeArg GhcRn]
  -> [LConDecl GhcRn]
  -> Either String (LSig GhcRn)
extractRecSel _ _ _ [] = Left "extractRecSel: selector not found"
extractRecSel nm t tvs (L _ con : rest) =
  case getRecConArgs_maybe con of
    Just (L _ fields)
      | ((l, L _ (ConDeclField _ _nn ty _)) : _) <- matching_fields fields ->
          pure (L (noAnnSrcSpan l) (TypeSig noAnn [noLocA nm] (mkEmptyWildCardBndrs $ mkEmptySigType (noLocA (HsFunTy noExtField (HsUnrestrictedArrow noExtField) data_ty (getBangType ty))))))
    _ -> extractRecSel nm t tvs rest
  where
    matching_fields :: [LConDeclField GhcRn] -> [(SrcSpan, LConDeclField GhcRn)]
    matching_fields flds =
      [ (locA l, f) | f@(L _ (ConDeclField _ ns _ _)) <- flds, L l n <- ns, foExt n == nm
      ]
    data_ty
      -- ResTyGADT _ ty <- con_res con = ty
      | ConDeclGADT{} <- con = con_res_ty con
      | otherwise = foldl' (\x y -> noLocA (mkAppTyArg x y)) (noLocA (HsTyVar noAnn NotPromoted (noLocA t))) tvs
      where
        mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
        mkAppTyArg f (HsValArg _ ty) = HsAppTy noExtField f ty
        mkAppTyArg f (HsTypeArg _ ki) = HsAppKindTy noExtField f ki
        mkAppTyArg f (HsArgPar _) = HsParTy noAnn f

-- | Keep export items with docs.
pruneExportItems :: [ExportItem GhcRn] -> [ExportItem GhcRn]
pruneExportItems = filter hasDoc
  where
    hasDoc (ExportDecl ExportD{expDMbDoc = (Documentation d _, _)}) = isJust d
    hasDoc _ = True

mkVisibleNames :: InstMap -> [ExportItem GhcRn] -> [DocOption] -> [Name]
mkVisibleNames instMap exports opts
  | OptHide `elem` opts = []
  | otherwise =
      let ns = concatMap exportName exports
       in seqList ns `seq` ns
  where
    exportName (ExportDecl e@ExportD{}) = name ++ subs ++ patsyns
      where
        subs = map fst (expDSubDocs e)
        patsyns = concatMap (getMainDeclBinder emptyOccEnv . fst) (expDPats e)
        name = case unLoc $ expDDecl e of
          InstD _ d -> maybeToList $ SrcLoc.lookupSrcSpan (getInstLoc d) instMap
          decl -> getMainDeclBinder emptyOccEnv decl
    exportName ExportNoDecl{} = [] -- we don't count these as visible, since
    -- we don't want links to go to them.
    exportName _ = []

seqList :: [a] -> ()
seqList [] = ()
seqList (x : xs) = x `seq` seqList xs
