{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}
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
-----------------------------------------------------------------------------
module Haddock.Interface.Create (IfM, runIfM, createInterface1) where

import Documentation.Haddock.Doc (metaDocAppend)
import Haddock.Convert (PrintRuntimeReps (..), tyThingToLHsDecl)
import Haddock.GhcUtils (addClassContext, filterSigNames, lHsQTyVarsToTypes, mkEmptySigType, moduleString, parents,
                         pretty, restrictTo, sigName, unL)
import Haddock.Interface.LexParseRn
import Haddock.Options (Flag (..), modulePackageInfo)
import Haddock.Types hiding (liftErrMsg)
import Haddock.Utils (replace)

import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadReader (..), ReaderT, asks, runReaderT)
import Control.Monad.Writer.Strict hiding (tell)
import Data.Bitraversable (bitraverse)
import Data.List (find, foldl')
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe, maybeToList)
import Data.Traversable (for)

import GHC hiding (lookupName)
import GHC.Core.Class (ClassMinimalDef, classMinimalDef)
import GHC.Core.ConLike (ConLike (..))
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Ppr (showSDoc)
import GHC.HsToCore.Docs hiding (mkMaps, unionArgMaps)
import GHC.IORef (readIORef)
import GHC.Stack (HasCallStack)
import GHC.Tc.Types hiding (IfM)
import GHC.Tc.Utils.Monad (finalSafeMode)
import GHC.Types.Avail hiding (avail)
import qualified GHC.Types.Avail as Avail
import GHC.Types.Basic (PromotionFlag (..))
import GHC.Types.Name (getOccString, getSrcSpan, isDataConName, isValName, nameIsLocalOrFrom, nameOccName, emptyOccEnv)
import GHC.Types.Name.Env (lookupNameEnv)
import GHC.Types.Name.Reader (GlobalRdrEnv, greMangledName, lookupGlobalRdrEnv)
import GHC.Types.Name.Set (elemNameSet, mkNameSet)
import GHC.Types.SourceFile (HscSource (..))
import GHC.Types.SourceText (SourceText (..), sl_fs)
import GHC.Unit.Types
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Unit.Module as Module
import GHC.Unit.Module.ModSummary (msHsFilePath)
import GHC.Unit.State (PackageName (..), UnitState, lookupModuleInAllUnits)
import qualified GHC.Utils.Outputable as O
import GHC.Utils.Panic (pprPanic)
import GHC.Unit.Module.Warnings
import GHC.Types.Unique.Map

newtype IfEnv m = IfEnv
  {
    -- | Lookup names in the enviroment.
    ife_lookup_name :: Name -> m (Maybe TyThing)
  }


-- | A monad in which we create Haddock interfaces. Not to be confused with
-- `GHC.Tc.Types.IfM` which is used to write GHC interfaces.
--
-- In the past `createInterface` was running in the `Ghc` monad but proved hard
-- to sustain as soon as we moved over for Haddock to be a plugin. Also abstracting
-- over the Ghc specific clarifies where side effects happen.
newtype IfM m a = IfM { unIfM :: ReaderT (IfEnv m) (WriterT [ErrMsg] m) a }


deriving newtype instance Functor m => Functor (IfM m)
deriving newtype instance Applicative m => Applicative (IfM m)
deriving newtype instance Monad m => Monad (IfM m)
deriving newtype instance MonadIO m => MonadIO (IfM m)
deriving newtype instance Monad m => MonadReader (IfEnv m) (IfM m)
deriving newtype instance Monad m => MonadWriter [ErrMsg] (IfM m)


-- | Run an `IfM` action.
runIfM
  -- | Lookup a global name in the current session. Used in cases
  -- where declarations don't
  :: (Name -> m (Maybe TyThing))
  -- | The action to run.
  -> IfM m a
  -- | Result and accumulated error/warning messages.
  -> m (a, [ErrMsg])
runIfM lookup_name action = do
  let
    if_env = IfEnv
      {
        ife_lookup_name = lookup_name
      }
  runWriterT (runReaderT (unIfM action) if_env)


liftErrMsg :: Monad m => ErrMsgM a -> IfM m a
liftErrMsg action = do
  writer (runWriter action)


lookupName :: Monad m => Name -> IfM m (Maybe TyThing)
lookupName name = IfM $ do
  lookup_name <- asks ife_lookup_name
  lift $ lift (lookup_name name)


createInterface1
  :: MonadIO m
  => [Flag]
  -> UnitState
  -> ModSummary
  -> TcGblEnv
  -> IfaceMap
  -> InstIfaceMap
  -> IfM m Interface
createInterface1 flags unit_state mod_sum tc_gbl_env ifaces inst_ifaces = do

  let
    ModSummary
      {
        -- Cached flags from OPTIONS, INCLUDE and LANGUAGE
        -- pragmas in the modules source code. Used to infer
        -- safety of module.
        ms_hspp_opts
      , ms_location = ModLocation
        {
          ml_hie_file
        }
      } = mod_sum

    TcGblEnv
      {
        tcg_mod
      , tcg_src
      , tcg_semantic_mod
      , tcg_rdr_env
      , tcg_exports
      , tcg_insts
      , tcg_fam_insts
      , tcg_warns

      -- Renamed source
      , tcg_rn_imports
      , tcg_rn_exports
      , tcg_rn_decls

      , tcg_th_docs
      , tcg_doc_hdr
      } = tc_gbl_env

    dflags = ms_hspp_opts

    is_sig = tcg_src == HsigFile

    (pkg_name_fs, _) =
      modulePackageInfo unit_state flags (Just tcg_mod)

    pkg_name :: Maybe Package
    pkg_name =
      let
        unpack (PackageName name) = unpackFS name
      in
        fmap unpack pkg_name_fs

    fixities :: FixMap
    fixities = case tcg_rn_decls of
      Nothing -> mempty
      Just dx -> mkFixMap dx

    -- Locations of all the TH splices
    loc_splices :: [SrcSpan]
    loc_splices = case tcg_rn_decls of
      Nothing -> []
      Just HsGroup { hs_splcds } -> [ locA loc | L loc _ <- hs_splcds ]

  decls <- case tcg_rn_decls of
    Nothing -> do
      tell [ "Warning: Renamed source is not available" ]
      pure []
    Just dx ->
      pure (topDecls dx)

  -- Derive final options to use for haddocking this module
  doc_opts <- liftErrMsg $ mkDocOpts (haddockOptions ms_hspp_opts) flags tcg_mod

  let
    -- All elements of an explicit export list, if present
    export_list :: Maybe [(IE GhcRn, Avails)]
    export_list
      | OptIgnoreExports `elem` doc_opts  =
          Nothing
      | Just rn_exports <- tcg_rn_exports =
          Just [ (ie, avail) | (L _ ie, avail) <- rn_exports ]
      | otherwise =
          Nothing

    -- All the exported Names of this module.
    exported_names :: [Name]
    exported_names =
      concatMap availNamesWithSelectors tcg_exports

    -- Module imports of the form `import X`. Note that there is
    -- a) no qualification and
    -- b) no import list
    imported_modules :: Map ModuleName [ModuleName]
    imported_modules
      | Just{} <- export_list =
          unrestrictedModuleImports (map unLoc tcg_rn_imports)
      | otherwise =
          M.empty

    -- TyThings that have instances defined in this module
    local_instances :: [Name]
    local_instances =
      [ name
      | name <- map getName tcg_insts ++ map getName tcg_fam_insts
      , nameIsLocalOrFrom tcg_semantic_mod name
      ]

  -- Infer module safety
  safety   <- liftIO (finalSafeMode ms_hspp_opts tc_gbl_env)

  -- The docs added via Template Haskell's putDoc
  thDocs@ExtractedTHDocs { ethd_mod_header = thMbDocStr } <-
    liftIO $ extractTHDocs <$> readIORef tcg_th_docs

  -- Process the top-level module header documentation.
  (!info, header_doc) <- liftErrMsg $ processModuleHeader dflags pkg_name
    tcg_rdr_env safety (fmap hsDocString thMbDocStr <|> (hsDocString . unLoc <$> tcg_doc_hdr))

  -- Warnings on declarations in this module
  decl_warnings <- liftErrMsg (mkWarningMap dflags tcg_warns tcg_rdr_env exported_names)

  -- Warning on the module header
  mod_warning <- liftErrMsg (moduleWarning dflags tcg_rdr_env tcg_warns)

  let
    -- Warnings in this module and transitive warnings from dependend modules
    warnings :: Map Name (Doc Name)
    warnings = M.unions (decl_warnings : map ifaceWarningMap (M.elems ifaces))

  maps@(!docs, !arg_docs, !decl_map, _) <-
    liftErrMsg (mkMaps dflags pkg_name tcg_rdr_env local_instances decls thDocs)

  export_items <- mkExportItems is_sig ifaces pkg_name tcg_mod tcg_semantic_mod
    warnings tcg_rdr_env exported_names (map fst decls) maps fixities
    imported_modules loc_splices export_list tcg_exports inst_ifaces dflags

  let
    visible_names :: [Name]
    visible_names = mkVisibleNames maps export_items doc_opts

    -- Measure haddock documentation coverage.
    pruned_export_items :: [ExportItem GhcRn]
    pruned_export_items = pruneExportItems export_items

    !haddockable = 1 + length export_items -- module + exports
    !haddocked = (if isJust tcg_doc_hdr then 1 else 0) + length pruned_export_items

    coverage :: (Int, Int)
    !coverage = (haddockable, haddocked)

    aliases :: Map Module ModuleName
    aliases = mkAliasMap unit_state tcg_rn_imports

  return $! Interface
    {
      ifaceMod               = tcg_mod
    , ifaceIsSig             = is_sig
    , ifaceOrigFilename      = msHsFilePath mod_sum
    , ifaceHieFile           = Just ml_hie_file
    , ifaceInfo              = info
    , ifaceDoc               = Documentation header_doc mod_warning
    , ifaceRnDoc             = Documentation Nothing Nothing
    , ifaceOptions           = doc_opts
    , ifaceDocMap            = docs
    , ifaceArgMap            = arg_docs
    , ifaceRnDocMap          = M.empty
    , ifaceRnArgMap          = M.empty
    , ifaceExportItems       = if OptPrune `elem` doc_opts then
                                 pruned_export_items else export_items
    , ifaceRnExportItems     = []
    , ifaceExports           = exported_names
    , ifaceVisibleExports    = visible_names
    , ifaceDeclMap           = decl_map
    , ifaceFixMap            = fixities
    , ifaceModuleAliases     = aliases
    , ifaceInstances         = tcg_insts
    , ifaceFamInstances      = tcg_fam_insts
    , ifaceOrphanInstances   = [] -- Filled in attachInstances
    , ifaceRnOrphanInstances = [] -- Filled in attachInstances
    , ifaceHaddockCoverage   = coverage
    , ifaceWarningMap        = warnings
    , ifaceDynFlags          = dflags
    }


-- | Given all of the @import M as N@ declarations in a package,
-- create a mapping from the module identity of M, to an alias N
-- (if there are multiple aliases, we pick the last one.)  This
-- will go in 'ifaceModuleAliases'.
mkAliasMap :: UnitState -> [LImportDecl GhcRn] -> M.Map Module ModuleName
mkAliasMap state impDecls =
  M.fromList $
  mapMaybe (\(SrcLoc.L _ impDecl) -> do
    SrcLoc.L _ alias <- ideclAs impDecl
    return $
      (lookupModuleDyn state
         -- TODO: This is supremely dodgy, because in general the
         -- UnitId isn't going to look anything like the package
         -- qualifier (even with old versions of GHC, the
         -- IPID would be p-0.1, but a package qualifier never
         -- has a version number it.  (Is it possible that in
         -- Haddock-land, the UnitIds never have version numbers?
         -- I, ezyang, have not quite understand Haddock's package
         -- identifier model.)
         --
         -- Additionally, this is simulating some logic GHC already
         -- has for deciding how to qualify names when it outputs
         -- them to the user.  We should reuse that information;
         -- or at least reuse the renamed imports, which know what
         -- they import!
         (ideclPkgQual impDecl)
         (case ideclName impDecl of SrcLoc.L _ name -> name),
       alias))
    impDecls

-- We want to know which modules are imported without any qualification. This
-- way we can display module reexports more compactly. This mapping also looks
-- through aliases:
--
-- module M (module X) where
--   import M1 as X
--   import M2 as X
--
-- With our mapping we know that we can display exported modules M1 and M2.
--
unrestrictedModuleImports :: [ImportDecl GhcRn] -> M.Map ModuleName [ModuleName]
unrestrictedModuleImports idecls =
  M.map (map (unLoc . ideclName))
  $ M.filter (all isInteresting) impModMap
  where
    impModMap =
      M.fromListWith (++) (concatMap moduleMapping idecls)

    moduleMapping idecl =
      concat [ [ (unLoc (ideclName idecl), [idecl]) ]
             , [ (unLoc mod_name, [idecl])
               | Just mod_name <- [ideclAs idecl]
               ]
             ]

    isInteresting idecl =
      case ideclImportList idecl of
        -- i) no subset selected
        Nothing             -> True
        -- ii) an import with a hiding clause
        -- without any names
        Just (EverythingBut, L _ []) -> True
        -- iii) any other case of qualification
        _                   -> False

-- Similar to GHC.lookupModule
-- ezyang: Not really...
lookupModuleDyn ::
  UnitState -> PkgQual -> ModuleName -> Module
lookupModuleDyn state pkg_qual mdlName = case pkg_qual of
  OtherPkg uid -> Module.mkModule (RealUnit (Definite uid)) mdlName
  ThisPkg uid  -> Module.mkModule (RealUnit (Definite uid)) mdlName
  NoPkgQual    -> case lookupModuleInAllUnits state mdlName of
    (m,_):_ -> m
    [] -> Module.mkModule Module.mainUnit mdlName


-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

mkWarningMap :: DynFlags -> Warnings a -> GlobalRdrEnv -> [Name] -> ErrMsgM WarningMap
mkWarningMap dflags warnings gre exps = case warnings of
  NoWarnings  -> pure M.empty
  WarnAll _   -> pure M.empty
  WarnSome ws ->
    let ws' = [ (n, w)
              | (occ, w) <- ws
              , elt <- lookupGlobalRdrEnv gre occ
              , let n = greMangledName elt, n `elem` exps ]
    in M.fromList <$> traverse (bitraverse pure (parseWarning dflags gre)) ws'

moduleWarning :: DynFlags -> GlobalRdrEnv -> Warnings a -> ErrMsgM (Maybe (Doc Name))
moduleWarning _ _ NoWarnings = pure Nothing
moduleWarning _ _ (WarnSome _) = pure Nothing
moduleWarning dflags gre (WarnAll w) = Just <$> parseWarning dflags gre w

parseWarning :: DynFlags -> GlobalRdrEnv -> WarningTxt a -> ErrMsgM (Doc Name)
parseWarning dflags gre w = case w of
  DeprecatedTxt _ msg -> format "Deprecated: " (foldMap (unpackFS . sl_fs . hsDocString . unLoc) msg)
  WarningTxt    _ msg -> format "Warning: "    (foldMap (unpackFS . sl_fs . hsDocString . unLoc) msg)
  where
    format x bs = DocWarning . DocParagraph . DocAppend (DocString x)
                  <$> processDocStringFromString dflags gre bs


-------------------------------------------------------------------------------
-- Doc options
--
-- Haddock options that are embedded in the source file
-------------------------------------------------------------------------------


mkDocOpts :: Maybe String -> [Flag] -> Module -> ErrMsgM [DocOption]
mkDocOpts mbOpts flags mdl = do
  opts <- case mbOpts of
    Just opts -> case words $ replace ',' ' ' opts of
      [] -> tell ["No option supplied to DOC_OPTION/doc_option"] >> return []
      xs -> liftM catMaybes (mapM parseOption xs)
    Nothing -> return []
  pure (foldl go opts flags)
  where
    mdlStr = moduleString mdl

    -- Later flags override earlier ones
    go os m | m == Flag_HideModule mdlStr     = OptHide : os
            | m == Flag_ShowModule mdlStr     = filter (/= OptHide) os
            | m == Flag_ShowAllModules        = filter (/= OptHide) os
            | m == Flag_IgnoreAllExports      = OptIgnoreExports : os
            | m == Flag_ShowExtensions mdlStr = OptIgnoreExports : os
            | otherwise                       = os

parseOption :: String -> ErrMsgM (Maybe DocOption)
parseOption "hide"            = return (Just OptHide)
parseOption "prune"           = return (Just OptPrune)
parseOption "ignore-exports"  = return (Just OptIgnoreExports)
parseOption "not-home"        = return (Just OptNotHome)
parseOption "show-extensions" = return (Just OptShowExtensions)
parseOption other = tell ["Unrecognised option: " ++ other] >> return Nothing


--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------


type Maps = (DocMap Name, ArgMap Name, DeclMap, InstMap)

-- | Create 'Maps' by looping through the declarations. For each declaration,
-- find its names, its subordinates, and its doc strings. Process doc strings
-- into 'Doc's.
mkMaps :: DynFlags
       -> Maybe Package  -- this package
       -> GlobalRdrEnv
       -> [Name]
       -> [(LHsDecl GhcRn, [HsDoc GhcRn])]
       -> ExtractedTHDocs -- ^ Template Haskell putDoc docs
       -> ErrMsgM Maps
mkMaps dflags pkgName gre instances decls thDocs = do
  (a, b, c) <- unzip3 <$> traverse mappings decls
  (th_a, th_b) <- thMappings
  pure ( th_a `M.union` f' (map (nubByName fst) a)
       , fmap intmap2mapint $
           th_b `unionArgMaps` (f (filterMapping (not . IM.null) b))
       , f  (filterMapping (not . null) c)
       , instanceMap
       )
  where
    f :: (Ord a, Monoid b) => [[(a, b)]] -> Map a b
    f = M.fromListWith (<>) . concat

    f' :: [[(Name, MDoc Name)]] -> Map Name (MDoc Name)
    f' = M.fromListWith metaDocAppend . concat

    filterMapping :: (b -> Bool) ->  [[(a, b)]] -> [[(a, b)]]
    filterMapping p = map (filter (p . snd))

    -- Convert IntMap -> IntMap
    -- TODO: should ArgMap eventually be switched over to IntMap?
    intmap2mapint = M.fromList . IM.toList

    -- | Extract the mappings from template haskell.
    -- No DeclMap/InstMap is needed since we already have access to the
    -- doc strings
    thMappings :: ErrMsgM (Map Name (MDoc Name), Map Name (IntMap (MDoc Name)))
    thMappings = do
      let ExtractedTHDocs
            _
            declDocs
            argDocs
            instDocs = thDocs
          ds2mdoc :: (HsDoc GhcRn) -> ErrMsgM (MDoc Name)
          ds2mdoc = processDocStringParas dflags pkgName gre . hsDocString

      let cvt = M.fromList . nonDetEltsUniqMap

      declDocs' <- mapM ds2mdoc (cvt declDocs)
      argDocs'  <- mapM (mapM ds2mdoc) (cvt argDocs)
      instDocs' <- mapM ds2mdoc (cvt instDocs)
      return (declDocs' <> instDocs', argDocs')


    mappings :: (LHsDecl GhcRn, [HsDoc GhcRn])
             -> ErrMsgM ( [(Name, MDoc Name)]
                        , [(Name, IntMap (MDoc Name))]
                        , [(Name,  [LHsDecl GhcRn])]
                        )
    mappings (ldecl@(L (SrcSpanAnn _ (RealSrcSpan l _)) decl), hs_docStrs) = do
      let docStrs = map hsDocString hs_docStrs
          declDoc :: [HsDocString] -> IntMap HsDocString
                  -> ErrMsgM (Maybe (MDoc Name), IntMap (MDoc Name))
          declDoc strs m = do
            doc' <- processDocStrings dflags pkgName gre strs
            m'   <- traverse (processDocStringParas dflags pkgName gre) m
            pure (doc', m')

      (doc, args) <- declDoc docStrs (fmap hsDocString (declTypeDocs decl))

      let
          subs :: [(Name, [HsDocString], IntMap HsDocString)]
          subs = map (\(n, ds, im) -> (n, map hsDocString ds, fmap hsDocString im))
                  $ subordinates emptyOccEnv instanceMap decl

      (subDocs, subArgs) <- unzip <$> traverse (\(_, strs, m) -> declDoc strs m) subs

      let
          ns = names l decl
          subNs = [ n | (n, _, _) <- subs ]
          dm = [ (n, d) | (n, Just d) <- zip ns (repeat doc) ++ zip subNs subDocs ]
          am = [ (n, args) | n <- ns ] ++ zip subNs subArgs
          cm = [ (n, [ldecl]) | n <- ns ++ subNs ]

      seqList ns `seq`
        seqList subNs `seq`
        doc `seq`
        seqList subDocs `seq`
        seqList subArgs `seq`
        pure (dm, am, cm)
    mappings (L (SrcSpanAnn _ (UnhelpfulSpan _)) _, _) = pure ([], [], [])

    instanceMap :: Map RealSrcSpan Name
    instanceMap = M.fromList [(l, n) | n <- instances, RealSrcSpan l _ <- [getSrcSpan n] ]

    names :: RealSrcSpan -> HsDecl GhcRn -> [Name]
    names _ (InstD _ d) = maybeToList (SrcLoc.lookupSrcSpan loc instanceMap) -- See note [2].
      where loc = case d of
              -- The CoAx's loc is the whole line, but only for TFs. The
              -- workaround is to dig into the family instance declaration and
              -- get the identifier with the right location.
              TyFamInstD _ (TyFamInstDecl _ d') -> getLocA (feqn_tycon d')
              _ -> getInstLoc d
    names l (DerivD {}) = maybeToList (M.lookup l instanceMap) -- See note [2].
    names _ decl = getMainDeclBinder emptyOccEnv decl

-- | Unions together two 'ArgDocMaps' (or ArgMaps in haddock-api), such that two
-- maps with values for the same key merge the inner map as well.
-- Left biased so @unionArgMaps a b@ prefers @a@ over @b@.

unionArgMaps :: forall b . Map Name (IntMap b)
             -> Map Name (IntMap b)
             -> Map Name (IntMap b)
unionArgMaps a b = M.foldrWithKey go b a
  where
    go :: Name -> IntMap b
            -> Map Name (IntMap b) -> Map Name (IntMap b)
    go n newArgMap acc
      | Just oldArgMap <- M.lookup n acc =
          M.insert n (newArgMap `IM.union` oldArgMap) acc
      | otherwise = M.insert n newArgMap acc

-- Note [2]:
------------
-- We relate ClsInsts to InstDecls and DerivDecls using the SrcSpans buried
-- inside them. That should work for normal user-written instances (from
-- looking at GHC sources). We can assume that commented instances are
-- user-written. This lets us relate Names (from ClsInsts) to comments
-- (associated with InstDecls and DerivDecls).

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------



-- | Extract a map of fixity declarations only
mkFixMap :: HsGroup GhcRn -> FixMap
mkFixMap group_ =
  M.fromList [ (n,f)
             | L _ (FixitySig _ ns f) <- hsGroupTopLevelFixitySigs group_,
               L _ n <- ns ]


-- | Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.
--
-- We create the export items even if the module is hidden, since they
-- might be useful when creating the export items for other modules.
mkExportItems
  :: Monad m
  => Bool               -- is it a signature
  -> IfaceMap
  -> Maybe Package      -- this package
  -> Module             -- this module
  -> Module             -- semantic module
  -> WarningMap
  -> GlobalRdrEnv
  -> [Name]             -- exported names (orig)
  -> [LHsDecl GhcRn]    -- renamed source declarations
  -> Maps
  -> FixMap
  -> M.Map ModuleName [ModuleName]
  -> [SrcSpan]          -- splice locations
  -> Maybe [(IE GhcRn, Avails)]
  -> Avails             -- exported stuff from this module
  -> InstIfaceMap
  -> DynFlags
  -> IfM m [ExportItem GhcRn]
mkExportItems
  is_sig modMap pkgName thisMod semMod warnings gre exportedNames decls
  maps fixMap unrestricted_imp_mods splices exportList allExports
  instIfaceMap dflags =
  case exportList of
    Nothing      ->
      fullModuleContents is_sig modMap pkgName thisMod semMod warnings gre
        exportedNames decls maps fixMap splices instIfaceMap dflags
        allExports
    Just exports -> liftM concat $ mapM lookupExport exports
  where
    lookupExport (IEGroup _ lev docStr, _)  = liftErrMsg $ do
      doc <- processDocString dflags gre (hsDocString . unLoc $ docStr)
      return [ExportGroup lev "" doc]

    lookupExport (IEDoc _ docStr, _)        = liftErrMsg $ do
      doc <- processDocStringParas dflags pkgName gre (hsDocString . unLoc $ docStr)
      return [ExportDoc doc]

    lookupExport (IEDocNamed _ str, _)      = liftErrMsg $
      findNamedDoc str [ unL d | d <- decls ] >>= \case
        Nothing -> return  []
        Just docStr -> do
          doc <- processDocStringParas dflags pkgName gre docStr
          return [ExportDoc doc]

    lookupExport (IEModuleContents _ (L _ mod_name), _)
      -- only consider exporting a module if we are sure we
      -- are really exporting the whole module and not some
      -- subset. We also look through module aliases here.
      | Just mods <- M.lookup mod_name unrestricted_imp_mods
      , not (null mods)
      = concat <$> traverse (moduleExport thisMod dflags modMap instIfaceMap) mods

    lookupExport (_, avails) =
      concat <$> traverse availExport (nubAvails avails)

    availExport avail =
      availExportItem is_sig modMap thisMod semMod warnings exportedNames
        maps fixMap splices instIfaceMap dflags avail


-- Extract the minimal complete definition of a Name, if one exists
minimalDef :: Monad m => Name -> IfM m (Maybe ClassMinimalDef)
minimalDef n = do
  mty <- lookupName n
  case mty of
    Just (ATyCon (tyConClass_maybe -> Just c)) ->
      return . Just $ classMinimalDef c
    _ ->
      return Nothing


availExportItem
  :: forall m
  .  Monad m
  => Bool               -- is it a signature
  -> IfaceMap
  -> Module             -- this module
  -> Module             -- semantic module
  -> WarningMap
  -> [Name]             -- exported names (orig)
  -> Maps
  -> FixMap
  -> [SrcSpan]          -- splice locations
  -> InstIfaceMap
  -> DynFlags
  -> AvailInfo
  -> IfM m [ExportItem GhcRn]
availExportItem is_sig modMap thisMod semMod warnings exportedNames
  (docMap, argMap, declMap, _) fixMap splices instIfaceMap
  dflags availInfo = declWith availInfo
  where
    declWith :: AvailInfo -> IfM m [ ExportItem GhcRn ]
    declWith avail = do
      let t = availName avail
      r    <- findDecl avail
      case r of
        ([L l' (ValD _ _)], (doc, _)) -> do
          let l = locA l'
          -- Top-level binding without type signature
          export <- hiValExportItem dflags t l doc (l `elem` splices) $ M.lookup t fixMap
          return [export]
        (ds, docs_) | decl : _ <- filter (not . isValD . unLoc) ds ->
          let declNames = getMainDeclBinder emptyOccEnv (unL decl)
          in case () of
            _
              -- We should not show a subordinate by itself if any of its
              -- parents is also exported. See note [1].
              | t `notElem` declNames,
                Just p <- find isExported (parents t $ unL decl) ->
                do liftErrMsg $ tell [
                     "Warning: " ++ moduleString thisMod ++ ": " ++
                     pretty dflags (nameOccName t) ++ " is exported separately but " ++
                     "will be documented under " ++ pretty dflags (nameOccName p) ++
                     ". Consider exporting it together with its parent(s)" ++
                     " for code clarity." ]
                   return []

              -- normal case
              | otherwise -> case decl of
                  -- A single signature might refer to many names, but we
                  -- create an export item for a single name only.  So we
                  -- modify the signature to contain only that single name.
                  L loc (SigD _ sig) ->
                    -- fromJust is safe since we already checked in guards
                    -- that 't' is a name declared in this declaration.
                    let newDecl = L loc . SigD noExtField . fromJust $ filterSigNames (== t) sig
                    in availExportDecl avail newDecl docs_

                  L loc (TyClD _ ClassDecl {..}) -> do
                    mdef <- minimalDef t
                    let sig = maybeToList $ fmap (noLocA . MinimalSig noAnn NoSourceText . noLocA . fmap noLocA) mdef
                    availExportDecl avail
                      (L loc $ TyClD noExtField ClassDecl { tcdSigs = sig ++ tcdSigs, .. }) docs_

                  _ -> availExportDecl avail decl docs_

        -- Declaration from another package
        ([], _) -> do
          mayDecl <- hiDecl dflags t
          case mayDecl of
            Nothing -> return [ ExportNoDecl t [] ]
            Just decl ->
              -- We try to get the subs and docs
              -- from the installed .haddock file for that package.
              -- TODO: This needs to be more sophisticated to deal
              -- with signature inheritance
              case M.lookup (nameModule t) instIfaceMap of
                Nothing -> do
                   liftErrMsg $ tell
                      ["Warning: Couldn't find .haddock for export " ++ pretty dflags t]
                   let subs_ = availNoDocs avail
                   availExportDecl avail decl (noDocForDecl, subs_)
                Just iface ->
                  availExportDecl avail decl (lookupDocs avail warnings (instDocMap iface) (instArgMap iface))

        _ -> return []

    -- Tries 'extractDecl' first then falls back to 'hiDecl' if that fails
    availDecl :: Name -> LHsDecl GhcRn -> IfM m (LHsDecl GhcRn)
    availDecl declName parentDecl =
      case extractDecl declMap declName parentDecl of
        Right d -> pure d
        Left err -> do
          synifiedDeclOpt <- hiDecl dflags declName
          case synifiedDeclOpt of
            Just synifiedDecl -> pure synifiedDecl
            Nothing -> pprPanic "availExportItem" (O.text err)

    availExportDecl :: AvailInfo -> LHsDecl GhcRn
                    -> (DocForDecl Name, [(Name, DocForDecl Name)])
                    -> IfM m [ ExportItem GhcRn ]
    availExportDecl avail decl (doc, subs)
      | availExportsDecl avail = do
          extractedDecl <- availDecl (availName avail) decl

          -- bundled pattern synonyms only make sense if the declaration is
          -- exported (otherwise there would be nothing to bundle to)
          bundledPatSyns <- findBundledPatterns avail

          let
            patSynNames =
              concatMap (getMainDeclBinder emptyOccEnv . fst) bundledPatSyns

            fixities =
                [ (n, f)
                | n <- availName avail : fmap fst subs ++ patSynNames
                , Just f <- [M.lookup n fixMap]
                ]

          return [ ExportDecl {
                       expItemDecl      = restrictTo (fmap fst subs) extractedDecl
                     , expItemPats      = bundledPatSyns
                     , expItemMbDoc     = doc
                     , expItemSubDocs   = subs
                     , expItemInstances = []
                     , expItemFixities  = fixities
                     , expItemSpliced   = False
                     }
                 ]

      | otherwise = for subs $ \(sub, sub_doc) -> do
          extractedDecl <- availDecl sub decl

          return ( ExportDecl {
                       expItemDecl      = extractedDecl
                     , expItemPats      = []
                     , expItemMbDoc     = sub_doc
                     , expItemSubDocs   = []
                     , expItemInstances = []
                     , expItemFixities  = [ (sub, f) | Just f <- [M.lookup sub fixMap] ]
                     , expItemSpliced   = False
                     } )

    exportedNameSet = mkNameSet exportedNames
    isExported n = elemNameSet n exportedNameSet

    findDecl :: AvailInfo -> IfM m ([LHsDecl GhcRn], (DocForDecl Name, [(Name, DocForDecl Name)]))
    findDecl avail
      | m == semMod =
          case M.lookup n declMap of
            Just ds -> return (ds, lookupDocs avail warnings docMap argMap)
            Nothing
              | is_sig -> do
                -- OK, so it wasn't in the local declaration map.  It could
                -- have been inherited from a signature.  Reconstitute it
                -- from the type.
                mb_r <- hiDecl dflags n
                case mb_r of
                    Nothing -> return ([], (noDocForDecl, availNoDocs avail))
                    -- TODO: If we try harder, we might be able to find
                    -- a Haddock!  Look in the Haddocks for each thing in
                    -- requirementContext (unitState)
                    Just decl -> return ([decl], (noDocForDecl, availNoDocs avail))
              | otherwise ->
                return ([], (noDocForDecl, availNoDocs avail))
      | Just iface <- M.lookup (semToIdMod (moduleUnit thisMod) m) modMap
      , Just ds <- M.lookup n (ifaceDeclMap iface) =
          return (ds, lookupDocs avail warnings
                            (ifaceDocMap iface)
                            (ifaceArgMap iface))
      | otherwise = return ([], (noDocForDecl, availNoDocs avail))
      where
        n = availName avail
        m = nameModule n

    findBundledPatterns :: AvailInfo -> IfM m [(HsDecl GhcRn, DocForDecl Name)]
    findBundledPatterns avail = do
      patsyns <- for constructor_names $ \name -> do
        mtyThing <- lookupName name
        case mtyThing of
          Just (AConLike PatSynCon{}) -> do
            export_items <- declWith (Avail.avail name)
            pure [ (unLoc patsyn_decl, patsyn_doc)
                 | ExportDecl {
                       expItemDecl  = patsyn_decl
                     , expItemMbDoc = patsyn_doc
                     } <- export_items
                 ]
          _ -> pure []
      pure (concat patsyns)
      where
        constructor_names =
          filter isDataConName (availSubordinates avail)

availSubordinates :: AvailInfo -> [Name]
availSubordinates = map greNameMangledName . availSubordinateGreNames

availNoDocs :: AvailInfo -> [(Name, DocForDecl Name)]
availNoDocs avail =
  zip (availSubordinates avail) (repeat noDocForDecl)

-- | Given a 'Module' from a 'Name', convert it into a 'Module' that
-- we can actually find in the 'IfaceMap'.
semToIdMod :: Unit -> Module -> Module
semToIdMod this_uid m
    | Module.isHoleModule m = mkModule this_uid (moduleName m)
    | otherwise             = m

hiDecl :: Monad m => DynFlags -> Name -> IfM m (Maybe (LHsDecl GhcRn))
hiDecl dflags t = do
  mayTyThing <- lookupName t
  case mayTyThing of
    Nothing -> do
      liftErrMsg $ tell ["Warning: Not found in environment: " ++ pretty dflags t]
      return Nothing
    Just x -> case tyThingToLHsDecl ShowRuntimeRep x of
      Left m -> liftErrMsg (tell [bugWarn m]) >> return Nothing
      Right (m, t') -> liftErrMsg (tell $ map bugWarn m)
                      >> return (Just $ noLocA t')
    where
      warnLine x = O.text "haddock-bug:" O.<+> O.text x O.<>
                   O.comma O.<+> O.quotes (O.ppr t) O.<+>
                   O.text "-- Please report this on Haddock issue tracker!"
      bugWarn = showSDoc dflags . warnLine

-- | This function is called for top-level bindings without type signatures.
-- It gets the type signature from GHC and that means it's not going to
-- have a meaningful 'SrcSpan'. So we pass down 'SrcSpan' for the
-- declaration and use it instead - 'nLoc' here.
hiValExportItem
  :: Monad m => DynFlags -> Name -> SrcSpan -> DocForDecl Name -> Bool
  -> Maybe Fixity -> IfM m (ExportItem GhcRn)
hiValExportItem dflags name nLoc doc splice fixity = do
  mayDecl <- hiDecl dflags name
  case mayDecl of
    Nothing -> return (ExportNoDecl name [])
    Just decl -> return (ExportDecl (fixSpan decl) [] doc [] [] fixities splice)
  where
    fixSpan (L (SrcSpanAnn a l) t) = L (SrcSpanAnn a (SrcLoc.combineSrcSpans l nLoc)) t
    fixities = case fixity of
      Just f  -> [(name, f)]
      Nothing -> []


-- | Lookup docs for a declaration from maps.
lookupDocs :: AvailInfo -> WarningMap -> DocMap Name -> ArgMap Name
           -> (DocForDecl Name, [(Name, DocForDecl Name)])
lookupDocs avail warnings docMap argMap =
  let n = availName avail in
  let lookupArgDoc x = M.findWithDefault M.empty x argMap in
  let doc = (lookupDoc n, lookupArgDoc n) in
  let subDocs = [ (s, (lookupDoc s, lookupArgDoc s))
                | s <- availSubordinates avail
                ] in
  (doc, subDocs)
  where
    lookupDoc name = Documentation (M.lookup name docMap) (M.lookup name warnings)


-- | Export the given module as `ExportModule`. We are not concerned with the
-- single export items of the given module.
moduleExport
  :: Monad m
  => Module           -- ^ Module A (identity, NOT semantic)
  -> DynFlags         -- ^ The flags used when typechecking A
  -> IfaceMap         -- ^ Already created interfaces
  -> InstIfaceMap     -- ^ Interfaces in other packages
  -> ModuleName       -- ^ The exported module
  -> IfM m [ExportItem GhcRn] -- ^ Resulting export items
moduleExport thisMod dflags ifaceMap instIfaceMap expMod =
    -- NB: we constructed the identity module when looking up in
    -- the IfaceMap.
    case M.lookup m ifaceMap of
      Just iface
        | OptHide `elem` ifaceOptions iface -> return (ifaceExportItems iface)
        | otherwise -> return [ ExportModule m ]

      Nothing -> -- We have to try to find it in the installed interfaces
                 -- (external packages).
        case M.lookup expMod (M.mapKeys moduleName instIfaceMap) of
          Just iface -> return [ ExportModule (instMod iface) ]
          Nothing -> do
            liftErrMsg $ tell ["Warning: " ++ pretty dflags thisMod ++ ": Could not find " ++
                               "documentation for exported module: " ++ pretty dflags expMod]
            return []
  where
    m = mkModule (moduleUnit thisMod) expMod -- Identity module!

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


-- | Simplified variant of 'mkExportItems', where we can assume that
-- every locally defined declaration is exported; thus, we just
-- zip through the renamed declarations.

fullModuleContents
  :: Monad m
  => Bool               -- is it a signature
  -> IfaceMap
  -> Maybe Package      -- this package
  -> Module             -- this module
  -> Module             -- semantic module
  -> WarningMap
  -> GlobalRdrEnv      -- ^ The renaming environment
  -> [Name]             -- exported names (orig)
  -> [LHsDecl GhcRn]    -- renamed source declarations
  -> Maps
  -> FixMap
  -> [SrcSpan]          -- splice locations
  -> InstIfaceMap
  -> DynFlags
  -> Avails
  -> IfM m [ExportItem GhcRn]
fullModuleContents is_sig modMap pkgName thisMod semMod warnings gre exportedNames
  decls maps@(_, _, declMap, _) fixMap splices instIfaceMap dflags avails = do
  let availEnv = availsToNameEnv (nubAvails avails)
  (concat . concat) `fmap` (for decls $ \decl -> do
    case decl of
      (L _ (DocD _ (DocGroup lev docStr))) -> do
        doc <- liftErrMsg (processDocString dflags gre (hsDocString . unLoc $ docStr))
        return [[ExportGroup lev "" doc]]
      (L _ (DocD _ (DocCommentNamed _ docStr))) -> do
        doc <- liftErrMsg (processDocStringParas dflags pkgName gre (hsDocString . unLoc $ docStr))
        return [[ExportDoc doc]]
      (L _ (ValD _ valDecl))
        | name:_ <- collectHsBindBinders CollNoDictBinders valDecl
        , Just (L _ SigD{}:_) <- filter isSigD <$> M.lookup name declMap
        -> return []
      _ ->
        for (getMainDeclBinder emptyOccEnv (unLoc decl)) $ \nm -> do
          case lookupNameEnv availEnv nm of
            Just avail ->
              availExportItem is_sig modMap thisMod
                semMod warnings exportedNames maps fixMap
                splices instIfaceMap dflags avail
            Nothing -> pure [])
  where
    isSigD (L _ SigD{}) = True
    isSigD _            = False

-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble
-- together a type signature for it...).
--
-- This function looks through the declarations in this module to try to find
-- the one with the right name.
extractDecl
  :: HasCallStack
  => DeclMap                   -- ^ all declarations in the file
  -> Name                      -- ^ name of the declaration to extract
  -> LHsDecl GhcRn             -- ^ parent declaration
  -> Either ErrMsg (LHsDecl GhcRn)
extractDecl declMap name decl
  | name `elem` getMainDeclBinder emptyOccEnv (unLoc decl) = pure decl
  | otherwise  =
    case unLoc decl of
      TyClD _ d@ClassDecl { tcdLName = L _ clsNm
                          , tcdSigs = clsSigs
                          , tcdATs = clsATs } ->
        let
          matchesMethod =
            [ lsig
            | lsig <- clsSigs
            , ClassOpSig _ False _ _ <- pure $ unLoc lsig
              -- Note: exclude `default` declarations (see #505)
            , name `elem` sigName lsig
            ]

          matchesAssociatedType =
            [ lfam_decl
            | lfam_decl <- clsATs
            , name == unLoc (fdLName (unLoc lfam_decl))
            ]

            -- TODO: document fixity
        in case (matchesMethod, matchesAssociatedType)  of
          ([s0], _) -> let tyvar_names = tyClDeclTyVars d
                           L pos sig = addClassContext clsNm tyvar_names s0
                       in pure (L pos (SigD noExtField sig))
          (_, [L pos fam_decl]) -> pure (L pos (TyClD noExtField (FamDecl noExtField fam_decl)))

          ([], [])
            | Just (famInstDecl:_) <- M.lookup name declMap
            -> extractDecl declMap name famInstDecl
          _ -> Left (concat [ "Ambiguous decl for ", getOccString name
                            , " in class ", getOccString clsNm ])

      TyClD _ d@DataDecl { tcdLName = L _ dataNm
                         , tcdDataDefn = HsDataDefn { dd_cons = dataCons } } -> do
        let ty_args = lHsQTyVarsToTypes (tyClDeclTyVars d)
        lsig <- if isDataConName name
                  then extractPatternSyn name dataNm ty_args dataCons
                  else extractRecSel name dataNm ty_args dataCons
        pure (SigD noExtField <$> lsig)

      TyClD _ FamDecl {}
        | isValName name
        , Just (famInst:_) <- M.lookup name declMap
        -> extractDecl declMap name famInst
      InstD _ (DataFamInstD _ (DataFamInstDecl
                            (FamEqn { feqn_tycon = L _ n
                                    , feqn_pats  = tys
                                    , feqn_rhs   = defn }))) ->
        if isDataConName name
        then fmap (SigD noExtField) <$> extractPatternSyn name n tys (dd_cons defn)
        else fmap (SigD noExtField) <$> extractRecSel name n tys (dd_cons defn)
      InstD _ (ClsInstD _ ClsInstDecl { cid_datafam_insts = insts })
        | isDataConName name ->
            let matches = [ d' | L _ d'@(DataFamInstDecl (FamEqn { feqn_rhs = dd })) <- insts
                               , name `elem` map unLoc (concatMap (getConNames . unLoc) (dd_cons dd))
                               ]
            in case matches of
                [d0] -> extractDecl declMap name (noLocA (InstD noExtField (DataFamInstD noExtField d0)))
                _    -> Left "internal: extractDecl (ClsInstD)"
        | otherwise ->
            let matches = [ d' | L _ d'@(DataFamInstDecl d )
                                   <- insts
                                 -- , L _ ConDecl { con_details = RecCon rec } <- dd_cons (feqn_rhs d)
                               , Just rec <- map (getRecConArgs_maybe . unLoc) (dd_cons (feqn_rhs d))
                               , ConDeclField { cd_fld_names = ns } <- map unLoc (unLoc rec)
                               , L _ n <- ns
                               , foExt n == name
                          ]
            in case matches of
              [d0] -> extractDecl declMap name (noLocA . InstD noExtField $ DataFamInstD noExtField d0)
              _ -> Left "internal: extractDecl (ClsInstD)"
      _ -> Left ("extractDecl: Unhandled decl for " ++ getOccString name)

extractPatternSyn :: HasCallStack
                  => Name -> Name
                  -> [LHsTypeArg GhcRn] -> [LConDecl GhcRn]
                  -> Either ErrMsg (LSig GhcRn)
extractPatternSyn nm t tvs cons =
  case filter matches cons of
    [] -> Left . O.showSDocOneLine O.defaultSDocContext $
          O.text "constructor pattern " O.<+> O.ppr nm O.<+> O.text "not found in type" O.<+> O.ppr t
    con:_ -> pure (extract <$> con)
 where
  matches :: LConDecl GhcRn -> Bool
  matches (L _ con) = nm `elem` (unLoc <$> getConNames con)
  extract :: ConDecl GhcRn -> Sig GhcRn
  extract con =
    let args =
          case con of
            ConDeclH98 { con_args = con_args' } -> case con_args' of
              PrefixCon _ args' -> map hsScaledThing args'
              RecCon (L _ fields) -> cd_fld_type . unLoc <$> fields
              InfixCon arg1 arg2 -> map hsScaledThing [arg1, arg2]
            ConDeclGADT { con_g_args = con_args' } -> case con_args' of
              PrefixConGADT args' -> map hsScaledThing args'
              RecConGADT (L _ fields) _ -> cd_fld_type . unLoc <$> fields
        typ = longArrow args (data_ty con)
        typ' =
          case con of
            ConDeclH98 { con_mb_cxt = Just cxt } -> noLocA (HsQualTy noExtField cxt typ)
            _ -> typ
        typ'' = noLocA (HsQualTy noExtField (noLocA []) typ')
    in PatSynSig noAnn [noLocA nm] (mkEmptySigType typ'')

  longArrow :: [LHsType GhcRn] -> LHsType GhcRn -> LHsType GhcRn
  longArrow inputs output = foldr (\x y -> noLocA (HsFunTy noAnn (HsUnrestrictedArrow noHsUniTok) x y)) output inputs

  data_ty con
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise = foldl' (\x y -> noLocA (mkAppTyArg x y)) (noLocA (HsTyVar noAnn NotPromoted (noLocA t))) tvs
                    where mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
                          mkAppTyArg f (HsValArg ty) = HsAppTy noExtField f ty
                          mkAppTyArg f (HsTypeArg l ki) = HsAppKindTy l f ki
                          mkAppTyArg f (HsArgPar _) = HsParTy noAnn f

extractRecSel :: Name -> Name -> [LHsTypeArg GhcRn] -> [LConDecl GhcRn]
              -> Either ErrMsg (LSig GhcRn)
extractRecSel _ _ _ [] = Left "extractRecSel: selector not found"

extractRecSel nm t tvs (L _ con : rest) =
  case getRecConArgs_maybe con of
    Just (L _ fields) | ((l,L _ (ConDeclField _ _nn ty _)) : _) <- matching_fields fields ->
      pure (L (noAnnSrcSpan l) (TypeSig noAnn [noLocA nm] (mkEmptyWildCardBndrs $ mkEmptySigType (noLocA (HsFunTy noAnn (HsUnrestrictedArrow noHsUniTok) data_ty (getBangType ty))))))
    _ -> extractRecSel nm t tvs rest
 where
  matching_fields :: [LConDeclField GhcRn] -> [(SrcSpan, LConDeclField GhcRn)]
  matching_fields flds = [ (locA l,f) | f@(L _ (ConDeclField _ ns _ _)) <- flds
                                      , L l n <- ns, foExt n == nm ]
  data_ty
    -- ResTyGADT _ ty <- con_res con = ty
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise = foldl' (\x y -> noLocA (mkAppTyArg x y)) (noLocA (HsTyVar noAnn NotPromoted (noLocA t))) tvs
                   where mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
                         mkAppTyArg f (HsValArg ty) = HsAppTy noExtField f ty
                         mkAppTyArg f (HsTypeArg l ki) = HsAppKindTy l f ki
                         mkAppTyArg f (HsArgPar _) = HsParTy noAnn f

-- | Keep export items with docs.
pruneExportItems :: [ExportItem GhcRn] -> [ExportItem GhcRn]
pruneExportItems = filter hasDoc
  where
    hasDoc (ExportDecl{expItemMbDoc = (Documentation d _, _)}) = isJust d
    hasDoc _ = True


mkVisibleNames :: Maps -> [ExportItem GhcRn] -> [DocOption] -> [Name]
mkVisibleNames (_, _, _, instMap) exports opts
  | OptHide `elem` opts = []
  | otherwise = let ns = concatMap exportName exports
                in seqList ns `seq` ns
  where
    exportName e@ExportDecl {} = name ++ subs ++ patsyns
      where subs    = map fst (expItemSubDocs e)
            patsyns = concatMap (getMainDeclBinder emptyOccEnv . fst) (expItemPats e)
            name = case unLoc $ expItemDecl e of
              InstD _ d -> maybeToList $ SrcLoc.lookupSrcSpan (getInstLoc d) instMap
              decl      -> getMainDeclBinder emptyOccEnv decl
    exportName ExportNoDecl {} = [] -- we don't count these as visible, since
                                    -- we don't want links to go to them.
    exportName _ = []

seqList :: [a] -> ()
seqList [] = ()
seqList (x : xs) = x `seq` seqList xs

-- | Find a stand-alone documentation comment by its name.
findNamedDoc :: String -> [HsDecl GhcRn] -> ErrMsgM (Maybe HsDocString)
findNamedDoc name = search
  where
    search [] = do
      tell ["Cannot find documentation for: $" ++ name]
      return Nothing
    search (DocD _ (DocCommentNamed name' doc) : rest)
      | name == name' = return (Just (hsDocString . unLoc $ doc))

      | otherwise = search rest
    search (_other_decl : rest) = search rest
