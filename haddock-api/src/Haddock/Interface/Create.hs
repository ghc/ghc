{-# LANGUAGE CPP, TupleSections, BangPatterns, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
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
module Haddock.Interface.Create (createInterface) where

import Documentation.Haddock.Doc (metaDocAppend)
import Haddock.Types
import Haddock.Options
import Haddock.GhcUtils
import Haddock.Utils
import Haddock.Convert
import Haddock.Interface.LexParseRn

import Data.Bifunctor
import Data.Bitraversable
import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Ord
import Control.Applicative
import Control.Monad
import Data.Traversable

import Avail hiding (avail)
import qualified Avail
import qualified Module
import qualified SrcLoc
import ConLike (ConLike(..))
import GHC
import HscTypes
import Name
import NameSet
import NameEnv
import Packages   ( lookupModuleInAllPackages, PackageName(..) )
import Bag
import RdrName
import TcRnTypes
import FastString ( unpackFS, fastStringToByteString)
import BasicTypes ( StringLiteral(..), SourceText(..), PromotionFlag(..) )
import qualified Outputable as O


-- | Use a 'TypecheckedModule' to produce an 'Interface'.
-- To do this, we need access to already processed modules in the topological
-- sort. That's what's in the 'IfaceMap'.
createInterface :: TypecheckedModule
                -> [Flag]       -- Boolean flags
                -> IfaceMap     -- Locally processed modules
                -> InstIfaceMap -- External, already installed interfaces
                -> ErrMsgGhc Interface
createInterface tm flags modMap instIfaceMap = do

  let ms             = pm_mod_summary . tm_parsed_module $ tm
      mi             = moduleInfo tm
      L _ hsm        = parsedSource tm
      !safety        = modInfoSafe mi
      mdl            = ms_mod ms
      sem_mdl        = tcg_semantic_mod (fst (tm_internals_ tm))
      is_sig         = ms_hsc_src ms == HsigFile
      dflags         = ms_hspp_opts ms
      !instances     = modInfoInstances mi
      !fam_instances = md_fam_insts md
      !exportedNames = modInfoExportsWithSelectors mi
      (pkgNameFS, _) = modulePackageInfo dflags flags (Just mdl)
      pkgName        = fmap (unpackFS . (\(PackageName n) -> n)) pkgNameFS

      (TcGblEnv { tcg_rdr_env = gre
                , tcg_warns   = warnings
                , tcg_exports = all_exports0
                }, md) = tm_internals_ tm
      all_local_avails = gresToAvailInfo . filter isLocalGRE . globalRdrEnvElts $ gre

  -- The 'pkgName' is necessary to decide what package to mention in "@since"
  -- annotations. Not having it is not fatal though.
  --
  -- Cabal can be trusted to pass the right flags, so this warning should be
  -- mostly encountered when running Haddock outside of Cabal.
  when (isNothing pkgName) $
    liftErrMsg $ tell [ "Warning: Package name is not available." ]

  -- The renamed source should always be available to us, but it's best
  -- to be on the safe side.
  (group_, imports, mayExports, mayDocHeader) <-
    case renamedSource tm of
      Nothing -> do
        liftErrMsg $ tell [ "Warning: Renamed source is not available." ]
        return (emptyRnGroup, [], Nothing, Nothing)
      Just x -> return x

  opts <- liftErrMsg $ mkDocOpts (haddockOptions dflags) flags mdl

  -- Process the top-level module header documentation.
  (!info, mbDoc) <- liftErrMsg $ processModuleHeader dflags pkgName gre safety mayDocHeader

  let declsWithDocs = topDecls group_

      exports0 = fmap (map (first unLoc)) mayExports
      (all_exports, exports)
        | OptIgnoreExports `elem` opts = (all_local_avails, Nothing)
        | otherwise = (all_exports0, exports0)

      unrestrictedImportedMods
        -- module re-exports are only possible with
        -- explicit export list
        | Just{} <- exports
        = unrestrictedModuleImports (map unLoc imports)
        | otherwise = M.empty

      fixMap = mkFixMap group_
      (decls, _) = unzip declsWithDocs
      localInsts = filter (nameIsLocalOrFrom sem_mdl)
                        $  map getName fam_instances
                        ++ map getName instances
      -- Locations of all TH splices
      splices = [ l | L l (SpliceD _ _) <- hsmodDecls hsm ]

  warningMap <- liftErrMsg (mkWarningMap dflags warnings gre exportedNames)

  maps@(!docMap, !argMap, !declMap, _) <-
    liftErrMsg (mkMaps dflags pkgName gre localInsts declsWithDocs)

  let allWarnings = M.unions (warningMap : map ifaceWarningMap (M.elems modMap))

  -- The MAIN functionality: compute the export items which will
  -- each be the actual documentation of this module.
  exportItems <- mkExportItems is_sig modMap pkgName mdl sem_mdl allWarnings gre
                   exportedNames decls maps fixMap unrestrictedImportedMods
                   splices exports all_exports instIfaceMap dflags

  let !visibleNames = mkVisibleNames maps exportItems opts

  -- Measure haddock documentation coverage.
  let prunedExportItems0 = pruneExportItems exportItems
      !haddockable = 1 + length exportItems -- module + exports
      !haddocked = (if isJust mbDoc then 1 else 0) + length prunedExportItems0
      !coverage = (haddockable, haddocked)

  -- Prune the export list to just those declarations that have
  -- documentation, if the 'prune' option is on.
  let prunedExportItems'
        | OptPrune `elem` opts = prunedExportItems0
        | otherwise = exportItems
      !prunedExportItems = seqList prunedExportItems' `seq` prunedExportItems'

  let !aliases =
        mkAliasMap dflags $ tm_renamed_source tm

  modWarn <- liftErrMsg (moduleWarning dflags gre warnings)

  return $! Interface {
    ifaceMod               = mdl
  , ifaceIsSig             = is_sig
  , ifaceOrigFilename      = msHsFilePath ms
  , ifaceInfo              = info
  , ifaceDoc               = Documentation mbDoc modWarn
  , ifaceRnDoc             = Documentation Nothing Nothing
  , ifaceOptions           = opts
  , ifaceDocMap            = docMap
  , ifaceArgMap            = argMap
  , ifaceRnDocMap          = M.empty
  , ifaceRnArgMap          = M.empty
  , ifaceExportItems       = prunedExportItems
  , ifaceRnExportItems     = []
  , ifaceExports           = exportedNames
  , ifaceVisibleExports    = visibleNames
  , ifaceDeclMap           = declMap
  , ifaceFixMap            = fixMap
  , ifaceModuleAliases     = aliases
  , ifaceInstances         = instances
  , ifaceFamInstances      = fam_instances
  , ifaceOrphanInstances   = [] -- Filled in `attachInstances`
  , ifaceRnOrphanInstances = [] -- Filled in `renameInterface`
  , ifaceHaddockCoverage   = coverage
  , ifaceWarningMap        = warningMap
  , ifaceHieFile           = Just $ ml_hie_file $ ms_location ms
  , ifaceDynFlags          = dflags
  }


-- | Given all of the @import M as N@ declarations in a package,
-- create a mapping from the module identity of M, to an alias N
-- (if there are multiple aliases, we pick the last one.)  This
-- will go in 'ifaceModuleAliases'.
mkAliasMap :: DynFlags -> Maybe RenamedSource -> M.Map Module ModuleName
mkAliasMap dflags mRenamedSource =
  case mRenamedSource of
    Nothing -> M.empty
    Just (_,impDecls,_,_) ->
      M.fromList $
      mapMaybe (\(SrcLoc.L _ impDecl) -> do
        SrcLoc.L _ alias <- ideclAs impDecl
        return $
          (lookupModuleDyn dflags
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
             (fmap Module.fsToUnitId $
              fmap sl_fs $ ideclPkgQual impDecl)
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
unrestrictedModuleImports :: [ImportDecl name] -> M.Map ModuleName [ModuleName]
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
      case ideclHiding idecl of
        -- i) no subset selected
        Nothing             -> True
        -- ii) an import with a hiding clause
        -- without any names
        Just (True, L _ []) -> True
        -- iii) any other case of qualification
        _                   -> False

-- Similar to GHC.lookupModule
-- ezyang: Not really...
lookupModuleDyn ::
  DynFlags -> Maybe UnitId -> ModuleName -> Module
lookupModuleDyn _ (Just pkgId) mdlName =
  Module.mkModule pkgId mdlName
lookupModuleDyn dflags Nothing mdlName =
  case lookupModuleInAllPackages dflags mdlName of
    (m,_):_ -> m
    [] -> Module.mkModule Module.mainUnitId mdlName


-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

mkWarningMap :: DynFlags -> Warnings -> GlobalRdrEnv -> [Name] -> ErrMsgM WarningMap
mkWarningMap dflags warnings gre exps = case warnings of
  NoWarnings  -> pure M.empty
  WarnAll _   -> pure M.empty
  WarnSome ws ->
    let ws' = [ (n, w)
              | (occ, w) <- ws
              , elt <- lookupGlobalRdrEnv gre occ
              , let n = gre_name elt, n `elem` exps ]
    in M.fromList <$> traverse (bitraverse pure (parseWarning dflags gre)) ws'

moduleWarning :: DynFlags -> GlobalRdrEnv -> Warnings -> ErrMsgM (Maybe (Doc Name))
moduleWarning _ _ NoWarnings = pure Nothing
moduleWarning _ _ (WarnSome _) = pure Nothing
moduleWarning dflags gre (WarnAll w) = Just <$> parseWarning dflags gre w

parseWarning :: DynFlags -> GlobalRdrEnv -> WarningTxt -> ErrMsgM (Doc Name)
parseWarning dflags gre w = case w of
  DeprecatedTxt _ msg -> format "Deprecated: " (foldMap (fastStringToByteString . sl_fs . unLoc) msg)
  WarningTxt    _ msg -> format "Warning: "    (foldMap (fastStringToByteString . sl_fs . unLoc) msg)
  where
    format x bs = DocWarning . DocParagraph . DocAppend (DocString x)
                  <$> processDocString dflags gre (mkHsDocStringUtf8ByteString bs)


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
       -> [(LHsDecl GhcRn, [HsDocString])]
       -> ErrMsgM Maps
mkMaps dflags pkgName gre instances decls = do
  (a, b, c) <- unzip3 <$> traverse mappings decls
  pure ( f' (map (nubByName fst) a)
       , f  (filterMapping (not . M.null) b)
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

    mappings :: (LHsDecl GhcRn, [HsDocString])
             -> ErrMsgM ( [(Name, MDoc Name)]
                        , [(Name, Map Int (MDoc Name))]
                        , [(Name,  [LHsDecl GhcRn])]
                        )
    mappings (ldecl, docStrs) = do
      let L l decl = ldecl
          declDoc :: [HsDocString] -> Map Int HsDocString
                  -> ErrMsgM (Maybe (MDoc Name), Map Int (MDoc Name))
          declDoc strs m = do
            doc' <- processDocStrings dflags pkgName gre strs
            m'   <- traverse (processDocStringParas dflags pkgName gre) m
            pure (doc', m')

      (doc, args) <- declDoc docStrs (declTypeDocs decl)

      let
          subs :: [(Name, [HsDocString], Map Int HsDocString)]
          subs = subordinates instanceMap decl

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

    instanceMap :: Map SrcSpan Name
    instanceMap = M.fromList [ (getSrcSpan n, n) | n <- instances ]

    names :: SrcSpan -> HsDecl GhcRn -> [Name]
    names _ (InstD _ d) = maybeToList (M.lookup loc instanceMap) -- See note [2].
      where loc = case d of
              -- The CoAx's loc is the whole line, but only for TFs. The
              -- workaround is to dig into the family instance declaration and
              -- get the identifier with the right location.
              TyFamInstD _ (TyFamInstDecl d') -> getLoc (feqn_tycon (hsib_body d'))
              _ -> getInstLoc d
    names l (DerivD {}) = maybeToList (M.lookup l instanceMap) -- See note [2].
    names _ decl = getMainDeclBinder decl

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


-- | Get all subordinate declarations inside a declaration, and their docs.
-- A subordinate declaration is something like the associate type or data
-- family of a type class.
subordinates :: InstMap
             -> HsDecl GhcRn
             -> [(Name, [HsDocString], Map Int HsDocString)]
subordinates instMap decl = case decl of
  InstD _ (ClsInstD _ d) -> do
    DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
      FamEqn { feqn_tycon = L l _
             , feqn_rhs   = defn }}} <- unLoc <$> cid_datafam_insts d
    [ (n, [], M.empty) | Just n <- [M.lookup l instMap] ] ++ dataSubs defn

  InstD _ (DataFamInstD _ (DataFamInstDecl (HsIB { hsib_body = d })))
    -> dataSubs (feqn_rhs d)
  TyClD _ d | isClassDecl d -> classSubs d
            | isDataDecl  d -> dataSubs (tcdDataDefn d)
  _ -> []
  where
    classSubs dd = [ (name, doc, declTypeDocs d) | (L _ d, doc) <- classDecls dd
                   , name <- getMainDeclBinder d, not (isValD d)
                   ]
    dataSubs :: HsDataDefn GhcRn -> [(Name, [HsDocString], Map Int HsDocString)]
    dataSubs dd = constrs ++ fields ++ derivs
      where
        cons = map unL $ (dd_cons dd)
        constrs = [ (unL cname, maybeToList $ fmap unL $ con_doc c, conArgDocs c)
                  | c <- cons, cname <- getConNames c ]
        fields  = [ (extFieldOcc n, maybeToList $ fmap unL doc, M.empty)
                  | RecCon flds <- map getConArgs cons
                  , L _ (ConDeclField _ ns _ doc) <- (unLoc flds)
                  , L _ n <- ns ]
        derivs  = [ (instName, [unL doc], M.empty)
                  | HsIB { hsib_body = L l (HsDocTy _ _ doc) }
                      <- concatMap (unLoc . deriv_clause_tys . unLoc) $
                           unLoc $ dd_derivs dd
                  , Just instName <- [M.lookup l instMap] ]

-- | Extract constructor argument docs from inside constructor decls.
conArgDocs :: ConDecl GhcRn -> Map Int HsDocString
conArgDocs con = case getConArgs con of
                   PrefixCon args -> go 0 (map unLoc args ++ ret)
                   InfixCon arg1 arg2 -> go 0 ([unLoc arg1, unLoc arg2] ++ ret)
                   RecCon _ -> go 1 ret
  where
    go n (HsDocTy _ _ (L _ ds) : tys) = M.insert n ds $ go (n+1) tys
    go n (HsBangTy _ _ (L _ (HsDocTy _ _ (L _ ds))) : tys) = M.insert n ds $ go (n+1) tys
    go n (_ : tys) = go (n+1) tys
    go _ [] = M.empty

    ret = case con of
            ConDeclGADT { con_res_ty = res_ty } -> [ unLoc res_ty ]
            _ -> []

-- | Extract function argument docs from inside top-level decls.
declTypeDocs :: HsDecl GhcRn -> Map Int HsDocString
declTypeDocs (SigD  _ (TypeSig _ _ ty))          = typeDocs (unLoc (hsSigWcType ty))
declTypeDocs (SigD  _ (ClassOpSig _ _ _ ty))     = typeDocs (unLoc (hsSigType ty))
declTypeDocs (SigD  _ (PatSynSig _ _ ty))        = typeDocs (unLoc (hsSigType ty))
declTypeDocs (ForD  _ (ForeignImport _ _ ty _))  = typeDocs (unLoc (hsSigType ty))
declTypeDocs (TyClD _ (SynDecl { tcdRhs = ty })) = typeDocs (unLoc ty)
declTypeDocs _ = M.empty

-- | Extract function argument docs from inside types.
typeDocs :: HsType GhcRn -> Map Int HsDocString
typeDocs = go 0
  where
    go n (HsForAllTy { hst_body = ty }) = go n (unLoc ty)
    go n (HsQualTy   { hst_body = ty }) = go n (unLoc ty)
    go n (HsFunTy _ (L _ (HsDocTy _ _ (L _ x))) (L _ ty)) = M.insert n x $ go (n+1) ty
    go n (HsFunTy _ _ ty) = go (n+1) (unLoc ty)
    go n (HsDocTy _ _ (L _ doc)) = M.singleton n doc
    go _ _ = M.empty

-- | All the sub declarations of a class (that we handle), ordered by
-- source location, with documentation attached if it exists.
classDecls :: TyClDecl GhcRn -> [(LHsDecl GhcRn, [HsDocString])]
classDecls class_ = filterDecls . collectDocs . sortByLoc $ decls
  where
    decls = docs ++ defs ++ sigs ++ ats
    docs  = mkDecls tcdDocs (DocD noExt) class_
    defs  = mkDecls (bagToList . tcdMeths) (ValD noExt) class_
    sigs  = mkDecls tcdSigs (SigD noExt) class_
    ats   = mkDecls tcdATs (TyClD noExt . FamDecl noExt) class_


-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup GhcRn -> [(LHsDecl GhcRn, [HsDocString])]
topDecls =
  filterClasses . filterDecls . collectDocs . sortByLoc . ungroup

-- | Extract a map of fixity declarations only
mkFixMap :: HsGroup GhcRn -> FixMap
mkFixMap group_ = M.fromList [ (n,f)
                             | L _ (FixitySig _ ns f) <- hs_fixds group_,
                               L _ n <- ns ]


-- | Take all declarations except pragmas, infix decls, rules from an 'HsGroup'.
ungroup :: HsGroup GhcRn -> [LHsDecl GhcRn]
ungroup group_ =
  mkDecls (tyClGroupTyClDecls . hs_tyclds) (TyClD noExt)  group_ ++
  mkDecls hs_derivds             (DerivD noExt) group_ ++
  mkDecls hs_defds               (DefD noExt)   group_ ++
  mkDecls hs_fords               (ForD noExt)   group_ ++
  mkDecls hs_docs                (DocD noExt)   group_ ++
  mkDecls (tyClGroupInstDecls . hs_tyclds) (InstD noExt)  group_ ++
  mkDecls (typesigs . hs_valds)  (SigD noExt)   group_ ++
  mkDecls (valbinds . hs_valds)  (ValD noExt)   group_
  where
    typesigs (XValBindsLR (NValBinds _ sigs)) = filter isUserLSig sigs
    typesigs _ = error "expected ValBindsOut"

    valbinds (XValBindsLR (NValBinds binds _)) = concatMap bagToList . snd . unzip $ binds
    valbinds _ = error "expected ValBindsOut"


-- | Take a field of declarations from a data structure and create HsDecls
-- using the given constructor
mkDecls :: (a -> [Located b]) -> (b -> c) -> a -> [Located c]
mkDecls field con struct = [ L loc (con decl) | L loc decl <- field struct ]


-- | Sort by source location
sortByLoc :: [Located a] -> [Located a]
sortByLoc = sortBy (comparing getLoc)


--------------------------------------------------------------------------------
-- Filtering of declarations
--
-- We filter out declarations that we don't intend to handle later.
--------------------------------------------------------------------------------


-- | Filter out declarations that we don't handle in Haddock
filterDecls :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterDecls = filter (isHandled . unL . fst)
  where
    isHandled (ForD _ (ForeignImport {})) = True
    isHandled (TyClD {})  = True
    isHandled (InstD {})  = True
    isHandled (DerivD {}) = True
    isHandled (SigD _ d)  = isUserLSig (reL d)
    isHandled (ValD {})   = True
    -- we keep doc declarations to be able to get at named docs
    isHandled (DocD {})   = True
    isHandled _ = False

-- | Go through all class declarations and filter their sub-declarations
filterClasses :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterClasses decls = [ if isClassD d then (L loc (filterClass d), doc) else x
                      | x@(L loc d, doc) <- decls ]
  where
    filterClass (TyClD x c) =
      TyClD x $ c { tcdSigs = filter (liftA2 (||) isUserLSig isMinimalLSig) $ tcdSigs c }
    filterClass _ = error "expected TyClD"


--------------------------------------------------------------------------------
-- Collect docs
--
-- To be able to attach the right Haddock comment to the right declaration,
-- we sort the declarations by their SrcLoc and "collect" the docs for each
-- declaration.
--------------------------------------------------------------------------------


-- | Collect docs and attach them to the right declarations.
collectDocs :: [LHsDecl a] -> [(LHsDecl a, [HsDocString])]
collectDocs = go Nothing []
  where
    go Nothing _ [] = []
    go (Just prev) docs [] = finished prev docs []
    go prev docs (L _ (DocD _ (DocCommentNext str)) : ds)
      | Nothing <- prev = go Nothing (str:docs) ds
      | Just decl <- prev = finished decl docs (go Nothing [str] ds)
    go prev docs (L _ (DocD _ (DocCommentPrev str)) : ds) = go prev (str:docs) ds
    go Nothing docs (d:ds) = go (Just d) docs ds
    go (Just prev) docs (d:ds) = finished prev docs (go (Just d) [] ds)

    finished decl docs rest = (decl, reverse docs) : rest


-- | Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.
--
-- We create the export items even if the module is hidden, since they
-- might be useful when creating the export items for other modules.
mkExportItems
  :: Bool               -- is it a signature
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
  -> ErrMsgGhc [ExportItem GhcRn]
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
      doc <- processDocString dflags gre docStr
      return [ExportGroup lev "" doc]

    lookupExport (IEDoc _ docStr, _)        = liftErrMsg $ do
      doc <- processDocStringParas dflags pkgName gre docStr
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

availExportItem :: Bool               -- is it a signature
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
                -> ErrMsgGhc [ExportItem GhcRn]
availExportItem is_sig modMap thisMod semMod warnings exportedNames
  (docMap, argMap, declMap, _) fixMap splices instIfaceMap
  dflags availInfo = declWith availInfo
  where
    declWith :: AvailInfo -> ErrMsgGhc [ ExportItem GhcRn ]
    declWith avail = do
      let t = availName avail
      r    <- findDecl avail
      case r of
        ([L l (ValD _ _)], (doc, _)) -> do
          -- Top-level binding without type signature
          export <- hiValExportItem dflags t l doc (l `elem` splices) $ M.lookup t fixMap
          return [export]
        (ds, docs_) | decl : _ <- filter (not . isValD . unLoc) ds ->
          let declNames = getMainDeclBinder (unL decl)
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
                    let newDecl = L loc . SigD noExt . fromJust $ filterSigNames (== t) sig
                    in availExportDecl avail newDecl docs_

                  L loc (TyClD _ cl@ClassDecl{}) -> do
                    mdef <- liftGhcToErrMsgGhc $ minimalDef t
                    let sig = maybeToList $ fmap (noLoc . MinimalSig noExt NoSourceText . noLoc . fmap noLoc) mdef
                    availExportDecl avail
                      (L loc $ TyClD noExt cl { tcdSigs = sig ++ tcdSigs cl }) docs_

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

    availExportDecl :: AvailInfo -> LHsDecl GhcRn
                    -> (DocForDecl Name, [(Name, DocForDecl Name)])
                    -> ErrMsgGhc [ ExportItem GhcRn ]
    availExportDecl avail decl (doc, subs)
      | availExportsDecl avail = do
          -- bundled pattern synonyms only make sense if the declaration is
          -- exported (otherwise there would be nothing to bundle to)
          bundledPatSyns <- findBundledPatterns avail

          let
            patSynNames =
              concatMap (getMainDeclBinder . fst) bundledPatSyns

            fixities =
                [ (n, f)
                | n <- availName avail : fmap fst subs ++ patSynNames
                , Just f <- [M.lookup n fixMap]
                ]

          return [ ExportDecl {
                       expItemDecl      = restrictTo (fmap fst subs)
                                            (extractDecl declMap (availName avail) decl)
                     , expItemPats      = bundledPatSyns
                     , expItemMbDoc     = doc
                     , expItemSubDocs   = subs
                     , expItemInstances = []
                     , expItemFixities  = fixities
                     , expItemSpliced   = False
                     }
                 ]

      | otherwise =
          return [ ExportDecl {
                       expItemDecl      = extractDecl declMap sub decl
                     , expItemPats      = []
                     , expItemMbDoc     = sub_doc
                     , expItemSubDocs   = []
                     , expItemInstances = []
                     , expItemFixities  = [ (sub, f) | Just f <- [M.lookup sub fixMap] ]
                     , expItemSpliced   = False
                     }
                 | (sub, sub_doc) <- subs
                 ]

    exportedNameSet = mkNameSet exportedNames
    isExported n = elemNameSet n exportedNameSet

    findDecl :: AvailInfo -> ErrMsgGhc ([LHsDecl GhcRn], (DocForDecl Name, [(Name, DocForDecl Name)]))
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
                    -- requirementContext (pkgState)
                    Just decl -> return ([decl], (noDocForDecl, availNoDocs avail))
              | otherwise ->
                return ([], (noDocForDecl, availNoDocs avail))
      | Just iface <- M.lookup (semToIdMod (moduleUnitId thisMod) m) modMap
      , Just ds <- M.lookup n (ifaceDeclMap iface) =
          return (ds, lookupDocs avail warnings
                            (ifaceDocMap iface)
                            (ifaceArgMap iface))
      | otherwise = return ([], (noDocForDecl, availNoDocs avail))
      where
        n = availName avail
        m = nameModule n

    findBundledPatterns :: AvailInfo -> ErrMsgGhc [(HsDecl GhcRn, DocForDecl Name)]
    findBundledPatterns avail = do
      patsyns <- for constructor_names $ \name -> do
        mtyThing <- liftGhcToErrMsgGhc (lookupName name)
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

-- this heavily depends on the invariants stated in Avail
availExportsDecl :: AvailInfo -> Bool
availExportsDecl (AvailTC ty_name names _)
  | n : _ <- names = ty_name == n
  | otherwise      = False
availExportsDecl _ = True

availSubordinates :: AvailInfo -> [Name]
availSubordinates avail =
  filter (/= availName avail) (availNamesWithSelectors avail)

availNoDocs :: AvailInfo -> [(Name, DocForDecl Name)]
availNoDocs avail =
  zip (availSubordinates avail) (repeat noDocForDecl)

-- | Given a 'Module' from a 'Name', convert it into a 'Module' that
-- we can actually find in the 'IfaceMap'.
semToIdMod :: UnitId -> Module -> Module
semToIdMod this_uid m
    | Module.isHoleModule m = mkModule this_uid (moduleName m)
    | otherwise      = m

hiDecl :: DynFlags -> Name -> ErrMsgGhc (Maybe (LHsDecl GhcRn))
hiDecl dflags t = do
  mayTyThing <- liftGhcToErrMsgGhc $ lookupName t
  case mayTyThing of
    Nothing -> do
      liftErrMsg $ tell ["Warning: Not found in environment: " ++ pretty dflags t]
      return Nothing
    Just x -> case tyThingToLHsDecl ShowRuntimeRep x of
      Left m -> liftErrMsg (tell [bugWarn m]) >> return Nothing
      Right (m, t') -> liftErrMsg (tell $ map bugWarn m)
                      >> return (Just $ noLoc t')
    where
      warnLine x = O.text "haddock-bug:" O.<+> O.text x O.<>
                   O.comma O.<+> O.quotes (O.ppr t) O.<+>
                   O.text "-- Please report this on Haddock issue tracker!"
      bugWarn = O.showSDoc dflags . warnLine

-- | This function is called for top-level bindings without type signatures.
-- It gets the type signature from GHC and that means it's not going to
-- have a meaningful 'SrcSpan'. So we pass down 'SrcSpan' for the
-- declaration and use it instead - 'nLoc' here.
hiValExportItem :: DynFlags -> Name -> SrcSpan -> DocForDecl Name -> Bool
                -> Maybe Fixity -> ErrMsgGhc (ExportItem GhcRn)
hiValExportItem dflags name nLoc doc splice fixity = do
  mayDecl <- hiDecl dflags name
  case mayDecl of
    Nothing -> return (ExportNoDecl name [])
    Just decl -> return (ExportDecl (fixSpan decl) [] doc [] [] fixities splice)
  where
    fixSpan (L l t) = L (SrcLoc.combineSrcSpans l nLoc) t
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
moduleExport :: Module           -- ^ Module A (identity, NOT semantic)
             -> DynFlags         -- ^ The flags used when typechecking A
             -> IfaceMap         -- ^ Already created interfaces
             -> InstIfaceMap     -- ^ Interfaces in other packages
             -> ModuleName       -- ^ The exported module
             -> ErrMsgGhc [ExportItem GhcRn] -- ^ Resulting export items
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
            liftErrMsg $
              tell ["Warning: " ++ pretty dflags thisMod ++ ": Could not find " ++
                    "documentation for exported module: " ++ pretty dflags expMod]
            return []
  where
    m = mkModule unitId expMod -- Identity module!
    unitId = moduleUnitId thisMod

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

fullModuleContents :: Bool               -- is it a signature
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
                   -> ErrMsgGhc [ExportItem GhcRn]
fullModuleContents is_sig modMap pkgName thisMod semMod warnings gre exportedNames
  decls maps@(_, _, declMap, _) fixMap splices instIfaceMap dflags avails = do
  let availEnv = availsToNameEnv (nubAvails avails)
  (concat . concat) `fmap` (for decls $ \decl -> do
    case decl of
      (L _ (DocD _ (DocGroup lev docStr))) -> do
        doc <- liftErrMsg (processDocString dflags gre docStr)
        return [[ExportGroup lev "" doc]]
      (L _ (DocD _ (DocCommentNamed _ docStr))) -> do
        doc <- liftErrMsg (processDocStringParas dflags pkgName gre docStr)
        return [[ExportDoc doc]]
      (L _ (ValD _ valDecl))
        | name:_ <- collectHsBindBinders valDecl
        , Just (L _ SigD{}:_) <- filter isSigD <$> M.lookup name declMap
        -> return []
      _ ->
        for (getMainDeclBinder (unLoc decl)) $ \nm -> do
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
extractDecl :: DeclMap -> Name -> LHsDecl GhcRn -> LHsDecl GhcRn
extractDecl declMap name decl
  | name `elem` getMainDeclBinder (unLoc decl) = decl
  | otherwise  =
    case unLoc decl of
      TyClD _ d@ClassDecl {} ->
        let
          matchesMethod =
            [ lsig
            | lsig <- tcdSigs d
            , ClassOpSig _ False _ _ <- pure $ unLoc lsig
              -- Note: exclude `default` declarations (see #505)
            , name `elem` sigName lsig
            ]

          matchesAssociatedType =
            [ lfam_decl
            | lfam_decl <- tcdATs d
            , name == unLoc (fdLName (unLoc lfam_decl))
            ]

            -- TODO: document fixity
        in case (matchesMethod, matchesAssociatedType)  of
          ([s0], _) -> let (n, tyvar_names) = (tcdName d, tyClDeclTyVars d)
                           L pos sig = addClassContext n tyvar_names s0
                       in L pos (SigD noExt sig)
          (_, [L pos fam_decl]) -> L pos (TyClD noExt (FamDecl noExt fam_decl))

          ([], [])
            | Just (famInstDecl:_) <- M.lookup name declMap
            -> extractDecl declMap name famInstDecl
          _ -> O.pprPanic "extractDecl" (O.text "Ambiguous decl for" O.<+> O.ppr name O.<+> O.text "in class:"
                                         O.$$ O.nest 4 (O.ppr d)
                                         O.$$ O.text "Matches:"
                                         O.$$ O.nest 4 (O.ppr matchesMethod O.<+> O.ppr matchesAssociatedType))
      TyClD _ d@DataDecl {} ->
        let (n, tyvar_tys) = (tcdName d, lHsQTyVarsToTypes (tyClDeclTyVars d))
        in if isDataConName name
           then SigD noExt <$> extractPatternSyn name n (map HsValArg tyvar_tys) (dd_cons (tcdDataDefn d))
           else SigD noExt <$> extractRecSel name n (map HsValArg tyvar_tys) (dd_cons (tcdDataDefn d))
      TyClD _ FamDecl {}
        | isValName name
        , Just (famInst:_) <- M.lookup name declMap
        -> extractDecl declMap name famInst
      InstD _ (DataFamInstD _ (DataFamInstDecl (HsIB { hsib_body =
                             FamEqn { feqn_tycon = L _ n
                                    , feqn_pats  = tys
                                    , feqn_rhs   = defn }}))) ->
        if isDataConName name
        then SigD noExt <$> extractPatternSyn name n tys (dd_cons defn)
        else SigD noExt <$> extractRecSel name n tys (dd_cons defn)
      InstD _ (ClsInstD _ ClsInstDecl { cid_datafam_insts = insts })
        | isDataConName name ->
            let matches = [ d' | L _ d'@(DataFamInstDecl (HsIB { hsib_body =
                                          FamEqn { feqn_rhs   = dd
                                                 }
                                         })) <- insts
                               , name `elem` map unLoc (concatMap (getConNames . unLoc) (dd_cons dd))
                               ]
            in case matches of
                [d0] -> extractDecl declMap name (noLoc (InstD noExt (DataFamInstD noExt d0)))
                _    -> error "internal: extractDecl (ClsInstD)"
        | otherwise ->
            let matches = [ d' | L _ d'@(DataFamInstDecl (HsIB { hsib_body = d }))
                                   <- insts
                                 -- , L _ ConDecl { con_details = RecCon rec } <- dd_cons (feqn_rhs d)
                               , RecCon rec <- map (getConArgs . unLoc) (dd_cons (feqn_rhs d))
                               , ConDeclField { cd_fld_names = ns } <- map unLoc (unLoc rec)
                               , L _ n <- ns
                               , extFieldOcc n == name
                          ]
            in case matches of
              [d0] -> extractDecl declMap name (noLoc . InstD noExt $ DataFamInstD noExt d0)
              _ -> error "internal: extractDecl (ClsInstD)"
      _ -> O.pprPanic "extractDecl" $
        O.text "Unhandled decl for" O.<+> O.ppr name O.<> O.text ":"
        O.$$ O.nest 4 (O.ppr decl)

extractPatternSyn :: Name -> Name -> [LHsTypeArg GhcRn] -> [LConDecl GhcRn] -> LSig GhcRn
extractPatternSyn nm t tvs cons =
  case filter matches cons of
    [] -> error "extractPatternSyn: constructor pattern not found"
    con:_ -> extract <$> con
 where
  matches :: LConDecl GhcRn -> Bool
  matches (L _ con) = nm `elem` (unLoc <$> getConNames con)
  extract :: ConDecl GhcRn -> Sig GhcRn
  extract con =
    let args =
          case getConArgs con of
            PrefixCon args' -> args'
            RecCon (L _ fields) -> cd_fld_type . unLoc <$> fields
            InfixCon arg1 arg2 -> [arg1, arg2]
        typ = longArrow args (data_ty con)
        typ' =
          case con of
            ConDeclH98 { con_mb_cxt = Just cxt } -> noLoc (HsQualTy noExt cxt typ)
            _ -> typ
        typ'' = noLoc (HsQualTy noExt (noLoc []) typ')
    in PatSynSig noExt [noLoc nm] (mkEmptyImplicitBndrs typ'')

  longArrow :: [LHsType GhcRn] -> LHsType GhcRn -> LHsType GhcRn
  longArrow inputs output = foldr (\x y -> noLoc (HsFunTy noExt x y)) output inputs

  data_ty con
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise = foldl' (\x y -> noLoc (mkAppTyArg x y)) (noLoc (HsTyVar noExt NotPromoted (noLoc t))) tvs
                    where mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
                          mkAppTyArg f (HsValArg ty) = HsAppTy noExt f ty
                          mkAppTyArg f (HsTypeArg l ki) = HsAppKindTy l f ki
                          mkAppTyArg f (HsArgPar _) = HsParTy noExt f

extractRecSel :: Name -> Name -> [LHsTypeArg GhcRn] -> [LConDecl GhcRn]
              -> LSig GhcRn
extractRecSel _ _ _ [] = error "extractRecSel: selector not found"

extractRecSel nm t tvs (L _ con : rest) =
  case getConArgs con of
    RecCon (L _ fields) | ((l,L _ (ConDeclField _ _nn ty _)) : _) <- matching_fields fields ->
      L l (TypeSig noExt [noLoc nm] (mkEmptySigWcType (noLoc (HsFunTy noExt data_ty (getBangType ty)))))
    _ -> extractRecSel nm t tvs rest
 where
  matching_fields :: [LConDeclField GhcRn] -> [(SrcSpan, LConDeclField GhcRn)]
  matching_fields flds = [ (l,f) | f@(L _ (ConDeclField _ ns _ _)) <- flds
                                 , L l n <- ns, extFieldOcc n == nm ]
  data_ty
    -- ResTyGADT _ ty <- con_res con = ty
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise = foldl' (\x y -> noLoc (mkAppTyArg x y)) (noLoc (HsTyVar noExt NotPromoted (noLoc t))) tvs
                   where mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
                         mkAppTyArg f (HsValArg ty) = HsAppTy noExt f ty
                         mkAppTyArg f (HsTypeArg l ki) = HsAppKindTy l f ki
                         mkAppTyArg f (HsArgPar _) = HsParTy noExt f

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
            patsyns = concatMap (getMainDeclBinder . fst) (expItemPats e)
            name = case unLoc $ expItemDecl e of
              InstD _ d -> maybeToList $ M.lookup (getInstLoc d) instMap
              decl      -> getMainDeclBinder decl
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
      | name == name' = return (Just doc)
      | otherwise = search rest
    search (_other_decl : rest) = search rest
