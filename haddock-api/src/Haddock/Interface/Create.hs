{-# LANGUAGE CPP, TupleSections, BangPatterns, LambdaCase #-}
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
import Documentation.Haddock.Utf8 as Utf8
import Haddock.Types
import Haddock.Options
import Haddock.GhcUtils
import Haddock.Utils
import Haddock.Convert
import Haddock.Interface.LexParseRn
import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Ast as Hyperlinker
import Haddock.Backends.Hyperlinker.Parser as Hyperlinker

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Control.Applicative
import Control.Arrow (second)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad
import Data.Traversable

import qualified Packages
import qualified Module
import qualified SrcLoc
import GHC
import HscTypes
import Name
import NameSet
import Bag
import RdrName
import TcRnTypes
import FastString (concatFS)
import BasicTypes ( StringLiteral(..), SourceText(..) )
import qualified Outputable as O
import HsDecls ( getConDetails )

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

      (TcGblEnv { tcg_rdr_env = gre
                , tcg_warns   = warnings
                , tcg_patsyns = patsyns
                }, md) = tm_internals_ tm

  -- The renamed source should always be available to us, but it's best
  -- to be on the safe side.
  (group_, mayExports, mayDocHeader) <-
    case renamedSource tm of
      Nothing -> do
        liftErrMsg $ tell [ "Warning: Renamed source is not available." ]
        return (emptyRnGroup, Nothing, Nothing)
      Just (x, _, y, z) -> return (x, y, z)

  opts0 <- liftErrMsg $ mkDocOpts (haddockOptions dflags) flags mdl
  let opts
        | Flag_IgnoreAllExports `elem` flags = OptIgnoreExports : opts0
        | otherwise = opts0

  -- Process the top-level module header documentation.
  (!info, mbDoc) <- liftErrMsg $ processModuleHeader dflags gre safety mayDocHeader

  let declsWithDocs = topDecls group_

      exports0 = fmap (reverse . map unLoc) mayExports
      exports
        | OptIgnoreExports `elem` opts = Nothing
        | otherwise = exports0
      warningMap = mkWarningMap dflags warnings gre exportedNames

      localBundledPatSyns :: Map Name [Name]
      localBundledPatSyns =
        case exports of
          Nothing  -> M.empty
          Just ies ->
            M.map (nubByName id) $
            M.fromListWith (++) [ (ieWrappedName ty_name, bundled_patsyns)
                                | IEThingWith (L _ ty_name) _ exported _ <- ies
                                , let bundled_patsyns =
                                        filter is_patsyn (map (ieWrappedName . unLoc) exported)
                                , not (null bundled_patsyns)
                                ]
        where
          is_patsyn name = elemNameSet name (mkNameSet (map getName patsyns))

      fixMap = mkFixMap group_
      (decls, _) = unzip declsWithDocs
      localInsts = filter (nameIsLocalOrFrom sem_mdl)
                        $  map getName instances
                        ++ map getName fam_instances
      -- Locations of all TH splices
      splices = [ l | L l (SpliceD _) <- hsmodDecls hsm ]

      maps@(!docMap, !argMap, !subMap, !declMap, _) =
        mkMaps dflags gre localInsts declsWithDocs

  let allWarnings = M.unions (warningMap : map ifaceWarningMap (M.elems modMap))

  -- The MAIN functionality: compute the export items which will
  -- each be the actual documentation of this module.
  exportItems <- mkExportItems is_sig modMap mdl sem_mdl allWarnings gre exportedNames decls
                   maps localBundledPatSyns fixMap splices exports instIfaceMap dflags

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
      modWarn = moduleWarning dflags gre warnings

  tokenizedSrc <- mkMaybeTokenizedSrc flags tm

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
  , ifaceBundledPatSynMap  = localBundledPatSyns
  , ifaceSubMap            = subMap
  , ifaceFixMap            = fixMap
  , ifaceModuleAliases     = aliases
  , ifaceInstances         = instances
  , ifaceFamInstances      = fam_instances
  , ifaceOrphanInstances   = [] -- Filled in `attachInstances`
  , ifaceRnOrphanInstances = [] -- Filled in `renameInterface`
  , ifaceHaddockCoverage   = coverage
  , ifaceWarningMap        = warningMap
  , ifaceTokenizedSrc      = tokenizedSrc
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

-- Similar to GHC.lookupModule
-- ezyang: Not really...
lookupModuleDyn ::
  DynFlags -> Maybe UnitId -> ModuleName -> Module
lookupModuleDyn _ (Just pkgId) mdlName =
  Module.mkModule pkgId mdlName
lookupModuleDyn dflags Nothing mdlName =
  case Packages.lookupModuleInAllPackages dflags mdlName of
    (m,_):_ -> m
    [] -> Module.mkModule Module.mainUnitId mdlName


-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

mkWarningMap :: DynFlags -> Warnings -> GlobalRdrEnv -> [Name] -> WarningMap
mkWarningMap dflags warnings gre exps = case warnings of
  NoWarnings  -> M.empty
  WarnAll _   -> M.empty
  WarnSome ws ->
    let ws' = [ (n, w) | (occ, w) <- ws, elt <- lookupGlobalRdrEnv gre occ
              , let n = gre_name elt, n `elem` exps ]
    in M.fromList $ map (second $ parseWarning dflags gre) ws'

moduleWarning :: DynFlags -> GlobalRdrEnv -> Warnings -> Maybe (Doc Name)
moduleWarning _ _ NoWarnings = Nothing
moduleWarning _ _ (WarnSome _) = Nothing
moduleWarning dflags gre (WarnAll w) = Just $ parseWarning dflags gre w

parseWarning :: DynFlags -> GlobalRdrEnv -> WarningTxt -> Doc Name
parseWarning dflags gre w = force $ case w of
  DeprecatedTxt _ msg -> format "Deprecated: " (concatFS $ map (sl_fs . unLoc) msg)
  WarningTxt    _ msg -> format "Warning: "    (concatFS $ map (sl_fs . unLoc) msg)
  where
    format x xs = DocWarning . DocParagraph . DocAppend (DocString x)
                  . processDocString dflags gre $ HsDocString xs


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
  hm <- if Flag_HideModule (moduleString mdl) `elem` flags
        then return $ OptHide : opts
        else return opts
  if Flag_ShowExtensions (moduleString mdl) `elem` flags
    then return $ OptShowExtensions : hm
    else return hm


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


type Maps = (DocMap Name, ArgMap Name, SubMap, DeclMap, InstMap)

-- | Create 'Maps' by looping through the declarations. For each declaration,
-- find its names, its subordinates, and its doc strings. Process doc strings
-- into 'Doc's.
mkMaps :: DynFlags
       -> GlobalRdrEnv
       -> [Name]
       -> [(LHsDecl Name, [HsDocString])]
       -> Maps
mkMaps dflags gre instances decls =
  let
    (a, b, c, d) = unzip4 $ map mappings decls
  in ( f' (map (nubByName fst) a)
     , f  (filterMapping (not . M.null) b)
     , f  (filterMapping (not . null) c)
     , f  (filterMapping (not . null) d)
     , instanceMap
     )
  where
    f :: (Ord a, Monoid b) => [[(a, b)]] -> Map a b
    f = M.fromListWith (<>) . concat

    f' :: [[(Name, MDoc Name)]] -> Map Name (MDoc Name)
    f' = M.fromListWith metaDocAppend . concat

    filterMapping :: (b -> Bool) ->  [[(a, b)]] -> [[(a, b)]]
    filterMapping p = map (filter (p . snd))

    mappings :: (LHsDecl Name, [HsDocString])
             -> ( [(Name, MDoc Name)]
                , [(Name, Map Int (MDoc Name))]
                , [(Name, [Name])]
                , [(Name,  [LHsDecl Name])]
                )
    mappings (ldecl, docStrs) =
      let L l decl = ldecl
          declDoc :: [HsDocString] -> Map Int HsDocString
                  -> (Maybe (MDoc Name), Map Int (MDoc Name))
          declDoc strs m =
            let doc' = processDocStrings dflags gre strs
                m' = M.map (processDocStringParas dflags gre) m
            in (doc', m')
          (doc, args) = declDoc docStrs (typeDocs decl)
          subs :: [(Name, [HsDocString], Map Int HsDocString)]
          subs = subordinates instanceMap decl
          (subDocs, subArgs) = unzip $ map (\(_, strs, m) -> declDoc strs m) subs
          ns = names l decl
          subNs = [ n | (n, _, _) <- subs ]
          dm = [ (n, d) | (n, Just d) <- zip ns (repeat doc) ++ zip subNs subDocs ]
          am = [ (n, args) | n <- ns ] ++ zip subNs subArgs
          sm = [ (n, subNs) | n <- ns ]
          cm = [ (n, [ldecl]) | n <- ns ++ subNs ]
      in seqList ns `seq`
          seqList subNs `seq`
          doc `seq`
          seqList subDocs `seq`
          seqList subArgs `seq`
          (dm, am, sm, cm)

    instanceMap :: Map SrcSpan Name
    instanceMap = M.fromList [ (getSrcSpan n, n) | n <- instances ]

    names :: SrcSpan -> HsDecl Name -> [Name]
    names l (InstD d) = maybeToList (M.lookup loc instanceMap) -- See note [2].
      where loc = case d of
              TyFamInstD _ -> l -- The CoAx's loc is the whole line, but only for TFs
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
             -> HsDecl Name
             -> [(Name, [HsDocString], Map Int HsDocString)]
subordinates instMap decl = case decl of
  InstD (ClsInstD d) -> do
    DataFamInstDecl { dfid_tycon = L l _
                    , dfid_defn = def    } <- unLoc <$> cid_datafam_insts d
    [ (n, [], M.empty) | Just n <- [M.lookup l instMap] ] ++ dataSubs def

  InstD (DataFamInstD d)  -> dataSubs (dfid_defn d)
  TyClD d | isClassDecl d -> classSubs d
          | isDataDecl  d -> dataSubs (tcdDataDefn d)
  _ -> []
  where
    classSubs dd = [ (name, doc, typeDocs d) | (L _ d, doc) <- classDecls dd
                   , name <- getMainDeclBinder d, not (isValD d)
                   ]
    dataSubs :: HsDataDefn Name -> [(Name, [HsDocString], Map Int HsDocString)]
    dataSubs dd = constrs ++ fields ++ derivs
      where
        cons = map unL $ (dd_cons dd)
        constrs = [ (unL cname, maybeToList $ fmap unL $ con_doc c, M.empty)
                  | c <- cons, cname <- getConNames c ]
        fields  = [ (selectorFieldOcc n, maybeToList $ fmap unL doc, M.empty)
                  | RecCon flds <- map getConDetails cons
                  , L _ (ConDeclField ns _ doc) <- (unLoc flds)
                  , L _ n <- ns ]
        derivs  = [ (instName, [unL doc], M.empty)
                  | HsIB { hsib_body = L l (HsDocTy _ doc) }
                      <- concatMap (unLoc . deriv_clause_tys . unLoc) $
                           unLoc $ dd_derivs dd
                  , Just instName <- [M.lookup l instMap] ]

-- | Extract function argument docs from inside types.
typeDocs :: HsDecl Name -> Map Int HsDocString
typeDocs d =
  let docs = go 0 in
  case d of
    SigD (TypeSig _ ty)      -> docs (unLoc (hsSigWcType ty))
    SigD (ClassOpSig _ _ ty) -> docs (unLoc (hsSigType ty))
    SigD (PatSynSig _ ty)    -> docs (unLoc (hsSigType ty))
    ForD (ForeignImport _ ty _ _)   -> docs (unLoc (hsSigType ty))
    TyClD (SynDecl { tcdRhs = ty }) -> docs (unLoc ty)
    _ -> M.empty
  where
    go n (HsForAllTy { hst_body = ty }) = go n (unLoc ty)
    go n (HsQualTy   { hst_body = ty }) = go n (unLoc ty)
    go n (HsFunTy (L _ (HsDocTy _ (L _ x))) (L _ ty)) = M.insert n x $ go (n+1) ty
    go n (HsFunTy _ ty) = go (n+1) (unLoc ty)
    go n (HsDocTy _ (L _ doc)) = M.singleton n doc
    go _ _ = M.empty


-- | All the sub declarations of a class (that we handle), ordered by
-- source location, with documentation attached if it exists.
classDecls :: TyClDecl Name -> [(LHsDecl Name, [HsDocString])]
classDecls class_ = filterDecls . collectDocs . sortByLoc $ decls
  where
    decls = docs ++ defs ++ sigs ++ ats
    docs  = mkDecls tcdDocs DocD class_
    defs  = mkDecls (bagToList . tcdMeths) ValD class_
    sigs  = mkDecls tcdSigs SigD class_
    ats   = mkDecls tcdATs (TyClD . FamDecl) class_


-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup Name -> [(LHsDecl Name, [HsDocString])]
topDecls = filterClasses . filterDecls . collectDocs . sortByLoc . ungroup

-- | Extract a map of fixity declarations only
mkFixMap :: HsGroup Name -> FixMap
mkFixMap group_ = M.fromList [ (n,f)
                             | L _ (FixitySig ns f) <- hs_fixds group_,
                               L _ n <- ns ]


-- | Take all declarations except pragmas, infix decls, rules from an 'HsGroup'.
ungroup :: HsGroup Name -> [LHsDecl Name]
ungroup group_ =
  mkDecls (tyClGroupTyClDecls . hs_tyclds) TyClD  group_ ++
  mkDecls hs_derivds             DerivD group_ ++
  mkDecls hs_defds               DefD   group_ ++
  mkDecls hs_fords               ForD   group_ ++
  mkDecls hs_docs                DocD   group_ ++
  mkDecls (tyClGroupInstDecls . hs_tyclds) InstD  group_ ++
  mkDecls (typesigs . hs_valds)  SigD   group_ ++
  mkDecls (valbinds . hs_valds)  ValD   group_
  where
    typesigs (ValBindsOut _ sigs) = filter isUserLSig sigs
    typesigs _ = error "expected ValBindsOut"

    valbinds (ValBindsOut binds _) = concatMap bagToList . snd . unzip $ binds
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
    isHandled (ForD (ForeignImport {})) = True
    isHandled (TyClD {})  = True
    isHandled (InstD {})  = True
    isHandled (DerivD {}) = True
    isHandled (SigD d) = isUserLSig (reL d)
    isHandled (ValD _) = True
    -- we keep doc declarations to be able to get at named docs
    isHandled (DocD _) = True
    isHandled _ = False


-- | Go through all class declarations and filter their sub-declarations
filterClasses :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterClasses decls = [ if isClassD d then (L loc (filterClass d), doc) else x
                      | x@(L loc d, doc) <- decls ]
  where
    filterClass (TyClD c) =
      TyClD $ c { tcdSigs = filter (liftA2 (||) isUserLSig isMinimalLSig) $ tcdSigs c }
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
    go prev docs (L _ (DocD (DocCommentNext str)) : ds)
      | Nothing <- prev = go Nothing (str:docs) ds
      | Just decl <- prev = finished decl docs (go Nothing [str] ds)
    go prev docs (L _ (DocD (DocCommentPrev str)) : ds) = go prev (str:docs) ds
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
  -> Module             -- this module
  -> Module             -- semantic module
  -> WarningMap
  -> GlobalRdrEnv
  -> [Name]             -- exported names (orig)
  -> [LHsDecl Name]     -- renamed source declarations
  -> Maps
  -> Map Name [Name]
  -> FixMap
  -> [SrcSpan]          -- splice locations
  -> Maybe [IE Name]
  -> InstIfaceMap
  -> DynFlags
  -> ErrMsgGhc [ExportItem Name]
mkExportItems
  is_sig modMap thisMod semMod warnings gre exportedNames decls
  maps@(docMap, argMap, subMap, declMap, instMap) patSynMap fixMap splices optExports instIfaceMap dflags =
  case optExports of
    Nothing -> fullModuleContents dflags warnings gre maps fixMap splices decls
    Just exports -> liftM concat $ mapM lookupExport exports
  where
    lookupExport (IEVar (L _ x))         = declWith [] $ ieWrappedName x
    lookupExport (IEThingAbs (L _ t))    = declWith [] $ ieWrappedName t
    lookupExport (IEThingAll (L _ t))    = do
      let name     = ieWrappedName t
      pats <- findBundledPatterns name
      declWith pats name
    lookupExport (IEThingWith (L _ t) _ _ _) = do
      let name     = ieWrappedName t
      pats <- findBundledPatterns name
      declWith pats name
    lookupExport (IEModuleContents (L _ m)) =
      -- TODO: We could get more accurate reporting here if IEModuleContents
      -- also recorded the actual names that are exported here.  We CAN
      -- compute this info using @gre@ but 'moduleExports does not seem to
      -- do so.
      -- NB: Pass in identity module, so we can look it up in index correctly
      moduleExports thisMod m dflags warnings gre exportedNames decls modMap instIfaceMap maps fixMap splices
    lookupExport (IEGroup lev docStr)  = return $
      return . ExportGroup lev "" $ processDocString dflags gre docStr

    lookupExport (IEDoc docStr)        = return $
      return . ExportDoc $ processDocStringParas dflags gre docStr

    lookupExport (IEDocNamed str)      = liftErrMsg $
      findNamedDoc str [ unL d | d <- decls ] >>= return . \case
        Nothing -> []
        Just doc -> return . ExportDoc $ processDocStringParas dflags gre doc

    declWith :: [(HsDecl Name, DocForDecl Name)] -> Name -> ErrMsgGhc [ ExportItem Name ]
    declWith pats t = do
      r <- findDecl t
      case r of
        ([L l (ValD _)], (doc, _)) -> do
          -- Top-level binding without type signature
          export <- hiValExportItem dflags t l doc (l `elem` splices) $ M.lookup t fixMap
          return [export]
        (ds, docs_) | decl : _ <- filter (not . isValD . unLoc) ds ->
          let declNames = getMainDeclBinder (unL decl)
          in case () of
            _
              -- TODO: temp hack: we filter out separately exported ATs, since we haven't decided how
              -- to handle them yet. We should really give an warning message also, and filter the
              -- name out in mkVisibleNames...
              | t `elem` declATs (unL decl)        -> return []

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
                  L loc (SigD sig) ->
                    -- fromJust is safe since we already checked in guards
                    -- that 't' is a name declared in this declaration.
                    let newDecl = L loc . SigD . fromJust $ filterSigNames (== t) sig
                    in return [ mkExportDecl t newDecl pats docs_ ]

                  L loc (TyClD cl@ClassDecl{}) -> do
                    mdef <- liftGhcToErrMsgGhc $ minimalDef t
                    let sig = maybeToList $ fmap (noLoc . MinimalSig NoSourceText . noLoc . fmap noLoc) mdef
                    return [ mkExportDecl t
                      (L loc $ TyClD cl { tcdSigs = sig ++ tcdSigs cl }) pats docs_ ]

                  _ -> return [ mkExportDecl t decl pats docs_ ]

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
                   let subs_ = [ (n, noDocForDecl) | (n, _, _) <- subordinates instMap (unLoc decl) ]
                   return [ mkExportDecl t decl pats (noDocForDecl, subs_) ]
                Just iface ->
                   return [ mkExportDecl t decl pats (lookupDocs t warnings (instDocMap iface) (instArgMap iface) (instSubMap iface)) ]

        _ -> return []


    mkExportDecl :: Name -> LHsDecl Name -> [(HsDecl Name, DocForDecl Name)]
                 -> (DocForDecl Name, [(Name, DocForDecl Name)]) -> ExportItem Name
    mkExportDecl name decl pats (doc, subs) = decl'
      where
        decl' = ExportDecl (restrictTo sub_names (extractDecl name decl)) pats' doc subs' [] fixities False
        subs' = filter (isExported . fst) subs
        pats' = [ d | d@(patsyn_decl, _) <- pats
                    , all isExported (getMainDeclBinder patsyn_decl) ]
        sub_names = map fst subs'
        pat_names = [ n | (patsyn_decl, _) <- pats', n <- getMainDeclBinder patsyn_decl]
        fixities = [ (n, f) | n <- name:sub_names++pat_names, Just f <- [M.lookup n fixMap] ]

    exportedNameSet = mkNameSet exportedNames
    isExported n = elemNameSet n exportedNameSet

    findDecl :: Name -> ErrMsgGhc ([LHsDecl Name], (DocForDecl Name, [(Name, DocForDecl Name)]))
    findDecl n
      | m == semMod =
          case M.lookup n declMap of
            Just ds -> return (ds, lookupDocs n warnings docMap argMap subMap)
            Nothing
              | is_sig -> do
                -- OK, so it wasn't in the local declaration map.  It could
                -- have been inherited from a signature.  Reconstitute it
                -- from the type.
                mb_r <- hiDecl dflags n
                case mb_r of
                    Nothing -> return ([], (noDocForDecl, []))
                    -- TODO: If we try harder, we might be able to find
                    -- a Haddock!  Look in the Haddocks for each thing in
                    -- requirementContext (pkgState)
                    Just decl -> return ([decl], (noDocForDecl, []))
              | otherwise ->
                return ([], (noDocForDecl, []))
      | Just iface <- M.lookup (semToIdMod (moduleUnitId thisMod) m) modMap
      , Just ds <- M.lookup n (ifaceDeclMap iface) =
          return (ds, lookupDocs n warnings
                            (ifaceDocMap iface)
                            (ifaceArgMap iface)
                            (ifaceSubMap iface))
      | otherwise = return ([], (noDocForDecl, []))
      where
        m = nameModule n

    findBundledPatterns :: Name -> ErrMsgGhc [(HsDecl Name, DocForDecl Name)]
    findBundledPatterns t =
      let
        m = nameModule t

        local_bundled_patsyns =
          M.findWithDefault [] t patSynMap

        iface_bundled_patsyns
          | Just iface <- M.lookup (semToIdMod (moduleUnitId thisMod) m) modMap
          , Just patsyns <- M.lookup t (ifaceBundledPatSynMap iface)
          = patsyns

          | Just iface <- M.lookup m instIfaceMap
          , Just patsyns <- M.lookup t (instBundledPatSynMap iface)
          = patsyns

          | otherwise
          = []

        patsyn_decls = do
          for (local_bundled_patsyns ++ iface_bundled_patsyns) $ \patsyn_name -> do
            -- call declWith here so we don't have to prepare the pattern synonym for
            -- showing ourselves.
            export_items <- declWith [] patsyn_name
            pure [ (unLoc patsyn_decl, patsyn_doc)
                 | ExportDecl {
                       expItemDecl  = patsyn_decl
                     , expItemMbDoc = patsyn_doc
                     } <- export_items
                 ]

      in concat <$> patsyn_decls

-- | Given a 'Module' from a 'Name', convert it into a 'Module' that
-- we can actually find in the 'IfaceMap'.
semToIdMod :: UnitId -> Module -> Module
semToIdMod this_uid m
    | Module.isHoleModule m = mkModule this_uid (moduleName m)
    | otherwise      = m

hiDecl :: DynFlags -> Name -> ErrMsgGhc (Maybe (LHsDecl Name))
hiDecl dflags t = do
  mayTyThing <- liftGhcToErrMsgGhc $ lookupName t
  case mayTyThing of
    Nothing -> do
      liftErrMsg $ tell ["Warning: Not found in environment: " ++ pretty dflags t]
      return Nothing
    Just x -> case tyThingToLHsDecl x of
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
                -> Maybe Fixity -> ErrMsgGhc (ExportItem Name)
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
lookupDocs :: Name -> WarningMap -> DocMap Name -> ArgMap Name -> SubMap
           -> (DocForDecl Name, [(Name, DocForDecl Name)])
lookupDocs n warnings docMap argMap subMap =
  let lookupArgDoc x = M.findWithDefault M.empty x argMap in
  let doc = (lookupDoc n, lookupArgDoc n) in
  let subs = M.findWithDefault [] n subMap in
  let subDocs = [ (s, (lookupDoc s, lookupArgDoc s)) | s <- subs ] in
  (doc, subDocs)
  where
    lookupDoc name = Documentation (M.lookup name docMap) (M.lookup name warnings)


-- | Return all export items produced by an exported module. That is, we're
-- interested in the exports produced by \"module B\" in such a scenario:
--
-- > module A (module B) where
-- > import B (...) hiding (...)
--
-- There are three different cases to consider:
--
-- 1) B is hidden, in which case we return all its exports that are in scope in A.
-- 2) B is visible, but not all its exports are in scope in A, in which case we
--    only return those that are.
-- 3) B is visible and all its exports are in scope, in which case we return
--    a single 'ExportModule' item.
moduleExports :: Module           -- ^ Module A (identity, NOT semantic)
              -> ModuleName       -- ^ The real name of B, the exported module
              -> DynFlags         -- ^ The flags used when typechecking A
              -> WarningMap
              -> GlobalRdrEnv     -- ^ The renaming environment used for A
              -> [Name]           -- ^ All the exports of A
              -> [LHsDecl Name]   -- ^ All the renamed declarations in A
              -> IfaceMap         -- ^ Already created interfaces
              -> InstIfaceMap     -- ^ Interfaces in other packages
              -> Maps
              -> FixMap
              -> [SrcSpan]        -- ^ Locations of all TH splices
              -> ErrMsgGhc [ExportItem Name] -- ^ Resulting export items
moduleExports thisMod expMod dflags warnings gre _exports decls ifaceMap instIfaceMap maps fixMap splices
  | expMod == moduleName thisMod
  = fullModuleContents dflags warnings gre maps fixMap splices decls
  | otherwise =
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
fullModuleContents :: DynFlags
                   -> WarningMap
                   -> GlobalRdrEnv      -- ^ The renaming environment
                   -> Maps
                   -> FixMap
                   -> [SrcSpan]         -- ^ Locations of all TH splices
                   -> [LHsDecl Name]    -- ^ All the renamed declarations
                   -> ErrMsgGhc [ExportItem Name]
fullModuleContents dflags warnings gre (docMap, argMap, subMap, declMap, instMap) fixMap splices decls =
  liftM catMaybes $ mapM mkExportItem (expandSigDecls decls)
  where
    -- A type signature can have multiple names, like:
    --   foo, bar :: Types..
    --
    -- We go through the list of declarations and expand type signatures, so
    -- that every type signature has exactly one name!
    expandSigDecls :: [LHsDecl name] -> [LHsDecl name]
    expandSigDecls = concatMap f
      where
        f (L l (SigD sig))              = [ L l (SigD s) | s <- expandSig sig ]

        -- also expand type signatures for class methods
        f (L l (TyClD cls@ClassDecl{})) =
          [ L l (TyClD cls { tcdSigs = concatMap expandLSig (tcdSigs cls) }) ]
        f x = [x]

    expandLSig :: LSig name -> [LSig name]
    expandLSig (L l sig) = [ L l s | s <- expandSig sig ]

    expandSig :: Sig name -> [Sig name]
    expandSig (TypeSig names t)      = [ TypeSig [n] t      | n <- names ]
    expandSig (ClassOpSig b names t) = [ ClassOpSig b [n] t | n <- names ]
    expandSig (PatSynSig names t)    = [ PatSynSig [n] t    | n <- names ]
    expandSig x                      = [x]

    mkExportItem :: LHsDecl Name -> ErrMsgGhc (Maybe (ExportItem Name))
    mkExportItem (L _ (DocD (DocGroup lev docStr))) = do
      return . Just . ExportGroup lev "" $ processDocString dflags gre docStr
    mkExportItem (L _ (DocD (DocCommentNamed _ docStr))) = do
      return . Just . ExportDoc $ processDocStringParas dflags gre docStr
    mkExportItem (L l (ValD d))
      | name:_ <- collectHsBindBinders d, Just [L _ (ValD _)] <- M.lookup name declMap =
          -- Top-level binding without type signature.
          let (doc, _) = lookupDocs name warnings docMap argMap subMap in
          fmap Just (hiValExportItem dflags name l doc (l `elem` splices) $ M.lookup name fixMap)
      | otherwise = return Nothing
    mkExportItem decl@(L l (InstD d))
      | Just name <- M.lookup (getInstLoc d) instMap =
        expInst decl l name
    mkExportItem decl@(L l (DerivD {}))
      | Just name <- M.lookup l instMap =
        expInst decl l name
    mkExportItem (L l (TyClD cl@ClassDecl{ tcdLName = L _ name, tcdSigs = sigs })) = do
      mdef <- liftGhcToErrMsgGhc $ minimalDef name
      let sig = maybeToList $ fmap (noLoc . MinimalSig NoSourceText . noLoc . fmap noLoc) mdef
      expDecl (L l (TyClD cl { tcdSigs = sig ++ sigs })) l name
    mkExportItem decl@(L l d)
      | name:_ <- getMainDeclBinder d = expDecl decl l name
      | otherwise = return Nothing

    fixities name subs = [ (n,f) | n <- name : map fst subs
                                 , Just f <- [M.lookup n fixMap] ]

    expDecl decl l name = return $ Just (ExportDecl decl [] doc subs [] (fixities name subs) (l `elem` splices))
      where (doc, subs) = lookupDocs name warnings docMap argMap subMap

    expInst decl l name =
        let (doc, subs) = lookupDocs name warnings docMap argMap subMap in
        return $ Just (ExportDecl decl [] doc subs [] (fixities name subs) (l `elem` splices))


-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble
-- together a type signature for it...).
extractDecl :: Name -> LHsDecl Name -> LHsDecl Name
extractDecl name decl
  | name `elem` getMainDeclBinder (unLoc decl) = decl
  | otherwise  =
    case unLoc decl of
      TyClD d@ClassDecl {} ->
        let matches = [ lsig
                      | lsig <- tcdSigs d
                      , ClassOpSig False _ _ <- pure $ unLoc lsig
                        -- Note: exclude `default` declarations (see #505)
                      , name `elem` sigName lsig
                      ]
            -- TODO: document fixity
        in case matches of
          [s0] -> let (n, tyvar_names) = (tcdName d, tyClDeclTyVars d)
                      L pos sig = addClassContext n tyvar_names s0
                  in L pos (SigD sig)
          _ -> O.pprPanic "extractDecl" (O.text "Ambiguous decl for" O.<+> O.ppr name O.<+> O.text "in class:"
                                         O.$$ O.nest 4 (O.ppr d)
                                         O.$$ O.text "Matches:"
                                         O.$$ O.nest 4 (O.ppr matches))
      TyClD d@DataDecl {} ->
        let (n, tyvar_tys) = (tcdName d, lHsQTyVarsToTypes (tyClDeclTyVars d))
        in SigD <$> extractRecSel name n tyvar_tys (dd_cons (tcdDataDefn d))
      InstD (DataFamInstD DataFamInstDecl { dfid_tycon = L _ n
                                          , dfid_pats = HsIB { hsib_body = tys }
                                          , dfid_defn = defn }) ->
        SigD <$> extractRecSel name n tys (dd_cons defn)
      InstD (ClsInstD ClsInstDecl { cid_datafam_insts = insts }) ->
        let matches = [ d | L _ d <- insts
                          -- , L _ ConDecl { con_details = RecCon rec } <- dd_cons (dfid_defn d)
                          , RecCon rec <- map (getConDetails . unLoc) (dd_cons (dfid_defn d))
                          , ConDeclField { cd_fld_names = ns } <- map unLoc (unLoc rec)
                          , L _ n <- ns
                          , selectorFieldOcc n == name
                      ]
        in case matches of
          [d0] -> extractDecl name (noLoc . InstD $ DataFamInstD d0)
          _ -> error "internal: extractDecl (ClsInstD)"
      _ -> error "internal: extractDecl"

extractRecSel :: Name -> Name -> [LHsType Name] -> [LConDecl Name]
              -> LSig Name
extractRecSel _ _ _ [] = error "extractRecSel: selector not found"

extractRecSel nm t tvs (L _ con : rest) =
  case getConDetails con of
    RecCon (L _ fields) | ((l,L _ (ConDeclField _nn ty _)) : _) <- matching_fields fields ->
      L l (TypeSig [noLoc nm] (mkEmptySigWcType (noLoc (HsFunTy data_ty (getBangType ty)))))
    _ -> extractRecSel nm t tvs rest
 where
  matching_fields :: [LConDeclField Name] -> [(SrcSpan, LConDeclField Name)]
  matching_fields flds = [ (l,f) | f@(L _ (ConDeclField ns _ _)) <- flds
                                 , L l n <- ns, selectorFieldOcc n == nm ]
  data_ty
    -- ResTyGADT _ ty <- con_res con = ty
    | ConDeclGADT{} <- con = hsib_body $ con_type con
    | otherwise = foldl' (\x y -> noLoc (HsAppTy x y)) (noLoc (HsTyVar NotPromoted (noLoc t))) tvs

-- | Keep export items with docs.
pruneExportItems :: [ExportItem Name] -> [ExportItem Name]
pruneExportItems = filter hasDoc
  where
    hasDoc (ExportDecl{expItemMbDoc = (Documentation d _, _)}) = isJust d
    hasDoc _ = True


mkVisibleNames :: Maps -> [ExportItem Name] -> [DocOption] -> [Name]
mkVisibleNames (_, _, _, _, instMap) exports opts
  | OptHide `elem` opts = []
  | otherwise = let ns = concatMap exportName exports
                in seqList ns `seq` ns
  where
    exportName e@ExportDecl {} = name ++ subs ++ patsyns
      where subs    = map fst (expItemSubDocs e)
            patsyns = concatMap (getMainDeclBinder . fst) (expItemPats e)
            name = case unLoc $ expItemDecl e of
              InstD d -> maybeToList $ M.lookup (getInstLoc d) instMap
              decl    -> getMainDeclBinder decl
    exportName ExportNoDecl {} = [] -- we don't count these as visible, since
                                    -- we don't want links to go to them.
    exportName _ = []

seqList :: [a] -> ()
seqList [] = ()
seqList (x : xs) = x `seq` seqList xs

mkMaybeTokenizedSrc :: [Flag] -> TypecheckedModule
                    -> ErrMsgGhc (Maybe [RichToken])
mkMaybeTokenizedSrc flags tm
    | Flag_HyperlinkedSource `elem` flags = case renamedSource tm of
        Just src -> do
            tokens <- liftGhcToErrMsgGhc . liftIO $ mkTokenizedSrc summary src
            return $ Just tokens
        Nothing -> do
            liftErrMsg . tell . pure $ concat
                [ "Warning: Cannot hyperlink module \""
                , moduleNameString . ms_mod_name $ summary
                , "\" because renamed source is not available"
                ]
            return Nothing
    | otherwise = return Nothing
  where
    summary = pm_mod_summary . tm_parsed_module $ tm

mkTokenizedSrc :: ModSummary -> RenamedSource -> IO [RichToken]
mkTokenizedSrc ms src = do
  -- make sure to read the whole file at once otherwise
  -- we run out of file descriptors (see #495)
  rawSrc <- BS.readFile (msHsFilePath ms) >>= evaluate
  return $ Hyperlinker.enrich src (Hyperlinker.parse (decodeUtf8 rawSrc))

-- | Find a stand-alone documentation comment by its name.
findNamedDoc :: String -> [HsDecl Name] -> ErrMsgM (Maybe HsDocString)
findNamedDoc name = search
  where
    search [] = do
      tell ["Cannot find documentation for: $" ++ name]
      return Nothing
    search (DocD (DocCommentNamed name' doc) : rest)
      | name == name' = return (Just doc)
      | otherwise = search rest
    search (_other_decl : rest) = search rest
