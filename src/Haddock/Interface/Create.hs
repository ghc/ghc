{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.Create
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Interface.Create (createInterface) where


import Haddock.Types
import Haddock.Options
import Haddock.GhcUtils
import Haddock.Utils
import Haddock.Convert
import Haddock.Interface.LexParseRn

import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Control.Applicative
import Control.Monad
import qualified Data.Traversable as Traversable

import GHC hiding (flags)
import HscTypes
import Name
import Bag
import RdrName (GlobalRdrEnv)


-- | Use a 'TypecheckedModule' to produce an 'Interface'.
-- To do this, we need access to already processed modules in the topological
-- sort. That's what's in the 'IfaceMap'.
createInterface :: TypecheckedModule -> [Flag] -> IfaceMap -> InstIfaceMap -> ErrMsgGhc Interface
createInterface tm flags modMap instIfaceMap = do

  let ms            = pm_mod_summary . tm_parsed_module $ tm
      mi            = moduleInfo tm
      mdl           = ms_mod ms
      dflags        = ms_hspp_opts ms
      instances     = modInfoInstances mi
      exportedNames = modInfoExports mi

  -- The renamed source should always be available to us, but it's best
  -- to be on the safe side.
  (group_, mayExports, mayDocHeader) <-
    case renamedSource tm of
      Nothing -> do
        liftErrMsg $ tell [ "Warning: Renamed source is not available." ]
        return (emptyRnGroup, Nothing, Nothing)
      Just (x, _, y, z) -> return (x, y, z)

  -- The pattern-match should not fail, because createInterface is only
  -- done on loaded modules.
  Just gre <- liftGhcToErrMsgGhc $ lookupLoadedHomeModuleGRE (moduleName mdl)

  opts0 <- liftErrMsg $ mkDocOpts (haddockOptions dflags) flags mdl
  let opts
        | Flag_IgnoreAllExports `elem` flags = OptIgnoreExports : opts0
        | otherwise = opts0

  (info, mbDoc) <- liftErrMsg $ lexParseRnHaddockModHeader dflags gre mayDocHeader

  let declsWithDocs = topDecls group_
      (decls, _) = unzip declsWithDocs
      localInsts = filter (nameIsLocalOrFrom mdl . getName) instances

  maps@(docMap, argMap, subMap, declMap) <-
    liftErrMsg $ mkMaps dflags gre localInsts exportedNames declsWithDocs

  let exports0 = fmap (reverse . map unLoc) mayExports
      exports
       | OptIgnoreExports `elem` opts = Nothing
       | otherwise = exports0

  liftErrMsg $ warnAboutFilteredDecls mdl decls

  exportItems <- mkExportItems modMap mdl gre exportedNames decls maps exports
                   instances instIfaceMap dflags

  let visibleNames = mkVisibleNames exportItems opts

  -- Measure haddock documentation coverage.
  let
    prunedExportItems0 = pruneExportItems exportItems
    haddockable = 1 + length exportItems -- module + exports
    haddocked = (if isJust mbDoc then 1 else 0) + length prunedExportItems0
    coverage = (haddockable, haddocked)

  -- Prune the export list to just those declarations that have
  -- documentation, if the 'prune' option is on.
  let
    prunedExportItems
      | OptPrune `elem` opts = prunedExportItems0
      | otherwise = exportItems

  return Interface {
    ifaceMod             = mdl,
    ifaceOrigFilename    = msHsFilePath ms,
    ifaceInfo            = info,
    ifaceDoc             = mbDoc,
    ifaceRnDoc           = Nothing,
    ifaceOptions         = opts,
    ifaceDocMap          = docMap,
    ifaceArgMap          = argMap,
    ifaceRnDocMap        = M.empty,
    ifaceRnArgMap        = M.empty,
    ifaceExportItems     = prunedExportItems,
    ifaceRnExportItems   = [],
    ifaceExports         = exportedNames,
    ifaceVisibleExports  = visibleNames,
    ifaceDeclMap         = declMap,
    ifaceSubMap          = subMap,
    ifaceInstances       = instances,
    ifaceHaddockCoverage = coverage
  }


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
  if Flag_HideModule (moduleString mdl) `elem` flags
    then return $ OptHide : opts
    else return opts


parseOption :: String -> ErrMsgM (Maybe DocOption)
parseOption "hide"           = return (Just OptHide)
parseOption "prune"          = return (Just OptPrune)
parseOption "ignore-exports" = return (Just OptIgnoreExports)
parseOption "not-home"       = return (Just OptNotHome)
parseOption other = tell ["Unrecognised option: " ++ other] >> return Nothing


--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------


type Maps = (DocMap Name, ArgMap Name, SubMap, DeclMap)


mkMaps :: DynFlags -> GlobalRdrEnv -> [Instance] -> [Name] -> [(LHsDecl Name, [HsDocString])] -> ErrMsgM Maps
mkMaps dflags gre instances exports decls = do
  maps <- mapM f decls
  let mergeMaps (a,b,c,d) (x,y,z,w) =
        (M.unionWith mappend a x, M.unionWith mappend b y,
         M.unionWith mappend c z, M.unionWith mappend d w)
  let emptyMaps = (M.empty, M.empty, M.empty, M.empty)
  return (foldl' mergeMaps emptyMaps maps)
  where
    instanceMap = M.fromList [ (getSrcSpan n, n) | i <- instances, let n = getName i ]

    f :: (LHsDecl Name, [HsDocString]) -> ErrMsgM Maps
    f (decl@(L _ d), docs) = do
      mayDoc <- lexParseRnHaddockCommentList dflags NormalHaddockComment gre docs
      argDocs <- fmap (M.mapMaybe id) $ Traversable.forM (typeDocs d) $
          \doc -> lexParseRnHaddockComment dflags NormalHaddockComment gre doc

      let subs_ = subordinates d
      let subs_' = filter (\(name, _, _) -> name `elem` exports) subs_

      (subDocs, subArgMap) <- unzip <$> (forM subs_' $ \(name, mbSubDocStr, subFnArgsDocStr) -> do
        mbSubDoc <- lexParseRnHaddockCommentList dflags NormalHaddockComment gre mbSubDocStr
        subFnArgsDoc <- fmap (M.mapMaybe id) $ Traversable.forM subFnArgsDocStr $
          \doc -> lexParseRnHaddockComment dflags NormalHaddockComment gre doc
        return ((name, mbSubDoc), (name, subFnArgsDoc)))

      let subNames = map fst subDocs

      let names = case d of
            InstD (InstDecl (L l _) _ _ _) -> maybeToList (M.lookup l instanceMap)  -- See note [2].
            _ -> filter (`elem` exports) (getMainDeclBinder d)

      let docMap' = M.fromList (mapMaybe (\(n,doc) -> fmap (n,) doc) ([ (n, mayDoc) | n <- names ] ++ subDocs))
      let argMap' = M.fromList [ (n, argDocs) | n <- names ] `mappend` M.fromList subArgMap
      let subMap' = M.fromList [ (n, subNames) | n <- names ]
      let dclMap' = M.fromList [ (n, [decl]) | n <- names ++ subNames ]
      return (docMap', argMap', subMap', dclMap')


-- Note [2]:
------------
-- We relate Instances to InstDecls using the SrcSpans buried inside them.
-- That should work for normal user-written instances (from looking at GHC
-- sources). We can assume that commented instances are user-written.
-- This lets us relate Names (from Instances) to comments (associated
-- with InstDecls).


subordinates :: HsDecl Name -> [(Name, [HsDocString], Map Int HsDocString)]
subordinates (TyClD decl)
  | isClassDecl decl = classSubs
  | isDataDecl  decl = dataSubs
  where
    classSubs = [ (name, doc, typeDocs d) | (L _ d, doc) <- classDecls decl
                , name <- getMainDeclBinder d, not (isValD d)
                ]
    dataSubs = constrs ++ fields
      where
        cons = map unL $ tcdCons decl
        constrs = [ (unL $ con_name c, maybeToList $ fmap unL $ con_doc c, M.empty)
                  | c <- cons ]
        fields  = [ (unL n, maybeToList $ fmap unL doc, M.empty)
                  | RecCon flds <- map con_details cons
                  , ConDeclField n _ doc <- flds ]
subordinates _ = []


-- | Extract function argument docs from inside types.
typeDocs :: HsDecl Name -> Map Int HsDocString
typeDocs d =
  let docs = go 0 in
  case d of
    SigD (TypeSig _ ty) -> docs (unLoc ty)
    ForD (ForeignImport _ ty _ _) -> docs (unLoc ty)
    TyClD (TySynonym {tcdSynRhs = ty}) -> docs (unLoc ty)
    _ -> M.empty
  where
    go n (HsForAllTy _ _ _ ty) = go n (unLoc ty)
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
    ats   = mkDecls tcdATs TyClD class_


-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup Name -> [(LHsDecl Name, [HsDocString])]
topDecls = filterClasses . filterDecls . collectDocs . sortByLoc . ungroup


-- | Take all declarations except pragmas, infix decls, rules from an 'HsGroup'.
ungroup :: HsGroup Name -> [LHsDecl Name]
ungroup group_ =
  mkDecls (concat   . hs_tyclds) TyClD  group_ ++
  mkDecls hs_derivds             DerivD group_ ++
  mkDecls hs_defds               DefD   group_ ++
  mkDecls hs_fords               ForD   group_ ++
  mkDecls hs_docs                DocD   group_ ++
  mkDecls hs_instds              InstD  group_ ++
  mkDecls (typesigs . hs_valds)  SigD   group_ ++
  mkDecls (valbinds . hs_valds)  ValD   group_
  where
    typesigs (ValBindsOut _ sigs) = filter isVanillaLSig sigs
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


warnAboutFilteredDecls :: Module -> [LHsDecl Name] -> ErrMsgM ()
warnAboutFilteredDecls mdl decls = do
  let modStr = moduleString mdl
  let typeInstances =
        nub [ tcdName d | L _ (TyClD d) <- decls, isFamInstDecl d ]

  unless (null typeInstances) $
    tell [
      "Warning: " ++ modStr ++ ": Instances of type and data "
      ++ "families are not yet supported. Instances of the following families "
      ++ "will be filtered out:\n  " ++ concat (intersperse ", "
      $ map (occNameString . nameOccName) typeInstances) ]

  let instances = nub [ pretty i | L _ (InstD (InstDecl i _ _ ats)) <- decls
                                 , not (null ats) ]

  unless (null instances) $
    tell [
      "Warning: " ++ modStr ++ ": We do not support associated types in instances yet. "
      ++ "These instances are affected:\n" ++ concat (intersperse ", " instances) ]


--------------------------------------------------------------------------------
-- Filtering of declarations
--
-- We filter out declarations that we don't intend to handle later.
--------------------------------------------------------------------------------


-- | Filter out declarations that we don't handle in Haddock
filterDecls :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterDecls decls = filter (isHandled . unL . fst) decls
  where
    isHandled (ForD (ForeignImport {})) = True
    isHandled (TyClD {}) = True
    isHandled (InstD {}) = True
    isHandled (SigD d) = isVanillaLSig (reL d)
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
      TyClD $ c { tcdSigs = filter isVanillaLSig $ tcdSigs c }
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
    go prev docs ((L _ (DocD (DocCommentNext str))):ds)
      | Nothing <- prev = go Nothing (str:docs) ds
      | Just decl <- prev = finished decl docs (go Nothing [str] ds)
    go prev docs ((L _ (DocD (DocCommentPrev str))):ds) = go prev (str:docs) ds
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
  :: IfaceMap
  -> Module             -- this module
  -> GlobalRdrEnv
  -> [Name]             -- exported names (orig)
  -> [LHsDecl Name]
  -> Maps
  -> Maybe [IE Name]
  -> [Instance]
  -> InstIfaceMap
  -> DynFlags
  -> ErrMsgGhc [ExportItem Name]
mkExportItems
  modMap thisMod gre exportedNames decls0
  (maps@(docMap, argMap, subMap, declMap)) optExports _ instIfaceMap dflags =
  case optExports of
    Nothing -> fullModuleContents dflags gre maps decls
    Just exports -> liftM (nubBy commaDeclared . concat) $ mapM lookupExport exports
  where
    decls = filter (not . isInstD . unLoc) decls0

    -- A type signature can have multiple names, like:
    --   foo, bar :: Types..
    -- When going throug the exported names we have to take care to detect such
    -- situations and remove the duplicates.
    commaDeclared (ExportDecl (L _ sig1) _ _ _) (ExportDecl (L _ sig2) _ _ _) =
      getMainDeclBinder sig1 == getMainDeclBinder sig2
    commaDeclared _ _ = False


    lookupExport (IEVar x)             = declWith x
    lookupExport (IEThingAbs t)        = declWith t
    lookupExport (IEThingAll t)        = declWith t
    lookupExport (IEThingWith t _)     = declWith t
    lookupExport (IEModuleContents m)  =
      moduleExports thisMod m dflags gre exportedNames decls modMap instIfaceMap maps
    lookupExport (IEGroup lev docStr)  = liftErrMsg $
      ifDoc (lexParseRnHaddockComment dflags DocSectionComment gre docStr)
            (\doc -> return [ ExportGroup lev "" doc ])
    lookupExport (IEDoc docStr)        = liftErrMsg $
      ifDoc (lexParseRnHaddockComment dflags NormalHaddockComment gre docStr)
            (\doc -> return [ ExportDoc doc ])
    lookupExport (IEDocNamed str)      = liftErrMsg $
      ifDoc (findNamedDoc str [ unL d | d <- decls ])
            (\docStr ->
            ifDoc (lexParseRnHaddockComment dflags NormalHaddockComment gre docStr)
                  (\doc -> return [ ExportDoc doc ]))


    ifDoc :: (Monad m) => m (Maybe a) -> (a -> m [b]) -> m [b]
    ifDoc parse finish = do
      mbDoc <- parse
      case mbDoc of Nothing -> return []; Just doc -> finish doc


    declWith :: Name -> ErrMsgGhc [ ExportItem Name ]
    declWith t =
      case findDecl t of
        ([L _ (ValD _)], (doc, _)) -> do
          -- Top-level binding without type signature
          export <- hiValExportItem t doc
          return [export]
        (ds, docs_) | decl : _ <- filter (not . isValD . unLoc) ds ->
          let declNames = getMainDeclBinder (unL decl)
          in case () of
            _
              -- temp hack: we filter out separately exported ATs, since we haven't decided how
              -- to handle them yet. We should really give an warning message also, and filter the
              -- name out in mkVisibleNames...
              | t `elem` declATs (unL decl)        -> return []

              -- We should not show a subordinate by itself if any of its
              -- parents is also exported. See note [1].
              | not $ t `elem` declNames,
                Just p <- find isExported (parents t $ unL decl) ->
                do liftErrMsg $ tell [
                     "Warning: " ++ moduleString thisMod ++ ": " ++
                     pretty (nameOccName t) ++ " is exported separately but " ++
                     "will be documented under " ++ pretty (nameOccName p) ++
                     ". Consider exporting it together with its parent(s)" ++
                     " for code clarity." ]
                   return []

              -- normal case
              | otherwise -> return [ mkExportDecl t newDecl docs_ ]
                  where
                    -- Since a single signature might refer to many names, we
                    -- need to filter the ones that are actually exported. This
                    -- requires modifying the type signatures to "hide" the
                    -- names that are not exported.
                    newDecl = case decl of
                      (L loc (SigD sig)) ->
                        L loc . SigD . fromJust $ filterSigNames isExported sig
                        -- fromJust is safe since we already checked in guards
                        -- that 't' is a name declared in this declaration.
                      _                  -> decl

        -- Declaration from another package
        ([], _) -> do
          mayDecl <- hiDecl t
          case mayDecl of
            Nothing -> return [ ExportNoDecl t [] ]
            Just decl -> do
              -- We try to get the subs and docs
              -- from the installed .haddock file for that package.
              case M.lookup (nameModule t) instIfaceMap of
                Nothing -> do
                   liftErrMsg $ tell
                      ["Warning: Couldn't find .haddock for export " ++ pretty t]
                   let subs_ = [ (n, noDocForDecl) | (n, _, _) <- subordinates (unLoc decl) ]
                   return [ mkExportDecl t decl (noDocForDecl, subs_) ]
                Just iface -> do
                   return [ mkExportDecl t decl (lookupDocs t (instDocMap iface) (instArgMap iface) (instSubMap iface)) ]

        _ -> return []


    mkExportDecl :: Name -> LHsDecl Name -> (DocForDecl Name, [(Name, DocForDecl Name)]) -> ExportItem Name
    mkExportDecl n decl (doc, subs) = decl'
      where
        decl' = ExportDecl (restrictTo sub_names (extractDecl n mdl decl)) doc subs' []
        mdl = nameModule n
        subs' = filter (isExported . fst) subs
        sub_names = map fst subs'


    isExported = (`elem` exportedNames)


    findDecl :: Name -> ([LHsDecl Name], (DocForDecl Name, [(Name, DocForDecl Name)]))
    findDecl n
      | m == thisMod, Just ds <- M.lookup n declMap =
          (ds, lookupDocs n docMap argMap subMap)
      | Just iface <- M.lookup m modMap, Just ds <- M.lookup n (ifaceDeclMap iface) =
          (ds, lookupDocs n (ifaceDocMap iface) (ifaceArgMap iface) (ifaceSubMap iface))
      | otherwise = ([], (noDocForDecl, []))
      where
        m = nameModule n


hiDecl :: Name -> ErrMsgGhc (Maybe (LHsDecl Name))
hiDecl t = do
  mayTyThing <- liftGhcToErrMsgGhc $ lookupName t
  case mayTyThing of
    Nothing -> do
      liftErrMsg $ tell ["Warning: Not found in environment: " ++ pretty t]
      return Nothing
    Just x -> return (Just (tyThingToLHsDecl x))


hiValExportItem :: Name -> DocForDecl Name -> ErrMsgGhc (ExportItem Name)
hiValExportItem name doc = do
  mayDecl <- hiDecl name
  case mayDecl of
    Nothing -> return (ExportNoDecl name [])
    Just decl -> return (ExportDecl decl doc [] [])


-- | Lookup docs for a declaration from maps.
lookupDocs :: Name -> DocMap Name -> ArgMap Name -> SubMap -> (DocForDecl Name, [(Name, DocForDecl Name)])
lookupDocs name docMap argMap subMap =
  let lookupArgMap x = maybe M.empty id (M.lookup x argMap) in
  let doc = (M.lookup name docMap, lookupArgMap name) in
  let subs = [ (sub, (M.lookup sub docMap, lookupArgMap sub)) | sub <- maybe [] id (M.lookup name subMap) ] in
  (doc, subs)


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
moduleExports :: Module           -- ^ Module A
              -> ModuleName       -- ^ The real name of B, the exported module
              -> DynFlags         -- ^ The flags used when typechecking A
              -> GlobalRdrEnv     -- ^ The renaming environment used for A
              -> [Name]           -- ^ All the exports of A
              -> [LHsDecl Name]   -- ^ All the declarations in A
              -> IfaceMap         -- ^ Already created interfaces
              -> InstIfaceMap     -- ^ Interfaces in other packages
              -> Maps
              -> ErrMsgGhc [ExportItem Name] -- ^ Resulting export items
moduleExports thisMod expMod dflags gre _exports decls ifaceMap instIfaceMap maps
  | m == thisMod = fullModuleContents dflags gre maps decls
  | otherwise =
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
              tell ["Warning: " ++ pretty thisMod ++ ": Could not find " ++
                    "documentation for exported module: " ++ pretty expMod]
            return []
  where
    m = mkModule packageId expMod
    packageId = modulePackageId thisMod


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


fullModuleContents :: DynFlags -> GlobalRdrEnv -> Maps -> [LHsDecl Name] -> ErrMsgGhc [ExportItem Name]
fullModuleContents dflags gre (docMap, argMap, subMap, declMap) decls =
  liftM catMaybes $ mapM mkExportItem decls
  where
    mkExportItem (L _ (DocD (DocGroup lev docStr))) = do
      mbDoc <- liftErrMsg $ lexParseRnHaddockComment dflags DocSectionComment gre docStr
      return $ fmap (ExportGroup lev "") mbDoc
    mkExportItem (L _ (DocD (DocCommentNamed _ docStr))) = do
      mbDoc <- liftErrMsg $ lexParseRnHaddockComment dflags NormalHaddockComment gre docStr
      return $ fmap ExportDoc mbDoc
    mkExportItem (L _ (ValD d))
      | name:_ <- collectHsBindBinders d, Just [L _ (ValD _)] <- M.lookup name declMap =
          -- Top-level binding without type signature.
          let (doc, _) = lookupDocs name docMap argMap subMap in
          fmap Just (hiValExportItem name doc)
      | otherwise = return Nothing
    mkExportItem decl
      | name:_ <- getMainDeclBinder (unLoc decl) =
        let (doc, subs) = lookupDocs name docMap argMap subMap in
        return $ Just (ExportDecl decl doc subs [])
      | otherwise = return Nothing


-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble
-- together a type signature for it...).
extractDecl :: Name -> Module -> LHsDecl Name -> LHsDecl Name
extractDecl name mdl decl
  | name `elem` getMainDeclBinder (unLoc decl) = decl
  | otherwise  =
    case unLoc decl of
      TyClD d | isClassDecl d ->
        let matches = [ sig | sig <- tcdSigs d, name `elem` sigName sig,
                        isVanillaLSig sig ] -- TODO: document fixity
        in case matches of
          [s0] -> let (n, tyvar_names) = name_and_tyvars d
                      L pos sig = extractClassDecl n tyvar_names s0
                  in L pos (SigD sig)
          _ -> error "internal: extractDecl"
      TyClD d | isDataDecl d ->
        let (n, tyvar_names) = name_and_tyvars d
            L pos sig = extractRecSel name mdl n tyvar_names (tcdCons d)
        in L pos (SigD sig)
      _ -> error "internal: extractDecl"
  where
    name_and_tyvars d = (unLoc (tcdLName d), hsLTyVarLocNames (tcdTyVars d))


toTypeNoLoc :: Located Name -> LHsType Name
toTypeNoLoc = noLoc . HsTyVar . unLoc


extractClassDecl :: Name -> [Located Name] -> LSig Name -> LSig Name
extractClassDecl c tvs0 (L pos (TypeSig lname ltype)) = case ltype of
  L _ (HsForAllTy expl tvs (L _ preds) ty) ->
    L pos (TypeSig lname (noLoc (HsForAllTy expl tvs (lctxt preds) ty)))
  _ -> L pos (TypeSig lname (noLoc (mkImplicitHsForAllTy (lctxt []) ltype)))
  where
    lctxt = noLoc . ctxt
    ctxt preds = nlHsTyConApp c (map toTypeNoLoc tvs0) : preds
extractClassDecl _ _ _ = error "extractClassDecl: unexpected decl"


extractRecSel :: Name -> Module -> Name -> [Located Name] -> [LConDecl Name]
              -> LSig Name
extractRecSel _ _ _ _ [] = error "extractRecSel: selector not found"

extractRecSel nm mdl t tvs (L _ con : rest) =
  case con_details con of
    RecCon fields | (ConDeclField n ty _ : _) <- matching_fields fields ->
      L (getLoc n) (TypeSig [noLoc nm] (noLoc (HsFunTy data_ty (getBangType ty))))
    _ -> extractRecSel nm mdl t tvs rest
 where
  matching_fields flds = [ f | f@(ConDeclField n _ _) <- flds, unLoc n == nm ]
  data_ty = foldl (\x y -> noLoc (HsAppTy x y)) (noLoc (HsTyVar t)) (map toTypeNoLoc tvs)


-- Pruning
pruneExportItems :: [ExportItem Name] -> [ExportItem Name]
pruneExportItems items = filter hasDoc items
  where
    hasDoc (ExportDecl{expItemMbDoc = (d, _)}) = isJust d
    hasDoc _ = True


mkVisibleNames :: [ExportItem Name] -> [DocOption] -> [Name]
mkVisibleNames exports opts
  | OptHide `elem` opts = []
  | otherwise = concatMap exportName exports
  where
    exportName e@ExportDecl {} = getMainDeclBinder (unL $ expItemDecl e) ++ subs
      where subs = map fst (expItemSubDocs e)
    exportName ExportNoDecl {} = [] -- we don't count these as visible, since
                                    -- we don't want links to go to them.
    exportName _ = []


-- | Find a stand-alone documentation comment by its name.
findNamedDoc :: String -> [HsDecl Name] -> ErrMsgM (Maybe HsDocString)
findNamedDoc name decls = search decls
  where
    search [] = do
      tell ["Cannot find documentation for: $" ++ name]
      return Nothing
    search ((DocD (DocCommentNamed name' doc)):rest)
      | name == name' = return (Just doc)
      | otherwise = search rest
    search (_other_decl : rest) = search rest
