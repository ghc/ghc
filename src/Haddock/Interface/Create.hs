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

import qualified Data.Map as Map
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
createInterface :: TypecheckedModule -> [Flag] -> IfaceMap -> InstIfaceMap
                -> ErrMsgGhc Interface
createInterface tm flags modMap instIfaceMap = do

  let ms                  = pm_mod_summary . tm_parsed_module $ tm
      mi                  = moduleInfo tm
      mdl                 = ms_mod ms
      dflags              = ms_hspp_opts ms
      instances           = modInfoInstances mi
      exportedNames       = modInfoExports mi
      -- XXX: confirm always a Just.
      Just (group_, _, optExports, optDocHeader) = renamedSource tm

  -- The pattern-match should not fail, because createInterface is only
  -- done on loaded modules.
  Just gre <- liftGhcToErrMsgGhc $ lookupLoadedHomeModuleGRE (moduleName mdl)

  opts0 <- liftErrMsg $ mkDocOpts (haddockOptions dflags) flags mdl
  let opts
        | Flag_IgnoreAllExports `elem` flags = OptIgnoreExports : opts0
        | otherwise = opts0

  (info, mbDoc)    <- liftErrMsg $ lexParseRnHaddockModHeader dflags gre optDocHeader

  let declsWithDocs = topDecls group_
      (decls, _) = unzip declsWithDocs
      localInsts = filter (nameIsLocalOrFrom mdl . getName) instances
  (docMap, argMap, subMap, declMap) <- liftErrMsg $ maps dflags gre localInsts exportedNames declsWithDocs

  let  exports0      = fmap (reverse . map unLoc) optExports
       exports
        | OptIgnoreExports `elem` opts = Nothing
        | otherwise = exports0

  liftErrMsg $ warnAboutFilteredDecls mdl decls

  exportItems <- mkExportItems modMap mdl gre exportedNames decls docMap argMap subMap declMap
                               exports instances instIfaceMap dflags

  let visibleNames = mkVisibleNames exportItems opts

  -- measure haddock documentation coverage.
  let
    prunedExportItems0 = pruneExportItems exportItems
    haddockable = 1 + length exportItems -- module + exports
    haddocked = (if isJust mbDoc then 1 else 0) + length prunedExportItems0
    coverage = (haddockable, haddocked)

  -- prune the export list to just those declarations that have
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
    ifaceRnDocMap        = Map.empty,
    ifaceRnArgMap        = Map.empty,
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


maps :: DynFlags -> GlobalRdrEnv -> [Instance] -> [Name] -> [(Decl, [HsDocString])] -> ErrMsgM Maps
maps dflags gre instances exports decls = do
  maps_ <- mapM f decls
  let mergeMaps (a,b,c,d) (x,y,z,w) =
        (M.unionWith mappend a x, M.unionWith mappend b y,
         M.unionWith mappend c z, M.unionWith mappend d w)
  let emptyMaps = (M.empty, M.empty, M.empty, M.empty)
  return (foldl' mergeMaps emptyMaps maps_)
  where
    instanceMap = M.fromList [ (getSrcSpan n, n) | i <- instances, let n = getName i ]

    f :: (Decl, [HsDocString]) -> ErrMsgM Maps
    f (decl@(L _ d), docs) = do
      mayDoc <- lexParseRnHaddockCommentList dflags NormalHaddockComment gre docs
      argDocs <- fmap (Map.mapMaybe id) $ Traversable.forM (typeDocs d) $
          \doc -> lexParseRnHaddockComment dflags NormalHaddockComment gre doc

      let subs_ = subordinates d
      let subs_' = filter (\(name, _, _) -> name `elem` exports) subs_

      (subDocs, subArgMap) <- unzip <$> (forM subs_' $ \(name, mbSubDocStr, subFnArgsDocStr) -> do
        mbSubDoc <- lexParseRnHaddockCommentList dflags NormalHaddockComment gre mbSubDocStr
        subFnArgsDoc <- fmap (Map.mapMaybe id) $ Traversable.forM subFnArgsDocStr $
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
        -- should we use the type-signature of the constructor
        -- and the docs of the fields to produce fnArgsDoc for the constr,
        -- just in case someone exports it without exporting the type
        -- and perhaps makes it look like a function?  I doubt it.
        constrs = [ (unL $ con_name c, maybeToList $ fmap unL $ con_doc c, Map.empty)
                  | c <- cons ]
        fields  = [ (unL n, maybeToList $ fmap unL doc, Map.empty)
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
    _ -> Map.empty
  where
    go n (HsForAllTy _ _ _ ty) = go n (unLoc ty)
    go n (HsFunTy (L _ (HsDocTy _ (L _ x))) (L _ ty)) = Map.insert n x $ go (n+1) ty
    go n (HsFunTy _ ty) = go (n+1) (unLoc ty)
    go n (HsDocTy _ (L _ doc)) = Map.singleton n doc
    go _ _ = Map.empty


-- | All the sub declarations of a class (that we handle), ordered by
-- source location, with documentation attached if it exists.
classDecls :: TyClDecl Name -> [(Decl, [HsDocString])]
classDecls class_ = filterDecls . collectDocs . sortByLoc $ decls
  where
    decls = docs ++ defs ++ sigs ++ ats
    docs  = mkDecls tcdDocs DocD class_
    defs  = mkDecls (bagToList . tcdMeths) ValD class_
    sigs  = mkDecls tcdSigs SigD class_
    ats   = mkDecls tcdATs TyClD class_


-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup Name -> [(Decl, [HsDocString])]
topDecls = filterClasses . filterDecls . collectDocs . sortByLoc . ungroup


-- | Take all declarations except pragmas, infix decls, rules from an 'HsGroup'.
ungroup :: HsGroup Name -> [Decl]
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
filterDecls :: [(Decl, doc)] -> [(Decl, doc)]
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
filterClasses :: [(Decl, doc)] -> [(Decl, doc)]
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


-- | Collect the docs and attach them to the right declaration.
collectDocs :: [Decl] -> [(Decl, [HsDocString])]
collectDocs = collect Nothing []

collect :: Maybe Decl -> [HsDocString] -> [Decl] -> [(Decl, [HsDocString])]
collect d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (e:es) =
  case e of
    L _ (DocD (DocCommentNext str)) ->
      case d of
        Nothing -> collect d (str:doc_so_far) es
        Just d0 -> finishedDoc d0 doc_so_far (collect Nothing [str] es)

    L _ (DocD (DocCommentPrev str)) -> collect d (str:doc_so_far) es

    _ -> case d of
      Nothing -> collect (Just e) doc_so_far es
      Just d0 -> finishedDoc d0 doc_so_far (collect (Just e) [] es)


-- This used to delete all DocD:s, unless doc was DocEmpty,
-- which I suppose means you could kill a DocCommentNamed
-- by:
--
-- > -- | killer
-- >
-- > -- $victim
--
-- Anyway I accidentally deleted the DocEmpty condition without
-- realizing it was necessary for retaining some DocDs (at least
-- DocCommentNamed), so I'm going to try just not testing any conditions
-- and see if anything breaks.  It really shouldn't break anything
-- to keep more doc decls around, IMHO.
--
-- -Isaac
finishedDoc :: Decl -> [HsDocString] -> [(Decl, [HsDocString])] -> [(Decl, [HsDocString])]
finishedDoc d docs rest = (d, reverse docs) : rest


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
  -> DocMap Name
  -> ArgMap Name
  -> SubMap
  -> DeclMap  -- maps local names to declarations
  -> Maybe [IE Name]
  -> [Instance]
  -> InstIfaceMap
  -> DynFlags
  -> ErrMsgGhc [ExportItem Name]
mkExportItems modMap thisMod gre exportedNames decls0 docMap argMap subMap declMap
              optExports _ instIfaceMap dflags =
  case optExports of
    Nothing      -> liftErrMsg $ fullContentsOfThisModule dflags gre docMap argMap subMap decls
    Just exports -> liftM (nubBy commaDeclared . concat) $ mapM lookupExport exports
  where
    decls = filter (\(L _ d) -> not (isInstD d || isValD d)) decls0

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
      moduleExports thisMod m dflags gre exportedNames decls modMap instIfaceMap docMap argMap subMap
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
      let doc = (Map.lookup t docMap, maybe Map.empty id (Map.lookup t argMap)) in
      case findDecl t of
        [L _ (ValD _)] -> do
          -- Top-level binding without type signature
          mayDecl <- ifaceDecl t
          case mayDecl of
            Nothing -> return [ ExportNoDecl t [] ]
            Just decl -> return [ ExportDecl decl doc [] [] ]

        ds | decl : _ <- filter (not . isValD . unLoc) ds ->
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
              | otherwise -> return [ mkExportDecl t newDecl (exportDecl t newDecl docMap argMap subMap) ]
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
        [] -> do
          mayDecl <- ifaceDecl t
          case mayDecl of
            Nothing -> return [ ExportNoDecl t [] ]
            Just decl -> do
              -- We try to get the subs and docs
              -- from the installed .haddock file for that package.
              case Map.lookup (nameModule t) instIfaceMap of
                Nothing -> do
                   liftErrMsg $ tell
                      ["Warning: Couldn't find .haddock for export " ++ pretty t]
                   let subs = [ (n, noDocForDecl) | (n, _, _) <- subordinates (unLoc decl) ]
                   return [ mkExportDecl t decl (noDocForDecl, subs) ]
                Just iface -> do
                   return [ mkExportDecl t decl (exportDecl t decl (instDocMap iface) (instArgMap iface) (instSubMap iface)) ]

        _ -> return []


    mkExportDecl :: Name -> Decl -> (DocForDecl Name, [(Name, DocForDecl Name)]) -> ExportItem Name
    mkExportDecl n decl (doc, subs) = decl'
      where
        decl' = ExportDecl (restrictTo sub_names (extractDecl n mdl decl)) doc subs' []
        mdl = nameModule n
        subs' = filter (isExported . fst) subs
        sub_names = map fst subs'


    isExported = (`elem` exportedNames)


    findDecl :: Name -> [Decl]
    findDecl n
      | m == thisMod = maybe [] id (Map.lookup n declMap)
      | otherwise = case Map.lookup m modMap of
                      Just iface -> maybe [] id (Map.lookup n (ifaceDeclMap iface))
                      Nothing -> []
      where
        m = nameModule n


ifaceDecl :: Name -> ErrMsgGhc (Maybe (LHsDecl Name))
ifaceDecl t = do
  mayTyThing <- liftGhcToErrMsgGhc $ lookupName t
  case mayTyThing of
    Nothing -> do
      liftErrMsg $ tell ["Warning: Not found in environment: " ++ pretty t]
      return Nothing
    Just x -> return (Just (tyThingToLHsDecl x))


exportDecl :: Name -> Decl -> DocMap Name -> ArgMap Name -> SubMap -> (DocForDecl Name, [(Name, DocForDecl Name)])
exportDecl name _ docMap argMap subMap =
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
              -> DynFlags         -- ^ The flag used when typechecking A
              -> GlobalRdrEnv     -- ^ The renaming environment used for A
              -> [Name]           -- ^ All the exports of A
              -> [Decl]       -- ^ All the declarations in A
              -> IfaceMap         -- ^ Already created interfaces
              -> InstIfaceMap     -- ^ Interfaces in other packages
              -> DocMap Name
              -> ArgMap Name
              -> SubMap
              -> ErrMsgGhc [ExportItem Name] -- ^ Resulting export items
moduleExports thisMod expMod dflags gre _exports decls ifaceMap instIfaceMap docMap argMap subMap
  | m == thisMod = liftErrMsg $ fullContentsOfThisModule dflags gre docMap argMap subMap decls
  | otherwise =
    case Map.lookup m ifaceMap of
      Just iface
        | OptHide `elem` ifaceOptions iface -> return (ifaceExportItems iface)
        | otherwise -> return [ ExportModule m ]

      Nothing -> -- we have to try to find it in the installed interfaces
                 -- (external packages)
        case Map.lookup expMod (Map.mapKeys moduleName instIfaceMap) of
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


fullContentsOfThisModule :: DynFlags -> GlobalRdrEnv -> DocMap Name -> ArgMap Name -> SubMap -> [Decl] -> ErrMsgM [ExportItem Name]
fullContentsOfThisModule dflags gre docMap argMap subMap decls = liftM catMaybes $ mapM mkExportItem decls
  where
    mkExportItem (L _ (DocD (DocGroup lev docStr))) = do
      mbDoc <- lexParseRnHaddockComment dflags DocSectionComment gre docStr
      return $ fmap (ExportGroup lev "") mbDoc
    mkExportItem (L _ (DocD (DocCommentNamed _ docStr))) = do
      mbDoc <- lexParseRnHaddockComment dflags NormalHaddockComment gre docStr
      return $ fmap ExportDoc mbDoc
    mkExportItem decl
      | name : _ <- getMainDeclBinder (unLoc decl) =
        let (doc, subs) = exportDecl name decl docMap argMap subMap in
        return $ Just (ExportDecl decl doc subs [])
      | otherwise = return Nothing

-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble
-- together a type signature for it...)
extractDecl :: Name -> Module -> Decl -> Decl
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


-- | Find a stand-alone documentation comment by its name
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
