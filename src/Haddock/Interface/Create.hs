--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Interface.Create (createInterface) where


import Haddock.Types
import Haddock.Options
import Haddock.GHC.Utils
import Haddock.Utils

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad
import Control.Arrow

import GHC
import Outputable
import SrcLoc
import Name
import Module
import InstEnv
import Class
import TypeRep
import Var hiding (varName)
import TyCon
import PrelNames
import Bag
import HscTypes


-- | Process the data in the GhcModule to produce an interface.
-- To do this, we need access to already processed modules in the topological
-- sort. That's what's in the module map.
createInterface :: GhcModule -> [Flag] -> ModuleMap -> InstIfaceMap
                -> ErrMsgM Interface
createInterface ghcMod flags modMap instIfaceMap = do

  let mod = ghcModule ghcMod

  opts0 <- mkDocOpts (ghcMbDocOpts ghcMod) flags mod
  let opts
        | Flag_IgnoreAllExports `elem` flags = OptIgnoreExports : opts0
        | otherwise = opts0

  let group         = ghcGroup ghcMod
      exports       = fmap (reverse . map unLoc) (ghcMbExports ghcMod)
      localNames    = ghcDefinedNames ghcMod
      decls0        = declInfos . topDecls $ group
      decls         = filterOutInstances decls0
      declMap       = mkDeclMap decls
      ignoreExps    = Flag_IgnoreAllExports `elem` flags
      exportedNames = ghcExportedNames ghcMod
      instances     = ghcInstances ghcMod

  warnAboutFilteredDecls mod decls0

  exportItems <- mkExportItems modMap mod (ghcExportedNames ghcMod) decls declMap
                               opts exports ignoreExps instances instIfaceMap

  let visibleNames = mkVisibleNames exportItems opts
  
  -- prune the export list to just those declarations that have
  -- documentation, if the 'prune' option is on.
  let 
    prunedExportItems
      | OptPrune `elem` opts = pruneExportItems exportItems
      | otherwise = exportItems
 
  return Interface {
    ifaceMod             = mod,
    ifaceOrigFilename    = ghcFilename ghcMod,
    ifaceInfo            = ghcHaddockModInfo ghcMod,
    ifaceDoc             = ghcMbDoc ghcMod,
    ifaceRnDoc           = Nothing,
    ifaceOptions         = opts,
    ifaceLocals          = localNames,
    ifaceRnDocMap        = Map.empty,
    ifaceExportItems     = prunedExportItems,
    ifaceRnExportItems   = [],
    ifaceExports         = exportedNames,
    ifaceVisibleExports  = visibleNames, 
    ifaceDeclMap         = declMap,
    ifaceSubMap          = mkSubMap declMap exportedNames,
    ifaceInstances       = ghcInstances ghcMod
  }


-------------------------------------------------------------------------------
-- Doc options
--
-- Haddock options that are embedded in the source file
-------------------------------------------------------------------------------


mkDocOpts :: Maybe String -> [Flag] -> Module -> ErrMsgM [DocOption]
mkDocOpts mbOpts flags mod = do
  opts <- case mbOpts of 
    Just opts -> case words $ replace ',' ' ' opts of
      [] -> tell ["No option supplied to DOC_OPTION/doc_option"] >> return []
      xs -> liftM catMaybes (mapM parseOption xs)
    Nothing -> return []
  if Flag_HideModule (moduleString mod) `elem` flags 
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

-- | Make a sub map from a declaration map. Make sure we only include exported
-- names.
mkSubMap :: Map Name DeclInfo -> [Name] -> Map Name [Name]
mkSubMap declMap exports =
  Map.filterWithKey (\k _ -> k `elem` exports) (Map.map filterSubs declMap)
  where
    filterSubs (_, _, subs) = [ sub  | (sub, _) <- subs, sub `elem` exports ]


-- Make a map from names to 'DeclInfo's. Exclude declarations that don't
-- have names (instances and stand-alone documentation comments). Include
-- subordinate names, but map them to their parent declarations. 
mkDeclMap :: [DeclInfo] -> Map Name DeclInfo
mkDeclMap decls = Map.fromList . concat $
  [ (declName d, (parent, doc, subs)) : subDecls
  | (parent@(L _ d), doc, subs) <- decls 
  , let subDecls = [ (n, (parent, doc', [])) | (n, doc') <- subs ]
  , not (isDocD d), not (isInstD d) ]


declInfos :: [(Decl, Maybe Doc)] -> [DeclInfo]
declInfos decls = [ (parent, doc, subordinates d)
                  | (parent@(L _ d), doc) <- decls]


subordinates (TyClD d) = classDataSubs d
subordinates _ = []


classDataSubs :: TyClDecl Name -> [(Name, Maybe Doc)]
classDataSubs decl
  | isClassDecl decl = classSubs
  | isDataDecl  decl = dataSubs
  | otherwise        = []
  where
    classSubs = [ (declName d, doc) | (L _ d, doc) <- classDecls decl ]
    dataSubs  = constrs ++ fields   
      where
        cons    = map unL $ tcdCons decl
        constrs = [ (unL $ con_name c, fmap unL $ con_doc c) | c <- cons ]
        fields  = [ (unL n, fmap unL doc)
                  | RecCon flds <- map con_details cons
                  , ConDeclField n _ doc <- flds ]


-- All the sub declarations of a class (that we handle), ordered by
-- source location, with documentation attached if it exists. 
classDecls = filterDecls . collectDocs . sortByLoc . declsFromClass


declsFromClass class_ = docs ++ defs ++ sigs ++ ats
  where 
    docs = decls tcdDocs DocD class_
    defs = decls (bagToList . tcdMeths) ValD class_
    sigs = decls tcdSigs SigD class_
    ats  = decls tcdATs TyClD class_


declName (TyClD d) = tcdName d
declName (ForD (ForeignImport n _ _)) = unLoc n
-- we have normal sigs only (since they are taken from ValBindsOut)
declName (SigD sig) = fromJust $ sigNameNoLoc sig


-- | The top-level declarations of a module that we care about, 
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup Name -> [(Decl, Maybe Doc)] 
topDecls = filterClasses . filterDecls . collectDocs . sortByLoc . declsFromGroup


filterOutInstances = filter (\(L _ d, _, _) -> not (isInstD d))


-- | Take all declarations except pragmas, infix decls, rules and value
-- bindings from an 'HsGroup'.
declsFromGroup :: HsGroup Name -> [Decl]
declsFromGroup group = 
  decls hs_tyclds  TyClD    group ++
  decls hs_derivds DerivD   group ++
  decls hs_defds   DefD     group ++
  decls hs_fords   ForD     group ++
  decls hs_docs    DocD     group ++
  decls hs_instds  InstD    group ++
  decls (typesigs . hs_valds) SigD group
  where
    typesigs (ValBindsOut _ sigs) = filter isVanillaLSig sigs


-- | Take a field of declarations from a data structure and create HsDecls
-- using the given constructor
decls field con struct = [ L loc (con decl) | L loc decl <- field struct ]


-- | Sort by source location
sortByLoc = sortBy (comparing getLoc)


warnAboutFilteredDecls mod decls = do
  let modStr = moduleString mod
  let typeInstances =
        nub [ tcdName d | (L _ (TyClD d), _, _) <- decls, isFamInstDecl d ]

  when (not $null typeInstances) $
    tell $ nub [
      "Warning: " ++ modStr ++ ": Instances of type and data "
      ++ "families are not yet supported. Instances of the following families "
      ++ "will be filtered out:\n  " ++ (concat $ intersperse ", "
      $ map (occNameString . nameOccName) typeInstances) ]

  let instances = nub [ pretty i | (L _ (InstD (InstDecl i _ _ ats)), _, _) <- decls
                                 , not (null ats) ]

  when (not $ null instances) $

    tell $ nub $ [
      "Warning: " ++ modStr ++ ": We do not support associated types in instances yet. "
      ++ "These instances are affected:\n" ++ (concat $ intersperse ", " instances) ]


--------------------------------------------------------------------------------
-- Filtering of declarations
--
-- We filter out declarations that we don't intend to handle later.
--------------------------------------------------------------------------------


-- | Filter out declarations that we don't handle in Haddock
filterDecls :: [(Decl, Maybe Doc)] -> [(Decl, Maybe Doc)]
filterDecls decls = filter (isHandled . unL . fst) decls
  where
    isHandled (ForD (ForeignImport {})) = True
    isHandled (TyClD {}) = True
    isHandled (InstD {}) = True
    isHandled (SigD d) = isVanillaLSig (reL d)
    -- we keep doc declarations to be able to get at named docs
    isHandled (DocD _) = True
    isHandled _ = False


-- | Go through all class declarations and filter their sub-declarations
filterClasses :: [(Decl, Maybe Doc)] -> [(Decl, Maybe Doc)]
filterClasses decls = [ if isClassD d then (L loc (filterClass d), doc) else x 
                      | x@(L loc d, doc) <- decls ]
  where
    filterClass (TyClD c) =
      TyClD $ c { tcdSigs = filter isVanillaLSig $ tcdSigs c }  


--------------------------------------------------------------------------------
-- Collect docs
--
-- To be able to attach the right Haddock comment to the right declaration,
-- we sort the declarations by their SrcLoc and "collect" the docs for each 
-- declaration.
--------------------------------------------------------------------------------


-- | Collect the docs and attach them to the right declaration.
collectDocs :: [Decl] -> [(Decl, (Maybe Doc))]
collectDocs decls = collect Nothing DocEmpty decls

collect :: Maybe Decl -> Doc -> [Decl] -> [(Decl, (Maybe Doc))]
collect d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (e:es) =
  case e of
    L _ (DocD (DocCommentNext str)) ->
      case d of
        Nothing -> collect d (docAppend doc_so_far str) es
        Just d0 -> finishedDoc d0 doc_so_far (collect Nothing str es)

    L _ (DocD (DocCommentPrev str)) -> collect d (docAppend doc_so_far str) es

    _ -> case d of
      Nothing -> collect (Just e) doc_so_far es
      Just d0 -> finishedDoc d0 doc_so_far (collect (Just e) DocEmpty es)


finishedDoc :: Decl -> Doc -> [(Decl, (Maybe Doc))] -> [(Decl, (Maybe Doc))]
finishedDoc d DocEmpty rest = (d, Nothing) : rest
finishedDoc d doc rest | notDocDecl d = (d, Just doc) : rest
  where
    notDocDecl (L _ (DocD _)) = False
    notDocDecl _              = True
finishedDoc _ _ rest = rest


{-
attachATs :: [IE Name] -> ([IE Name], [Name])
attachATs exports = 
  where
    ats =   <- export ]
-}


-- | Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.
--
-- We create the export items even if the module is hidden, since they
-- might be useful when creating the export items for other modules.
mkExportItems
  :: ModuleMap
  -> Module			-- this module
  -> [Name]			-- exported names (orig)
  -> [DeclInfo]
  -> Map Name DeclInfo             -- maps local names to declarations
  -> [DocOption]
  -> Maybe [IE Name]
  -> Bool				-- --ignore-all-exports flag
  -> [Instance]
  -> InstIfaceMap
  -> ErrMsgM [ExportItem Name]

mkExportItems modMap this_mod exported_names decls declMap
              opts maybe_exps ignore_all_exports instances instIfaceMap
  | isNothing maybe_exps || ignore_all_exports || OptIgnoreExports `elem` opts
    = everything_local_exported
  | Just specs <- maybe_exps = liftM concat $ mapM lookupExport specs
  where
    instances = [ d | d@(L _ decl, _, _) <- decls, isInstD decl ]

    everything_local_exported =  -- everything exported
      return (fullContentsOfThisModule this_mod decls)
   

    lookupExport (IEVar x) = declWith x
    lookupExport (IEThingAbs t) = declWith t
  --    | Just fam <- Map.lookup t famMap = absFam fam
  --    | otherwise = declWith t
 --     where
   --     absFam (Just (famDecl, doc), instances) = return $ [ ExportDecl famDecl doc [] ] ++ matchingInsts t
     --   absFam (Nothing, instances) =

    lookupExport (IEThingAll t)        = declWith t
    lookupExport (IEThingWith t cs)    = declWith t
    lookupExport (IEModuleContents m)  = fullContentsOf m
    lookupExport (IEGroup lev doc)     = return [ ExportGroup lev "" doc ]
    lookupExport (IEDoc doc)           = return [ ExportDoc doc ] 
    lookupExport (IEDocNamed str) = do
      r <- findNamedDoc str [ unL d | (d,_,_) <- decls ]
      case r of
        Nothing -> return []
        Just found -> return [ ExportDoc found ]

    declWith :: Name -> ErrMsgM [ ExportItem Name ]
    declWith t =
      case findDecl t of
        Just x@(decl,_,_) ->
          let declName =
                case getMainDeclBinder (unL decl) of
                  Just declName -> declName
                  Nothing -> error "declWith: should not happen"
          in case () of
            _
              -- temp hack: we filter out separately exported ATs, since we haven't decided how
              -- to handle them yet. We should really give an warning message also, and filter the
              -- name out in mkVisibleNames...
              | t `elem` declATs (unL decl)        -> return []

              -- We should not show a subordinate by itself if any of its
              -- parents is also exported. See note [1].
              | t /= declName,
                Just p <- find isExported (parents t $ unL decl) ->
                do tell [ 
                     "Warning: " ++ moduleString this_mod ++ ": " ++
                     pretty (nameOccName t) ++ " is exported separately but " ++
                     "will be documented under " ++ pretty (nameOccName p) ++
                     ". Consider exporting it together with its parent(s)" ++
                     " for code clarity." ]
                   return []

              -- normal case
              | otherwise                          -> return [ mkExportDecl t x ]
        Nothing ->
          -- If we can't find the declaration, it must belong to another package.
          -- We return just the name of the declaration and try to get the subs
          -- from the installed interface of that package.
          case Map.lookup (nameModule t) instIfaceMap of
            Nothing -> return [ ExportNoDecl t [] ]
            Just iface ->
              let subs = case Map.lookup t (instSubMap iface) of
                           Nothing -> []
                           Just x -> x
              in return [ ExportNoDecl t subs ]

    mkExportDecl :: Name -> DeclInfo -> ExportItem Name
    mkExportDecl n (decl, doc, subs) = decl'
      where
        decl' = ExportDecl (restrictTo sub_names (extractDecl n mdl decl)) doc subs' []
        mdl = nameModule n
        subs' = filter ((`elem` exported_names) . fst) subs
        sub_names = map fst subs'

    isExported n = n `elem` exported_names

    fullContentsOf modname
	| m == this_mod = return (fullContentsOfThisModule this_mod decls)
	| otherwise = 
	   case Map.lookup m modMap of
	     Just iface
		| OptHide `elem` ifaceOptions iface
			-> return (ifaceExportItems iface)
		| otherwise -> return [ ExportModule m ]
               
	     Nothing -> -- we have to try to find it in the installed interfaces
                        -- (external packages)
               case Map.lookup modname (Map.mapKeys moduleName instIfaceMap) of
                 Just iface -> return [ ExportModule (instMod iface) ]
                 Nothing -> do
                   tell ["Warning: " ++ pretty this_mod ++ ": Could not find " ++
                         "documentation for exported module: " ++ pretty modname]
                   return []
      where
        m = mkModule packageId modname
        packageId = modulePackageId this_mod

    
    findDecl :: Name -> Maybe DeclInfo
    findDecl n 
      | m == this_mod = Map.lookup n declMap
      | otherwise = case Map.lookup m modMap of
                      Just iface -> Map.lookup n (ifaceDeclMap iface) 
                      Nothing -> Nothing
      where
        m = nameModule n


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


fullContentsOfThisModule :: Module -> [DeclInfo] -> [ExportItem Name]
fullContentsOfThisModule module_ decls = catMaybes (map mkExportItem decls)
  where
    mkExportItem (L _ (DocD (DocGroup lev doc)), _, _) = Just $ ExportGroup lev "" doc
    mkExportItem (L _ (DocD (DocCommentNamed _ doc)), _, _)   = Just $ ExportDoc doc
    mkExportItem (decl, doc, subs) = Just $ ExportDecl decl doc subs []

--    mkExportItem _ = Nothing -- TODO: see if this is really needed


-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble 
-- together a type signature for it...)
extractDecl :: Name -> Module -> Decl -> Decl
extractDecl name mdl decl
  | Just n <- getMainDeclBinder (unLoc decl), n == name = decl
  | otherwise  =  
    case unLoc decl of
      TyClD d | isClassDecl d -> 
        let matches = [ sig | sig <- tcdSigs d, sigName sig == Just name,
                        isVanillaLSig sig ] -- TODO: document fixity
--        let assocMathes = [ tyDecl | at <- tcdATs d,  ] 
        in case matches of 
          [s0] -> let (n, tyvar_names) = name_and_tyvars d
                      L pos sig = extractClassDecl n mdl tyvar_names s0
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
toTypeNoLoc lname = noLoc (HsTyVar (unLoc lname))


rmLoc :: Located a -> Located a
rmLoc a = noLoc (unLoc a)


extractClassDecl :: Name -> Module -> [Located Name] -> LSig Name -> LSig Name
extractClassDecl c mdl tvs0 (L pos (TypeSig lname ltype)) = case ltype of
  L _ (HsForAllTy exp tvs (L _ preds) ty) -> 
    L pos (TypeSig lname (noLoc (HsForAllTy exp tvs (lctxt preds) ty)))
  _ -> L pos (TypeSig lname (noLoc (mkImplicitHsForAllTy (lctxt []) ltype)))
  where
    lctxt preds = noLoc (ctxt preds)
    ctxt preds = [noLoc (HsClassP c (map toTypeNoLoc tvs0))] ++ preds  

extractClassDecl _ _ _ d = error $ "extractClassDecl: unexpected decl"


extractRecSel :: Name -> Module -> Name -> [Located Name] -> [LConDecl Name]
              -> LSig Name
extractRecSel _ _ _ _ [] = error "extractRecSel: selector not found"

extractRecSel nm mdl t tvs (L _ con : rest) =
  case con_details con of
    RecCon fields | (ConDeclField n ty _ : _) <- matching_fields fields -> 
      L (getLoc n) (TypeSig (noLoc nm) (noLoc (HsFunTy data_ty (getBangType ty))))
    _ -> extractRecSel nm mdl t tvs rest
 where 
  matching_fields flds = [ f | f@(ConDeclField n _ _) <- flds, (unLoc n) == nm ]   
  data_ty = foldl (\x y -> noLoc (HsAppTy x y)) (noLoc (HsTyVar t)) (map toTypeNoLoc tvs)


-- Pruning
pruneExportItems :: [ExportItem Name] -> [ExportItem Name]
pruneExportItems items = filter hasDoc items
  where hasDoc (ExportDecl _ d _ _) = isJust d
	hasDoc _ = True


mkVisibleNames :: [ExportItem Name] -> [DocOption] -> [Name]
mkVisibleNames exports opts
  | OptHide `elem` opts = []
  | otherwise = concatMap exportName exports
  where
    exportName e@ExportDecl {} =
      case getMainDeclBinder $ unL $ expItemDecl e of
        Just n -> n : subs
        Nothing -> subs
      where subs = map fst (expItemSubDocs e) 
    exportName e@ExportNoDecl {} = [] -- we don't count these as visible, since
                                      -- we don't want links to go to them.
    exportName _ = []

      
exportModuleMissingErr this mdl 
  = ["Warning: in export list of " ++ show (moduleString this)
	 ++ ": module not found: " ++ show (moduleString mdl)]


-- | For a given entity, find all the names it "owns" (ie. all the
-- constructors and field names of a tycon, or all the methods of a
-- class).
allSubsOfName :: Map Module Interface -> Name -> [Name]
allSubsOfName ifaces name =
  case Map.lookup (nameModule name) ifaces of
    Just iface -> subsOfName name (ifaceDeclMap iface)
    Nothing -> []


subsOfName :: Name -> Map Name DeclInfo -> [Name]
subsOfName n declMap =
  case Map.lookup n declMap of
    Just (_, _, subs) -> map fst subs
    Nothing -> []


-- | Find a stand-alone documentation comment by its name
findNamedDoc :: String -> [HsDecl Name] -> ErrMsgM (Maybe Doc)
findNamedDoc name decls = search decls
  where
    search [] = do
      tell ["Cannot find documentation for: $" ++ name]
      return Nothing
    search ((DocD (DocCommentNamed name' doc)):rest) 
      | name == name' = return (Just doc)
      | otherwise = search rest
    search (_other_decl : rest) = search rest
