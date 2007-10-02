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
import Control.Monad.Writer

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
import Util (handleDyn)
import ErrUtils (printBagOfErrors)
import FastString
#define FSLIT(x) (mkFastString# (x#))


-- | Process the data in the GhcModule to produce an interface.
-- To do this, we need access to already processed modules in the topological
-- sort. That's what's in the module map.
createInterface :: GhcModule -> [Flag] -> ModuleMap -> ErrMsgM Interface
createInterface ghcMod flags modMap = do

  let mod = ghcModule ghcMod

  opts <- mkDocOpts (ghcMbDocOpts ghcMod) flags mod

  let group        = ghcGroup ghcMod
      entities     = (nubBy sameName . getTopEntities) group
      exports      = fmap (reverse . map unLoc) (ghcMbExports ghcMod)
      entityNames_ = entityNames entities
      subNames     = allSubNames group
      localNames   = entityNames_ ++ subNames
      subMap       = mkSubMap group
      expDeclMap   = mkDeclMap (ghcExportedNames ghcMod) group
      localDeclMap = mkDeclMap entityNames_ group
      docMap       = mkDocMap group 
      ignoreExps   = Flag_IgnoreAllExports `elem` flags

  visibleNames <- mkVisibleNames mod modMap localNames 
                                 (ghcNamesInScope ghcMod) 
                                 subMap exports opts localDeclMap 

  exportItems <- mkExportItems modMap mod (ghcExportedNames ghcMod)
                               expDeclMap localDeclMap subMap entities 
                               opts exports ignoreExps docMap 

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
    ifaceDocMap          = docMap,
    ifaceRnDocMap        = Map.empty,
    ifaceSubMap          = subMap,
    ifaceExportItems     = prunedExportItems,
    ifaceRnExportItems   = [], 
    ifaceExports         = ghcExportedNames ghcMod,
    ifaceVisibleExports  = visibleNames, 
    ifaceExportedDeclMap = expDeclMap,
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
-- Source code entities
-- 
-- An entity is a Haskell declaration or a Haddock comment. We need to extract
-- entities out of classes and top levels since we need them in the interface. 
--------------------------------------------------------------------------------


data Entity = DocEntity (DocDecl Name) | DeclEntity Name
data LEntity = Located Entity

sameName (DocEntity _) _ = False
sameName (DeclEntity _) (DocEntity _) = False
sameName (DeclEntity a) (DeclEntity b) = a == b


sortByLoc = map unLoc . sortBy (comparing getLoc)


-- | Get all the entities in a class. The entities are sorted by their 
-- SrcLoc.
getClassEntities tcd = sortByLoc (docs ++ meths ++ sigs)
  where
    docs = [ L l (DocEntity d) | L l d <- tcdDocs tcd ]
    meths = 
      let bindings = bagToList (tcdMeths tcd)
          bindingName = unLoc . fun_id
      in [ L l (DeclEntity (bindingName b)) | L l b <- bindings ] 
    sigs = 
      let sigName = fromJust . sigNameNoLoc 
      in [ L l (DeclEntity (sigName sig)) | L l sig <- tcdSigs tcd ]  


-- | Get all the top level entities in a module. The entities are sorted by
-- their SrcLoc.
getTopEntities :: HsGroup Name -> [Entity]
getTopEntities group = sortByLoc (docs ++ declarations)
  where
    docs = [ L l (DocEntity d) | L l d <- hs_docs group ]

    declarations = [ L l (DeclEntity n) | (l, n) <- valds ++ tyclds ++ fords ]
      where
        valds = let ValBindsOut _ sigs = hs_valds group 
             -- we just use the sigs here for now.
             -- TODO: collect from the bindings as well 
             -- (needed for docs to work for inferred entities)
                in [ (l, fromJust (sigNameNoLoc s)) | L l s <- sigs ] 
        tyclds = [ (l, tcdName t) | L l t <- hs_tyclds group ]
        fords  = [ (l, forName f) | L l f <- hs_fords group ]  
          where
            forName (ForeignImport name _ _) = unLoc name
            forName (ForeignExport name _ _) = unLoc name


--------------------------------------------------------------------------------
-- Collect docs
--
-- To be able to attach the right Haddock comment to the right declaration,
-- we sort the entities by their SrcLoc and "collect" the docs for each 
-- declaration.
--------------------------------------------------------------------------------


-- | Collect the docs and attach them to the right name
collectDocs :: [Entity] -> [(Name, HsDoc Name)]
collectDocs entities = collect Nothing DocEmpty entities


collect :: Maybe Entity -> HsDoc Name -> [Entity] -> [(Name, HsDoc Name)]
collect d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (e:es) =
  case e of
    DocEntity (DocCommentNext str) ->
      case d of
        Nothing -> collect d (docAppend doc_so_far str) es
        Just d0 -> finishedDoc d0 doc_so_far (collect Nothing str es)

    DocEntity (DocCommentPrev str) -> collect d (docAppend doc_so_far str) es

    _ -> case d of
      Nothing -> collect (Just e) doc_so_far es
      Just d0
        | sameName d0 e -> collect d doc_so_far es  
        | otherwise -> finishedDoc d0 doc_so_far (collect (Just e) DocEmpty es)


finishedDoc :: Entity -> HsDoc Name -> [(Name, HsDoc Name)] -> 
               [(Name, HsDoc Name)]
finishedDoc d DocEmpty rest = rest
finishedDoc (DeclEntity name) doc rest = (name, doc) : rest
finishedDoc _ _ rest = rest


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------


-- This map includes everything that can be exported separately,
-- that means: top declarations, class methods and record selectors
-- TODO: merge this with mkDeclMap and the extractXXX functions 
mkDocMap :: HsGroup Name -> Map Name (HsDoc Name)
mkDocMap group = Map.fromList (topDeclDocs ++ classMethDocs ++ recordFieldDocs)
  where
    tyclds    = map unLoc (hs_tyclds group)
    classes   = filter isClassDecl tyclds 
    datadecls = filter isDataDecl tyclds
    constrs   = [ con | d <- datadecls, L _ con <- tcdCons d ]
    fields    = concat [ fields | RecCon fields <- map con_details constrs]

    topDeclDocs   = collectDocs (getTopEntities group)
    classMethDocs = concatMap (collectDocs . getClassEntities) classes

    recordFieldDocs = [ (unLoc lname, doc) | 
                        ConDeclField lname _ (Just (L _ doc)) <- fields ]

       
allSubNames :: HsGroup Name -> [Name]
allSubNames group = 
  concat [ tail (map unLoc (tyClDeclNames tycld)) | L _ tycld <- hs_tyclds group ]


mkSubMap :: HsGroup Name -> Map Name [Name]
mkSubMap group = Map.fromList [ (name, subs) | L _ tycld <- hs_tyclds group,
 let name:subs = map unLoc (tyClDeclNames tycld) ]


mkDeclMap :: [Name] -> HsGroup Name -> Map Name (LHsDecl Name) 
mkDeclMap names group = Map.fromList [ (n,d)  | (n,Just d) <- maybeDecls ]
  where 
  maybeDecls = [ (name, getDeclFromGroup group name) | name <- names ]


entityNames :: [Entity] -> [Name]
entityNames entities = [ name | DeclEntity name <- entities ] 
{-
getValSig :: Name -> HsValBinds Name -> TypeEnv -> Maybe (LSig Name)
getValSig name (ValBindsOut recsAndBinds _) typEnv = case matchingBinds of
  [bind] -> -- OK we have found a binding that matches. Now look up the
            -- type, even though it may be present in the ValBindsOut
            let tything = lookupTypeEnv typeEnv name       
  _ -> Nothing
  where 
    binds = snd $ unzip recsAndBinds 
    matchingBinds = Bag.filter matchesName binds
    matchesName (L _ bind) = fun_id bind == name
getValSig _ _ _ = error "getValSig"
-}


getDeclFromGroup :: HsGroup Name -> Name -> Maybe (LHsDecl Name)
getDeclFromGroup group name = 
  case catMaybes [ getDeclFromVals  (hs_valds  group), 
                   getDeclFromTyCls (hs_tyclds group),
                   getDeclFromFors  (hs_fords  group) ] of
    [decl] -> Just decl
    _ -> Nothing
  where 
    getDeclFromVals (ValBindsOut _ lsigs) = case matching of 
      [lsig] -> Just (L (getLoc lsig) (SigD (unLoc lsig)))
      _      -> Nothing
     where 
        matching = [ lsig | lsig <- lsigs, let Just n = sigName lsig, n == name, 
                     isNormal (unLoc lsig) ]
        isNormal (TypeSig _ _) = True
        isNormal _ = False

    getDeclFromVals _ = error "getDeclFromVals: illegal input"

{-    getDeclFromVals (ValBindsOut recsAndbinds _) = 
      let binds = snd $ unzip recsAndBinds 
          matchingBinds = Bag.filter matchesName binds
          matchesName (L _ bind) = fun_id bind == name
      in case matchingBinds of 
        [bind] -> -- OK we have found a binding that matches. Now look up the
                  -- type, even though it may be present in the ValBindsOut
                  
        _ -> Nothing
     where 
        matching = [ lsig | lsig <- lsigs, let Just n = sigName lsig, n == name ]
    getDeclFromVals _ = error "getDeclFromVals: illegal input"
  -}    
    getDeclFromTyCls ltycls = case matching of 
      [ltycl] -> Just (L (getLoc ltycl) (TyClD (unLoc ltycl)))
      _       -> Nothing
      where
        matching = [ ltycl | ltycl <- ltycls, 
                     name `elem` map unLoc (tyClDeclNames (unLoc ltycl))]
 
    getDeclFromFors lfors = case matching of 
      [for] -> Just (L (getLoc for) (ForD (unLoc for)))
      _      -> Nothing
      where
        matching = [ for | for <- lfors, forName (unLoc for) == name ]
        forName (ForeignExport n _ _) = unLoc n
        forName (ForeignImport n _ _) = unLoc n


-- | Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.
mkExportItems
  :: ModuleMap
  -> Module			-- this module
  -> [Name]			-- exported names (orig)
  -> Map Name (LHsDecl Name) -- maps exported names to declarations
  -> Map Name (LHsDecl Name) -- maps local names to declarations
  -> Map Name [Name]	-- sub-map for this module
  -> [Entity]	-- entities in the current module
  -> [DocOption]
  -> Maybe [IE Name]
  -> Bool				-- --ignore-all-exports flag
  -> Map Name (HsDoc Name)
  -> ErrMsgM [ExportItem Name]

mkExportItems modMap this_mod exported_names exportedDeclMap localDeclMap sub_map entities
              opts maybe_exps ignore_all_exports docMap
  | isNothing maybe_exps || ignore_all_exports || OptIgnoreExports `elem` opts
    = everything_local_exported
  | Just specs <- maybe_exps = do 
      exps <- mapM lookupExport specs
      return (concat exps)
  where
    everything_local_exported =  -- everything exported
      return (fullContentsOfThisModule this_mod entities localDeclMap docMap)
   
    packageId = modulePackageId this_mod

    lookupExport (IEVar x)             = declWith x
    lookupExport (IEThingAbs t)        = declWith t
    lookupExport (IEThingAll t)        = declWith t
    lookupExport (IEThingWith t cs)    = declWith t
    lookupExport (IEModuleContents m)  = fullContentsOf (mkModule packageId m)
    lookupExport (IEGroup lev doc)     = return [ ExportGroup lev "" doc ]
    lookupExport (IEDoc doc)           = return [ ExportDoc doc ] 
    lookupExport (IEDocNamed str)
	= do r <- findNamedDoc str entities
	     case r of
		Nothing -> return []
		Just found -> return [ ExportDoc found ]
 
    declWith :: Name -> ErrMsgM [ ExportItem Name ]
    declWith t
	| (Just decl, maybeDoc) <- findDecl t
        = return [ ExportDecl t (restrictTo subs (extractDecl t mdl decl)) maybeDoc [] ]
	| otherwise
	= return []
	where 
              mdl = nameModule t
	      subs = filter (`elem` exported_names) all_subs
              all_subs | mdl == this_mod = Map.findWithDefault [] t sub_map
		       | otherwise       = allSubsOfName modMap t

    fullContentsOf m  
	| m == this_mod = return (fullContentsOfThisModule this_mod entities localDeclMap docMap)
	| otherwise = 
	   case Map.lookup m modMap of
	     Just iface
		| OptHide `elem` ifaceOptions iface
			-> return (ifaceExportItems iface)
		| otherwise -> return [ ExportModule m ]
	     Nothing -> return [] -- already emitted a warning in visibleNames

    findDecl :: Name -> (Maybe (LHsDecl Name), Maybe (HsDoc Name))
    findDecl n | not (isExternalName n) = error "This shouldn't happen"
    findDecl n 
	| m == this_mod = (Map.lookup n exportedDeclMap, Map.lookup n docMap)
	| otherwise = 
	   case Map.lookup m modMap of
		Just iface -> (Map.lookup n (ifaceExportedDeclMap iface), 
                      Map.lookup n (ifaceDocMap iface))
		Nothing -> (Nothing, Nothing)
      where
        m = nameModule n


fullContentsOfThisModule :: Module -> [Entity] -> Map Name (LHsDecl Name) ->
                            Map Name (HsDoc Name) -> [ExportItem Name]
fullContentsOfThisModule module_ entities declMap docMap 
  = catMaybes (map mkExportItem entities)
  where 
    mkExportItem (DocEntity (DocGroup lev doc)) = Just (ExportGroup lev "" doc)
    mkExportItem (DeclEntity name) = fmap mkExport (Map.lookup name declMap) 
      where mkExport decl = ExportDecl name decl (Map.lookup name docMap) []
    mkExportItem _ = Nothing


-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble 
-- together a type signature for it...)
extractDecl :: Name -> Module -> LHsDecl Name -> LHsDecl Name
extractDecl name mdl decl
  | Just n <- getMainDeclBinder (unLoc decl), n == name = decl
  | otherwise  =  
    case unLoc decl of
      TyClD d | isClassDecl d -> 
        let matches = [ sig | sig <- tcdSigs d, sigName sig == Just name ] 
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
  where hasDoc (ExportDecl _ _ d _) = isJust d
	hasDoc _ = True


-- | Gather a list of original names exported from this module
mkVisibleNames :: Module 
             -> ModuleMap
             -> [Name] 
             -> [Name]
             -> Map Name [Name]
             -> Maybe [IE Name]
             -> [DocOption]
             -> Map Name (LHsDecl Name)
             -> ErrMsgM [Name]

mkVisibleNames mdl modMap localNames scope subMap maybeExps opts declMap 
  -- if no export list, just return all local names 
  | Nothing <- maybeExps         = return (filter hasDecl localNames)
  | OptIgnoreExports `elem` opts = return localNames
  | Just expspecs <- maybeExps = do
      visibleNames <- mapM extract expspecs
      return $ filter isNotPackageName (concat visibleNames)
 where
  hasDecl name = isJust (Map.lookup name declMap)
  isNotPackageName name = nameMod == mdl || isJust (Map.lookup nameMod modMap)
    where nameMod = nameModule name

  extract e = 
   case e of
    IEVar x -> return [x]
    IEThingAbs t -> return [t]
    IEThingAll t -> return (t : all_subs)
	 where
	      all_subs | nameModule t == mdl = Map.findWithDefault [] t subMap
		       | otherwise = allSubsOfName modMap t

    IEThingWith t cs -> return (t : cs)
	
    IEModuleContents m
	| mkModule (modulePackageId mdl) m == mdl -> return localNames 
	| otherwise -> let m' = mkModule (modulePackageId mdl) m in
	  case Map.lookup m' modMap of
	    Just mod
		| OptHide `elem` ifaceOptions mod ->
		    return (filter (`elem` scope) (ifaceExports mod))
		| otherwise -> return []
	    Nothing
		-> tell (exportModuleMissingErr mdl m') >> return []
  
    _ -> return []


exportModuleMissingErr this mdl 
  = ["Warning: in export list of " ++ show (moduleString this)
	 ++ ": module not found: " ++ show (moduleString mdl)]


-- | For a given entity, find all the names it "owns" (ie. all the
-- constructors and field names of a tycon, or all the methods of a
-- class).
allSubsOfName :: ModuleMap -> Name -> [Name]
allSubsOfName modMap name 
  | isExternalName name =
    case Map.lookup (nameModule name) modMap of
      Just iface -> Map.findWithDefault [] name (ifaceSubMap iface)
      Nothing   -> []
  | otherwise =  error $ "Main.allSubsOfName: unexpected unqual'd name"


-- Named documentation

findNamedDoc :: String -> [Entity] -> ErrMsgM (Maybe (HsDoc Name))
findNamedDoc name entities = search entities 
	where search [] = do
		tell ["Cannot find documentation for: $" ++ name]
		return Nothing
	      search ((DocEntity (DocCommentNamed name' doc)):rest) 
			| name == name' = return (Just doc)
		   	| otherwise = search rest
	      search (_other_decl : rest) = search rest
