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
import Haddock.Interface.ExtractFnArgDocs

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Ord
import Control.Monad
import qualified Data.Traversable as Traversable

import GHC hiding (flags)
import Name
import Bag
import RdrName (GlobalRdrEnv)


-- | Process the data in the GhcModule to produce an interface.
-- To do this, we need access to already processed modules in the topological
-- sort. That's what's in the module map.
createInterface :: GhcModule -> [Flag] -> ModuleMap -> InstIfaceMap
                -> ErrMsgGhc Interface
createInterface ghcMod flags modMap instIfaceMap = do

  let mdl = ghcModule ghcMod

  -- The pattern-match should not fail, because createInterface is only
  -- done on loaded modules.
  Just gre <- liftGhcToErrMsgGhc $ lookupLoadedHomeModuleGRE (moduleName mdl)

  opts0 <- liftErrMsg $ mkDocOpts (ghcMbDocOpts ghcMod) flags mdl
  let opts
        | Flag_IgnoreAllExports `elem` flags = OptIgnoreExports : opts0
        | otherwise = opts0

  (info, mbDoc)    <- liftErrMsg $ lexParseRnHaddockModHeader
                                       gre (ghcMbDocHdr ghcMod)
  decls0           <- liftErrMsg $ declInfos gre (topDecls (ghcGroup ghcMod))
  let decls         = filterOutInstances decls0
      declMap       = mkDeclMap decls
      exports       = fmap (reverse . map unLoc) (ghcMbExports ghcMod)
      localNames    = ghcDefinedNames ghcMod
      ignoreExps    = Flag_IgnoreAllExports `elem` flags
      exportedNames = ghcExportedNames ghcMod
      instances     = ghcInstances ghcMod

  liftErrMsg $ warnAboutFilteredDecls mdl decls0

  exportItems <- mkExportItems modMap mdl gre (ghcExportedNames ghcMod) decls declMap
                               opts exports ignoreExps instances instIfaceMap

  let visibleNames = mkVisibleNames exportItems opts
  
  -- prune the export list to just those declarations that have
  -- documentation, if the 'prune' option is on.
  let 
    prunedExportItems
      | OptPrune `elem` opts = pruneExportItems exportItems
      | otherwise = exportItems
 
  return Interface {
    ifaceMod             = mdl,
    ifaceOrigFilename    = ghcFilename ghcMod,
    ifaceInfo            = info,
    ifaceDoc             = mbDoc,
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


declInfos :: GlobalRdrEnv -> [(Decl, MaybeDocStrings)] -> ErrMsgM [DeclInfo]
declInfos gre decls =
  forM decls $ \(parent@(L _ d), mbDocString) -> do
            mbDoc <- lexParseRnHaddockCommentList NormalHaddockComment
                       gre mbDocString
            fnArgsDoc <- fmap (Map.mapMaybe id) $
                Traversable.forM (getDeclFnArgDocs d) $
                \doc -> lexParseRnHaddockComment NormalHaddockComment gre doc

            let subs_ = subordinates d
            subs <- forM subs_ $ \(subName, mbSubDocStr, subFnArgsDocStr) -> do
                mbSubDoc <- lexParseRnHaddockCommentList NormalHaddockComment
                              gre mbSubDocStr
                subFnArgsDoc <- fmap (Map.mapMaybe id) $
                  Traversable.forM subFnArgsDocStr $
                  \doc -> lexParseRnHaddockComment NormalHaddockComment gre doc
                return (subName, (mbSubDoc, subFnArgsDoc))

            return (parent, (mbDoc, fnArgsDoc), subs)


-- | If you know the HsDecl can't contain any docs
-- (e.g., it was loaded from a .hi file and you don't have a .haddock file
-- to help you find out about the subs or docs)
-- then you can use this to get its subs.
subordinatesWithNoDocs :: HsDecl Name -> [(Name, DocForDecl Name)]
subordinatesWithNoDocs decl = map noDocs (subordinates decl)
  where
    -- check the condition... or shouldn't we be checking?
    noDocs (n, doc1, doc2) | null doc1, Map.null doc2
        = (n, noDocForDecl)
    noDocs _ = error ("no-docs thing has docs! " ++ pretty decl)


subordinates :: HsDecl Name -> [(Name, MaybeDocStrings, Map Int HsDocString)]
subordinates (TyClD d) = classDataSubs d
subordinates _ = []


classDataSubs :: TyClDecl Name -> [(Name, MaybeDocStrings, Map Int HsDocString)]
classDataSubs decl
  | isClassDecl decl = classSubs
  | isDataDecl  decl = dataSubs
  | otherwise        = []
  where
    classSubs = [ (declName d, doc, fnArgsDoc)
                | (L _ d, doc) <- classDecls decl
                , let fnArgsDoc = getDeclFnArgDocs d ]
    dataSubs  = constrs ++ fields   
      where
        cons    = map unL $ tcdCons decl
        -- should we use the type-signature of the constructor
        -- and the docs of the fields to produce fnArgsDoc for the constr,
        -- just in case someone exports it without exporting the type
        -- and perhaps makes it look like a function?  I doubt it.
        constrs = [ (unL $ con_name c, maybeToList $ fmap unL $ con_doc c, Map.empty)
                  | c <- cons ]
        fields  = [ (unL n, maybeToList $ fmap unL doc, Map.empty)
                  | RecCon flds <- map con_details cons
                  , ConDeclField n _ doc <- flds ]


-- All the sub declarations of a class (that we handle), ordered by
-- source location, with documentation attached if it exists. 
classDecls :: TyClDecl Name -> [(Decl, MaybeDocStrings)]
classDecls = filterDecls . collectDocs . sortByLoc . declsFromClass


declsFromClass :: TyClDecl a -> [Located (HsDecl a)]
declsFromClass class_ = docs ++ defs ++ sigs ++ ats
  where 
    docs = mkDecls tcdDocs DocD class_
    defs = mkDecls (bagToList . tcdMeths) ValD class_
    sigs = mkDecls tcdSigs SigD class_
    ats  = mkDecls tcdATs TyClD class_


declName :: HsDecl a -> a
declName (TyClD d) = tcdName d
declName (ForD (ForeignImport n _ _)) = unLoc n
-- we have normal sigs only (since they are taken from ValBindsOut)
declName (SigD sig) = fromJust $ sigNameNoLoc sig
declName _ = error "unexpected argument to declName"


-- | The top-level declarations of a module that we care about, 
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup Name -> [(Decl, MaybeDocStrings)] 
topDecls = filterClasses . filterDecls . collectDocs . sortByLoc . declsFromGroup


filterOutInstances :: [(Located (HsDecl a), b, c)] -> [(Located (HsDecl a), b, c)]
filterOutInstances = filter (\(L _ d, _, _) -> not (isInstD d))


-- | Take all declarations except pragmas, infix decls, rules and value
-- bindings from an 'HsGroup'.
declsFromGroup :: HsGroup Name -> [Decl]
declsFromGroup group_ = 
  mkDecls hs_tyclds  TyClD    group_ ++
  mkDecls hs_derivds DerivD   group_ ++
  mkDecls hs_defds   DefD     group_ ++
  mkDecls hs_fords   ForD     group_ ++
  mkDecls hs_docs    DocD     group_ ++
  mkDecls hs_instds  InstD    group_ ++
  mkDecls (typesigs . hs_valds) SigD group_
  where
    typesigs (ValBindsOut _ sigs) = filter isVanillaLSig sigs
    typesigs _ = error "expected ValBindsOut"


-- | Take a field of declarations from a data structure and create HsDecls
-- using the given constructor
mkDecls :: (a -> [Located b]) -> (b -> c) -> a -> [Located c]
mkDecls field con struct = [ L loc (con decl) | L loc decl <- field struct ]


-- | Sort by source location
sortByLoc :: [Located a] -> [Located a]
sortByLoc = sortBy (comparing getLoc)


warnAboutFilteredDecls :: Module -> [(LHsDecl Name, b, c)] -> ErrMsgM ()
warnAboutFilteredDecls mdl decls = do
  let modStr = moduleString mdl
  let typeInstances =
        nub [ tcdName d | (L _ (TyClD d), _, _) <- decls, isFamInstDecl d ]

  unless (null typeInstances) $
    tell $ nub [
      "Warning: " ++ modStr ++ ": Instances of type and data "
      ++ "families are not yet supported. Instances of the following families "
      ++ "will be filtered out:\n  " ++ (concat $ intersperse ", "
      $ map (occNameString . nameOccName) typeInstances) ]

  let instances = nub [ pretty i | (L _ (InstD (InstDecl i _ _ ats)), _, _) <- decls
                                 , not (null ats) ]

  unless (null instances) $
    tell $ nub [
      "Warning: " ++ modStr ++ ": We do not support associated types in instances yet. "
      ++ "These instances are affected:\n" ++ (concat $ intersperse ", " instances) ]


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

type MaybeDocStrings = [HsDocString]
-- avoid [] because we're appending from the left (quadratic),
-- and avoid adding another package dependency for haddock,
-- so use the difference-list pattern
type MaybeDocStringsFast = MaybeDocStrings -> MaybeDocStrings
docStringEmpty :: MaybeDocStringsFast
docStringEmpty = id
docStringSingleton :: HsDocString -> MaybeDocStringsFast
docStringSingleton = (:)
docStringAppend :: MaybeDocStringsFast -> MaybeDocStringsFast -> MaybeDocStringsFast
docStringAppend = (.)
docStringToList :: MaybeDocStringsFast -> MaybeDocStrings
docStringToList = ($ [])

-- | Collect the docs and attach them to the right declaration.
collectDocs :: [Decl] -> [(Decl, MaybeDocStrings)]
collectDocs = collect Nothing docStringEmpty

collect :: Maybe Decl -> MaybeDocStringsFast -> [Decl] -> [(Decl, MaybeDocStrings)]
collect d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (e:es) =
  case e of
    L _ (DocD (DocCommentNext str)) ->
      case d of
        Nothing -> collect d
                     (docStringAppend doc_so_far (docStringSingleton str))
                     es
        Just d0 -> finishedDoc d0 doc_so_far (collect Nothing
                     (docStringSingleton str)
                     es)

    L _ (DocD (DocCommentPrev str)) -> collect d
                     (docStringAppend doc_so_far (docStringSingleton str))
                     es

    _ -> case d of
      Nothing -> collect (Just e) doc_so_far es
      Just d0 -> finishedDoc d0 doc_so_far (collect (Just e) docStringEmpty es)


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
finishedDoc :: Decl -> MaybeDocStringsFast -> [(Decl, MaybeDocStrings)] -> [(Decl, MaybeDocStrings)]
finishedDoc d doc rest = (d, docStringToList doc) : rest


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
  -> GlobalRdrEnv
  -> [Name]			-- exported names (orig)
  -> [DeclInfo]
  -> Map Name DeclInfo             -- maps local names to declarations
  -> [DocOption]
  -> Maybe [IE Name]
  -> Bool				-- --ignore-all-exports flag
  -> [Instance]
  -> InstIfaceMap
  -> ErrMsgGhc [ExportItem Name]

mkExportItems modMap this_mod gre exported_names decls declMap
              opts maybe_exps ignore_all_exports _ instIfaceMap
  | isNothing maybe_exps || ignore_all_exports || OptIgnoreExports `elem` opts
    = everything_local_exported
  | otherwise = liftM concat $ mapM lookupExport (fromJust maybe_exps)
  where

-- creating export items for intsances (unfinished experiment)
--    instances = [ d | d@(L _ decl, _, _) <- decls, isInstD decl ]

    everything_local_exported =  -- everything exported
      liftErrMsg $ fullContentsOfThisModule gre decls
   

    lookupExport (IEVar x) = declWith x
    lookupExport (IEThingAbs t) = declWith t
  --    | Just fam <- Map.lookup t famMap = absFam fam
  --    | otherwise = declWith t
 --     where
   --     absFam (Just (famDecl, doc), instances) = return $ [ ExportDecl famDecl doc [] ] ++ matchingInsts t
     --   absFam (Nothing, instances) =

    lookupExport (IEThingAll t)        = declWith t
    lookupExport (IEThingWith t _)     = declWith t
    lookupExport (IEModuleContents m)  = fullContentsOf m
    lookupExport (IEGroup lev docStr)  = liftErrMsg $ do
      ifDoc (lexParseRnHaddockComment DocSectionComment gre docStr)
            (\doc -> return [ ExportGroup lev "" doc ])
    lookupExport (IEDoc docStr)        = liftErrMsg $ do
      ifDoc (lexParseRnHaddockComment NormalHaddockComment gre docStr)
            (\doc -> return [ ExportDoc doc ])
    lookupExport (IEDocNamed str) = liftErrMsg $ do
      ifDoc (findNamedDoc str [ unL d | (d,_,_) <- decls ])
            (\docStr ->
            ifDoc (lexParseRnHaddockComment NormalHaddockComment gre docStr)
                  (\doc -> return [ ExportDoc doc ]))

    ifDoc :: (Monad m) => m (Maybe a) -> (a -> m [b]) -> m [b]
    ifDoc parse finish = do
      mbDoc <- parse
      case mbDoc of Nothing -> return []; Just doc -> finish doc

    declWith :: Name -> ErrMsgGhc [ ExportItem Name ]
    declWith t =
      case findDecl t of
        Just x@(decl,_,_) ->
          let declName_ =
                case getMainDeclBinder (unL decl) of
                  Just n -> n
                  Nothing -> error "declWith: should not happen"
          in case () of
            _
              -- temp hack: we filter out separately exported ATs, since we haven't decided how
              -- to handle them yet. We should really give an warning message also, and filter the
              -- name out in mkVisibleNames...
              | t `elem` declATs (unL decl)        -> return []

              -- We should not show a subordinate by itself if any of its
              -- parents is also exported. See note [1].
              | t /= declName_,
                Just p <- find isExported (parents t $ unL decl) ->
                do liftErrMsg $ tell [
                     "Warning: " ++ moduleString this_mod ++ ": " ++
                     pretty (nameOccName t) ++ " is exported separately but " ++
                     "will be documented under " ++ pretty (nameOccName p) ++
                     ". Consider exporting it together with its parent(s)" ++
                     " for code clarity." ]
                   return []

              -- normal case
              | otherwise                          -> return [ mkExportDecl t x ]
        Nothing -> do
          -- If we can't find the declaration, it must belong to
          -- another package
          mbTyThing <- liftGhcToErrMsgGhc $ lookupName t
          -- show the name as exported as well as the name's
          -- defining module (because the latter is where we
          -- looked for the .hi/.haddock).  It's to help people
          -- debugging after all, so good to show more info.
          let exportInfoString =
                         moduleString this_mod ++ "." ++ getOccString t
                      ++ ": "
                      ++ pretty (nameModule t) ++ "." ++ getOccString t

          case mbTyThing of
            Nothing -> do
              liftErrMsg $ tell
                 ["Warning: Couldn't find TyThing for exported "
                 ++ exportInfoString ++ "; not documenting."]
              -- Is getting to here a bug in Haddock?
              -- Aren't the .hi files always present?
              return [ ExportNoDecl t [] ]
            Just tyThing -> do
              let hsdecl = tyThingToHsSynSig tyThing
              -- This is not the ideal way to implement haddockumentation
              -- for functions/values without explicit type signatures.
              --
              -- However I didn't find an easy way to implement it properly,
              -- and as long as we're using lookupName it is going to find
              -- the types of local inferenced binds.  If we don't check for
              -- this at all, then we'll get the "warning: couldn't find
              -- .haddock" which is wrong.
              --
              -- The reason this is not an ideal implementation
              -- (besides that we take a trip to desugared syntax and back
              -- unnecessarily)
              -- is that Haddock won't be able to detect doc-strings being
              -- attached to such a function, such as,
              --
              -- > -- | this is an identity function
              -- > id a = a
              --
              -- . It's more difficult to say what it ought to mean in cases
              -- where multiple exports are bound at once, like
              --
              -- > -- | comment...
              -- > (a, b) = ...
              --
              -- especially since in the export-list they might not even
              -- be next to each other.  But a proper implementation would
              -- really need to find the type of *all* exports as well as
              -- addressing all these issues.  This implementation works
              -- adequately.  Do you see a way to improve the situation?
              -- Please go ahead!  I got stuck trying to figure out how to
              -- get the 'PostTcType's that we want for all the bindings
              -- of an HsBind (you get 'LHsBinds' from 'GHC.typecheckedSource'
              -- for example).
              --
              -- But I might be missing something obvious.  What's important
              -- *here* is that we behave reasonably when we run into one of
              -- those exported type-inferenced values.
              isLocalAndTypeInferenced <- liftGhcToErrMsgGhc $
                    isLoaded (moduleName (nameModule t))
              if isLocalAndTypeInferenced
               then do
                   -- I don't think there can be any subs in this case,
                   -- currently?  But better not to rely on it.
                   let subs = subordinatesWithNoDocs (unLoc hsdecl)
                   return [ mkExportDecl t (hsdecl, noDocForDecl, subs) ]
               else
              -- We try to get the subs and docs
              -- from the installed interface of that package.
               case Map.lookup (nameModule t) instIfaceMap of
                -- It's Nothing in the cases where I thought
                -- Haddock has already warned the user: "Warning: The
                -- documentation for the following packages are not
                -- installed. No links will be generated to these packages:
                -- ..."
                -- But I guess it was Cabal creating that warning. Anyway,
                -- this is more serious than links: it's exported decls where
                -- we don't have the docs that they deserve!

                -- We could use 'subordinates' to find the Names of the subs
                -- (with no docs). Is that necessary? Yes it is, otherwise
                -- e.g. classes will be shown without their exported subs.
                Nothing -> do
                   liftErrMsg $ tell
                      ["Warning: Couldn't find .haddock for exported "
                      ++ exportInfoString]
                   let subs = subordinatesWithNoDocs (unLoc hsdecl)
                   return [ mkExportDecl t (hsdecl, noDocForDecl, subs) ]
                Just iface -> do
                   let subs = case Map.lookup t (instSubMap iface) of
                           Nothing -> []
                           Just x -> x
                   return [ mkExportDecl t
                     ( hsdecl
                     , fromMaybe noDocForDecl $
                          Map.lookup t (instDocMap iface)
                     , map (\subt ->
                              ( subt ,
                                fromMaybe noDocForDecl $
                                   Map.lookup subt (instDocMap iface)
                              )
                           ) subs
                     )]

    mkExportDecl :: Name -> DeclInfo -> ExportItem Name
    mkExportDecl n (decl, doc, subs) = decl'
      where
        decl' = ExportDecl (restrictTo sub_names (extractDecl n mdl decl)) doc subs' []
        mdl = nameModule n
        subs' = filter ((`elem` exported_names) . fst) subs
        sub_names = map fst subs'

    isExported n = n `elem` exported_names

    fullContentsOf modname
	| m == this_mod = liftErrMsg $ fullContentsOfThisModule gre decls
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
                   liftErrMsg $
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


fullContentsOfThisModule :: GlobalRdrEnv -> [DeclInfo] -> ErrMsgM [ExportItem Name]
fullContentsOfThisModule gre decls = liftM catMaybes $ mapM mkExportItem decls
  where
    mkExportItem (L _ (DocD (DocGroup lev docStr)), _, _) = do
        mbDoc <- lexParseRnHaddockComment DocSectionComment gre docStr
        return $ fmap (\doc -> ExportGroup lev "" doc) mbDoc
    mkExportItem (L _ (DocD (DocCommentNamed _ docStr)), _, _) = do
        mbDoc <- lexParseRnHaddockComment NormalHaddockComment gre docStr
        return $ fmap ExportDoc mbDoc
    mkExportItem (decl, doc, subs) = return $ Just $ ExportDecl decl doc subs []


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
    ctxt preds = noLoc (HsClassP c (map toTypeNoLoc tvs0)) : preds  
extractClassDecl _ _ _ = error "extractClassDecl: unexpected decl"


extractRecSel :: Name -> Module -> Name -> [Located Name] -> [LConDecl Name]
              -> LSig Name
extractRecSel _ _ _ _ [] = error "extractRecSel: selector not found"

extractRecSel nm mdl t tvs (L _ con : rest) =
  case con_details con of
    RecCon fields | (ConDeclField n ty _ : _) <- matching_fields fields -> 
      L (getLoc n) (TypeSig (noLoc nm) (noLoc (HsFunTy data_ty (getBangType ty))))
    _ -> extractRecSel nm mdl t tvs rest
 where 
  matching_fields flds = [ f | f@(ConDeclField n _ _) <- flds, unLoc n == nm ]   
  data_ty = foldl (\x y -> noLoc (HsAppTy x y)) (noLoc (HsTyVar t)) (map toTypeNoLoc tvs)


-- Pruning
pruneExportItems :: [ExportItem Name] -> [ExportItem Name]
pruneExportItems items = filter hasDoc items
  where hasDoc (ExportDecl{expItemMbDoc = (d, _)}) = isJust d
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
