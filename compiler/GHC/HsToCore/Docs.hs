-- | Extract docs from the renamer output so they can be serialized.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.HsToCore.Docs where

import GHC.Prelude
import GHC.Hs.Binds
import GHC.Hs.Doc
import GHC.Hs.Decls
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Hs.Type
import GHC.Hs.Utils
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.SrcLoc
import GHC.Tc.Types
import GHC.Parser.Annotation

import Control.Applicative
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map.Strict (Map)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe
import Data.Semigroup
import GHC.IORef (readIORef)
import GHC.Unit.Types
import GHC.Hs
import GHC.Types.Avail
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Unit.Module.Imported
import GHC.Driver.DynFlags
import GHC.Types.TypeEnv
import GHC.Types.Id
import GHC.Types.Unique.Map

-- | Extract docs from renamer output.
-- This is monadic since we need to be able to read documentation added from
-- Template Haskell's @putDoc@, which is stored in 'tcg_th_docs'.
extractDocs :: MonadIO m
            => DynFlags -> TcGblEnv
            -> m (Maybe Docs)
            -- ^
            -- 1. Module header
            -- 2. Docs on top level declarations
            -- 3. Docs on arguments
extractDocs dflags
      TcGblEnv { tcg_semantic_mod = semantic_mdl
               , tcg_mod = mdl
               , tcg_rn_decls = Just rn_decls
               , tcg_rn_exports = mb_rn_exports
               , tcg_exports = all_exports
               , tcg_imports = import_avails
               , tcg_insts = insts
               , tcg_fam_insts = fam_insts
               , tcg_hdr_info = mb_hdr_info
               , tcg_th_docs = th_docs_var
               , tcg_type_env = ty_env
               } = do
    th_docs <- liftIO $ readIORef th_docs_var
    let doc_hdr = unLoc <$> fst mb_hdr_info
        ExtractedTHDocs th_hdr th_decl_docs th_arg_docs th_inst_docs = extractTHDocs th_docs
        mod_docs
         =  Docs
         { docs_mod_hdr = th_hdr <|> doc_hdr
         , docs_exports = exports_docs
         -- Left biased union (see #21220)
         , docs_decls = plusUniqMap_C (\a _ -> a)
                          ((:[]) <$> th_decl_docs `plusUniqMap` th_inst_docs)
                          -- These will not clash so safe to use plusUniqMap
                          doc_map
         , docs_args = th_arg_docs `unionArgMaps` arg_map
         , docs_structure = doc_structure
         , docs_named_chunks = named_chunks
         , docs_haddock_opts = haddockOptions dflags
         , docs_language = language_
         , docs_extensions = exts
         }
    pure (Just mod_docs)
  where
    exts = extensionFlags dflags
    language_ = language dflags

    -- We need to lookup the Names for default methods, so we
    -- can put them in the correct map
    -- See Note [default method Name] in GHC.Iface.Recomp
    def_meths_env = mkOccEnv [(occ, nm)
                             | id <- typeEnvIds ty_env
                             , let nm = idName id
                                   occ = nameOccName nm
                             , isDefaultMethodOcc occ
                             ]

    exports_docs = maybe emptyUniqMap mkExportsDocs mb_rn_exports
    (doc_map, arg_map) = mkMaps def_meths_env local_insts decls_with_docs
    decls_with_docs = topDecls rn_decls
    local_insts = filter (nameIsLocalOrFrom semantic_mdl)
                         $ map getName insts ++ map getName fam_insts
    doc_structure = mkDocStructure mdl import_avails mb_rn_exports rn_decls
                                   all_exports def_meths_env
    named_chunks = getNamedChunks (isJust mb_rn_exports) rn_decls
extractDocs _ _ = pure Nothing

mkExportsDocs :: [(LIE GhcRn, Avails)] -> UniqMap Name (HsDoc GhcRn)
mkExportsDocs = foldMap f
  where
    f :: (LIE GhcRn, Avails) -> UniqMap Name (HsDoc GhcRn)
    f (L _ ie, avails)
      | Just (L _ doc) <- ieExportDoc ie =
        listToUniqMap [ (availName nm, doc) | nm <- avails ]
    f _ = emptyUniqMap

    ieExportDoc :: IE GhcRn -> Maybe (ExportDoc GhcRn)
    ieExportDoc (IEVar _ _ doc) = doc
    ieExportDoc (IEThingAbs _ _ doc) = doc
    ieExportDoc (IEThingAll _ _ doc) = doc
    ieExportDoc (IEThingWith _ _ _ _ doc) = doc
    ieExportDoc (IEModuleContents _ _) = Nothing
    ieExportDoc (IEGroup _ _ _) = Nothing
    ieExportDoc (IEDoc _ _) = Nothing
    ieExportDoc (IEDocNamed _ _) = Nothing

-- | If we have an explicit export list, we extract the documentation structure
-- from that.
-- Otherwise we use the renamed exports and declarations.
mkDocStructure :: Module                               -- ^ The current module
               -> ImportAvails                         -- ^ Imports
               -> Maybe [(LIE GhcRn, Avails)] -- ^ Explicit export list
               -> HsGroup GhcRn
               -> [AvailInfo]                          -- ^ All exports
               -> OccEnv Name                          -- ^ Default Methods
               -> DocStructure
mkDocStructure mdl import_avails (Just export_list) _ _ _ =
    mkDocStructureFromExportList mdl import_avails export_list
mkDocStructure _ _ Nothing rn_decls all_exports def_meths_env =
    mkDocStructureFromDecls def_meths_env all_exports rn_decls

-- TODO:
-- * Maybe remove items that export nothing?
-- * Combine sequences of DsiExports?
mkDocStructureFromExportList
  :: Module                         -- ^ The current module
  -> ImportAvails
  -> [(LIE GhcRn, Avails)] -- ^ Explicit export list
  -> DocStructure
mkDocStructureFromExportList mdl import_avails export_list =
    toDocStructure . first unLoc <$> export_list
  where
    toDocStructure :: (IE GhcRn, Avails) -> DocStructureItem
    toDocStructure = \case
      (IEModuleContents _ lmn, avails) -> moduleExport (unLoc lmn) avails
      (IEGroup _ level doc, _)         -> DsiSectionHeading level (unLoc doc)
      (IEDoc _ doc, _)                 -> DsiDocChunk (unLoc doc)
      (IEDocNamed _ name, _)           -> DsiNamedChunkRef name
      (_, avails)                      -> DsiExports (nubAvails avails)

    moduleExport :: ModuleName -- Alias
                 -> Avails
                 -> DocStructureItem
    moduleExport alias avails =
        DsiModExport (nubSortNE orig_names) (sortAvails (nubAvails avails))
      where
        orig_names = M.findWithDefault aliasErr alias aliasMap
        aliasErr = error $ "mkDocStructureFromExportList: "
                           ++ (moduleNameString . moduleName) mdl
                           ++ ": Can't find alias " ++ moduleNameString alias
        nubSortNE = NonEmpty.fromList .
                    Set.toList .
                    Set.fromList .
                    NonEmpty.toList

    -- Map from aliases to true module names.
    aliasMap :: Map ModuleName (NonEmpty ModuleName)
    aliasMap =
        M.fromListWith (<>) $
          (this_mdl_name, this_mdl_name :| [])
          : (flip concatMap (M.toList imported) $ \(mdl, imvs) ->
              [(imv_name imv, moduleName mdl :| []) | imv <- imvs])
      where
        this_mdl_name = moduleName mdl

    imported = M.map importedByUser (imp_mods import_avails)

-- | Figure out the documentation structure by correlating
-- the module exports with the located declarations.
mkDocStructureFromDecls :: OccEnv Name -- ^ The default method environment
                        -> [AvailInfo] -- ^ All exports, unordered
                        -> HsGroup GhcRn
                        -> DocStructure
mkDocStructureFromDecls env all_exports decls =
    map unLoc (sortLocated (docs ++ avails))
  where
    avails :: [Located DocStructureItem]
    avails = flip fmap all_exports $ \avail ->
      case M.lookup (availName avail) name_locs of
        Just loc -> L loc (DsiExports [avail])
        -- FIXME: This is just a workaround that we use when handling e.g.
        -- associated data families like in the html-test Instances.hs.
        Nothing -> noLoc (DsiExports [])

        -- This causes the associated data family to be incorrectly documented
        -- separately from its class:
        -- Nothing -> noLoc (DsiExports [avail])

        -- This panics on the associated data family:
        -- Nothing -> panicDoc "mkDocStructureFromDecls: No loc found for"
        --                     (ppr avail)

    docs = mapMaybe structuralDoc (hs_docs decls)

    structuralDoc :: LDocDecl GhcRn
                  -> Maybe (Located DocStructureItem)
    structuralDoc = \case
      L loc (DocCommentNamed _name doc) ->
        -- TODO: Is this correct?
        -- NB: There is no export list where we could reference the named chunk.
        Just (L (locA loc) (DsiDocChunk (unLoc doc)))

      L loc (DocGroup level doc) ->
        Just (L (locA loc) (DsiSectionHeading level (unLoc doc)))

      _ -> Nothing

    name_locs = M.fromList (concatMap ldeclNames (ungroup decls))
    ldeclNames (L loc d) = zip (getMainDeclBinder env d) (repeat (locA loc))

-- | Extract named documentation chunks from the renamed declarations.
--
-- If there is no explicit export list, we simply return an empty map
-- since there would be no way to link to a named chunk.
getNamedChunks :: Bool -- ^ Do we have an explicit export list?
               -> HsGroup (GhcPass pass)
               -> Map String (HsDoc (GhcPass pass))
getNamedChunks True decls =
  M.fromList $ flip mapMaybe (unLoc <$> hs_docs decls) $ \case
    DocCommentNamed name doc -> Just (name, unLoc doc)
    _                        -> Nothing
getNamedChunks False _ = M.empty

-- | Create decl and arg doc-maps by looping through the declarations.
-- For each declaration, find its names, its subordinates, and its doc strings.
mkMaps :: OccEnv Name
       -> [Name]
       -> [(LHsDecl GhcRn, [HsDoc GhcRn])]
       -> (UniqMap Name [HsDoc GhcRn], UniqMap Name (IntMap (HsDoc GhcRn)))
mkMaps env instances decls =
    ( listsToMapWith (++) (map (nubByName fst) decls')
    , listsToMapWith (<>) (filterMapping (not . IM.null) args)
    )
  where
    (decls', args) = unzip (map mappings decls)

    listsToMapWith f = listToUniqMap_C f . concat

    filterMapping :: (b -> Bool) ->  [[(a, b)]] -> [[(a, b)]]
    filterMapping p = map (filter (p . snd))

    mappings :: (LHsDecl GhcRn, [HsDoc GhcRn])
             -> ( [(Name, [HsDoc GhcRn])]
                , [(Name, IntMap (HsDoc GhcRn))]
                )
    mappings (L (EpAnn (EpaSpan (RealSrcSpan l _)) _ _) decl, doc) =
           (dm, am)
      where
        args = declTypeDocs decl

        subs :: [(Name, [HsDoc GhcRn], IntMap (HsDoc GhcRn))]
        subs = subordinates env instanceMap decl

        (subNs, subDocs, subArgs) =
          unzip3 subs

        ns = names l decl
        dm = [(n, d) | (n, d) <- zip ns (repeat doc) ++ zip subNs subDocs, not $ all (isEmptyDocString . hsDocString) d]
        am = [(n, args) | n <- ns] ++ zip subNs subArgs
    mappings (L (EpAnn _ _ _) _, _) = ([], [])

    instanceMap :: Map RealSrcSpan Name
    instanceMap = M.fromList [(l, n) | n <- instances, RealSrcSpan l _ <- [getSrcSpan n] ]

    names :: RealSrcSpan -> HsDecl GhcRn -> [Name]
    names _ (InstD _ d) = maybeToList $ lookupSrcSpan (getInstLoc d) instanceMap
    names l (DerivD {}) = maybeToList (M.lookup l instanceMap) -- See Note [1].
    names _ decl = getMainDeclBinder env decl

{-
Note [1]
~~~~~~~~
We relate ClsInsts to InstDecls and DerivDecls using the SrcSpans buried
inside them. That should work for normal user-written instances (from
looking at GHC sources). We can assume that commented instances are
user-written. This lets us relate Names (from ClsInsts) to comments
(associated with InstDecls and DerivDecls).
-}

getMainDeclBinder
  :: OccEnv Name -- ^ Default method environment for this module. See Note [default method Name] in GHC.Iface.Recomp
  -> HsDecl GhcRn -> [Name]
getMainDeclBinder _ (TyClD _ d) = [tcdName d]
getMainDeclBinder _ (ValD _ d) =
  case collectHsBindBinders CollNoDictBinders d of
    []       -> []
    (name:_) -> [name]
getMainDeclBinder env (SigD _ d) = sigNameNoLoc env d
getMainDeclBinder _   (ForD _ (ForeignImport _ name _ _)) = [unLoc name]
getMainDeclBinder _   (ForD _ (ForeignExport _ _ _ _)) = []
getMainDeclBinder _ _ = []


-- | The "OccEnv Name" is the default method environment for this module
-- Ultimately, the a special "defaultMethodOcc" name is used for
-- the signatures on bindings for default methods. Unfortunately, this
-- name isn't generated until typechecking, so it is not in the renamed AST.
-- We have to look it up from the 'OccEnv' parameter constructed from the typechecked
-- AST.
-- See also Note [default method Name] in GHC.Iface.Recomp
sigNameNoLoc :: forall a . (UnXRec a, HasOccName (IdP a)) => OccEnv (IdP a) -> Sig a -> [IdP a]
sigNameNoLoc _   (TypeSig    _   ns _)         = map (unXRec @a) ns
sigNameNoLoc _   (ClassOpSig _ False ns _)     = map (unXRec @a) ns
sigNameNoLoc env (ClassOpSig _ True  ns _)     = mapMaybe (lookupOccEnv env . mkDefaultMethodOcc . occName) $ map (unXRec @a) ns
sigNameNoLoc _   (PatSynSig  _   ns _)         = map (unXRec @a) ns
sigNameNoLoc _   (SpecSig    _   n _ _)        = [unXRec @a n]
sigNameNoLoc _   (InlineSig  _   n _)          = [unXRec @a n]
sigNameNoLoc _   (FixSig _ (FixitySig _ ns _)) = map (unXRec @a) ns
sigNameNoLoc _   _                             = []

-- Extract the source location where an instance is defined. This is used
-- to correlate InstDecls with their Instance/CoAxiom Names, via the
-- instanceMap.
getInstLoc :: Anno (IdGhcP p) ~ SrcSpanAnnN => InstDecl (GhcPass p) -> SrcSpan
getInstLoc = \case
  ClsInstD _ (ClsInstDecl { cid_poly_ty = ty }) -> getLocA ty
  -- The Names of data and type family instances have their SrcSpan's attached
  -- to the *type constructor*. For example, the Name "D:R:Foo:Int" would have
  -- its SrcSpan attached here:
  --   type family Foo a
  --   type instance Foo Int = Bool
  --                 ^^^
  DataFamInstD _ (DataFamInstDecl
    { dfid_eqn = FamEqn { feqn_tycon = L l _ }}) -> locA l
  -- Since CoAxioms' Names refer to the whole line for type family instances
  -- in particular, we need to dig a bit deeper to pull out the entire
  -- equation. This does not happen for data family instances, for some reason.
  TyFamInstD _ (TyFamInstDecl
    { tfid_eqn = FamEqn { feqn_tycon = L l _ }}) -> locA l

-- | Get all subordinate declarations inside a declaration, and their docs.
-- A subordinate declaration is something like the associate type or data
-- family of a type class.
subordinates :: OccEnv Name -- ^ The default method environment
             -> Map RealSrcSpan Name
             -> HsDecl GhcRn
             -> [(Name, [HsDoc GhcRn], IntMap (HsDoc GhcRn))]
subordinates env instMap decl = case decl of
  InstD _ (ClsInstD _ d) -> let
    data_fams = do
      DataFamInstDecl { dfid_eqn =
        FamEqn { feqn_tycon = L l _
               , feqn_rhs   = defn }} <- unLoc <$> cid_datafam_insts d
      [ (n, [], IM.empty) | Just n <- [lookupSrcSpan (locA l) instMap] ] ++ dataSubs defn
    ty_fams = do
      TyFamInstDecl { tfid_eqn = FamEqn { feqn_tycon = L l _ } } <- unLoc <$> cid_tyfam_insts d
      [ (n, [], IM.empty) | Just n <- [lookupSrcSpan (locA l) instMap] ]
    in data_fams ++ ty_fams

  InstD _ (DataFamInstD _ (DataFamInstDecl d))
    -> dataSubs (feqn_rhs d)
  TyClD _ d | isClassDecl d -> classSubs d
            | isDataDecl  d -> dataSubs (tcdDataDefn d)
  _ -> []
  where
    classSubs dd = [ (name, doc, declTypeDocs d)
                   | (L _ d, doc) <- classDecls dd
                   , name <- getMainDeclBinder env d, not (isValD d)
                   ]
    dataSubs :: HsDataDefn GhcRn
             -> [(Name, [HsDoc GhcRn], IntMap (HsDoc GhcRn))]
    dataSubs dd = constrs ++ fields  ++ derivs
      where
        cons = unLoc <$> dd_cons dd
        constrs = [ ( unLoc cname
                    , maybeToList $ fmap unLoc $ con_doc c
                    , conArgDocs c)
                  | c <- toList cons, cname <- getConNames c ]
        fields  = [ (foExt n, maybeToList $ fmap unLoc doc, IM.empty)
                  | Just flds <- toList $ fmap getRecConArgs_maybe cons
                  , (L _ (ConDeclField _ ns _ doc)) <- (unLoc flds)
                  , (L _ n) <- ns ]
        derivs  = [ (instName, [unLoc doc], IM.empty)
                  | (l, doc) <- concatMap (extract_deriv_clause_tys .
                                           deriv_clause_tys . unLoc) $
                                -- unLoc $ dd_derivs dd
                                dd_derivs dd
                  , Just instName <- [lookupSrcSpan l instMap] ]

        extract_deriv_clause_tys :: LDerivClauseTys GhcRn -> [(SrcSpan, LHsDoc GhcRn)]
        extract_deriv_clause_tys (L _ dct) =
          case dct of
            DctSingle _ ty -> maybeToList $ extract_deriv_ty ty
            DctMulti _ tys -> mapMaybe extract_deriv_ty tys

        extract_deriv_ty :: LHsSigType GhcRn -> Maybe (SrcSpan, LHsDoc GhcRn)
        extract_deriv_ty (L l (HsSig{sig_body = L _ ty})) =
          case ty of
            -- deriving (C a {- ^ Doc comment -})
            HsDocTy _ _ doc -> Just (locA l, doc)
            _               -> Nothing

-- | Extract constructor argument docs from inside constructor decls.
conArgDocs :: ConDecl GhcRn -> IntMap (HsDoc GhcRn)
conArgDocs (ConDeclH98{con_args = args}) =
  h98ConArgDocs args
conArgDocs (ConDeclGADT{con_g_args = args, con_res_ty = res_ty}) =
  gadtConArgDocs args (unLoc res_ty)

h98ConArgDocs :: HsConDeclH98Details GhcRn -> IntMap (HsDoc GhcRn)
h98ConArgDocs con_args = case con_args of
  PrefixCon _ args   -> con_arg_docs 0 $ map (unLoc . hsScaledThing) args
  InfixCon arg1 arg2 -> con_arg_docs 0 [ unLoc (hsScaledThing arg1)
                                       , unLoc (hsScaledThing arg2) ]
  RecCon _           -> IM.empty

gadtConArgDocs :: HsConDeclGADTDetails GhcRn -> HsType GhcRn -> IntMap (HsDoc GhcRn)
gadtConArgDocs con_args res_ty = case con_args of
  PrefixConGADT _ args -> con_arg_docs 0 $ map (unLoc . hsScaledThing) args ++ [res_ty]
  RecConGADT _ _       -> con_arg_docs 1 [res_ty]

con_arg_docs :: Int -> [HsType GhcRn] -> IntMap (HsDoc GhcRn)
con_arg_docs n = IM.fromList . catMaybes . zipWith f [n..]
  where
    f n (HsDocTy _ _ lds) = Just (n, unLoc lds)
    f n (HsBangTy _ _ (L _ (HsDocTy _ _ lds))) = Just (n, unLoc lds)
    f _ _ = Nothing

isValD :: HsDecl a -> Bool
isValD (ValD _ _) = True
isValD _ = False

-- | All the sub declarations of a class (that we handle), ordered by
-- source location, with documentation attached if it exists.
classDecls :: TyClDecl GhcRn -> [(LHsDecl GhcRn, [HsDoc GhcRn])]
classDecls class_ = filterDecls . collectDocs . sortLocatedA $ decls
  where
    decls = docs ++ defs ++ sigs ++ ats
    docs  = mkDecls tcdDocs (DocD noExtField) class_
    defs  = mkDecls tcdMeths (ValD noExtField) class_
    sigs  = mkDecls tcdSigs (SigD noExtField) class_
    ats   = mkDecls tcdATs (TyClD noExtField . FamDecl noExtField) class_

-- | Extract function argument docs from inside top-level decls.
declTypeDocs :: HsDecl GhcRn -> IntMap (HsDoc GhcRn)
declTypeDocs = \case
  SigD  _ (TypeSig _ _ ty)          -> sigTypeDocs (unLoc (dropWildCards ty))
  SigD  _ (ClassOpSig _ _ _ ty)     -> sigTypeDocs (unLoc ty)
  SigD  _ (PatSynSig _ _ ty)        -> sigTypeDocs (unLoc ty)
  ForD  _ (ForeignImport _ _ ty _)  -> sigTypeDocs (unLoc ty)
  TyClD _ (SynDecl { tcdRhs = ty }) -> typeDocs (unLoc ty)
  _                                 -> IM.empty

nubByName :: (a -> Name) -> [a] -> [a]
nubByName f ns = go emptyNameSet ns
  where
    go _ [] = []
    go s (x:xs)
      | y `elemNameSet` s = go s xs
      | otherwise         = let !s' = extendNameSet s y
                            in x : go s' xs
      where
        y = f x

-- | Extract function argument docs from inside types.
typeDocs :: HsType GhcRn -> IntMap (HsDoc GhcRn)
typeDocs = go 0
  where
    go n = \case
      HsForAllTy { hst_body = ty }          -> go n (unLoc ty)
      HsQualTy   { hst_body = ty }          -> go n (unLoc ty)
      HsFunTy _ _ (unLoc->HsDocTy _ _ x) ty -> IM.insert n (unLoc x) $ go (n+1) (unLoc ty)
      HsFunTy _ _ _ ty                      -> go (n+1) (unLoc ty)
      HsDocTy _ _ doc                       -> IM.singleton n (unLoc doc)
      _                                     -> IM.empty

-- | Extract function argument docs from inside types.
sigTypeDocs :: HsSigType GhcRn -> IntMap (HsDoc GhcRn)
sigTypeDocs (HsSig{sig_body = body}) = typeDocs (unLoc body)

-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup GhcRn -> [(LHsDecl GhcRn, [HsDoc GhcRn])]
topDecls = filterClasses . filterDecls . collectDocs . sortLocatedA . ungroup

-- | Take all declarations except pragmas, infix decls, rules from an 'HsGroup'.
ungroup :: HsGroup GhcRn -> [LHsDecl GhcRn]
ungroup group_ =
  mkDecls (tyClGroupTyClDecls . hs_tyclds) (TyClD noExtField)  group_ ++
  mkDecls hs_derivds             (DerivD noExtField) group_ ++
  mkDecls hs_defds               (DefD noExtField)   group_ ++
  mkDecls hs_fords               (ForD noExtField)   group_ ++
  mkDecls hs_docs                (DocD noExtField)   group_ ++
  mkDecls (tyClGroupInstDecls . hs_tyclds) (InstD noExtField)  group_ ++
  mkDecls (typesigs . hs_valds)  (SigD noExtField)   group_ ++
  mkDecls (valbinds . hs_valds)  (ValD noExtField)   group_
  where
    typesigs :: HsValBinds GhcRn -> [LSig GhcRn]
    typesigs (XValBindsLR (NValBinds _ sig)) = filter (isUserSig . unLoc) sig
    typesigs ValBinds{} = error "expected XValBindsLR"

    valbinds :: HsValBinds GhcRn -> [LHsBind GhcRn]
    valbinds (XValBindsLR (NValBinds binds _)) =
      concat . snd . unzip $ binds
    valbinds ValBinds{} = error "expected XValBindsLR"

-- | Collect docs and attach them to the right declarations.
--
-- A declaration may have multiple doc strings attached to it.
collectDocs :: forall p. UnXRec p => [LHsDecl p] -> [(LHsDecl p, [HsDoc p])]
-- ^ This is an example.
collectDocs = go [] Nothing
  where
    go docs mprev decls = case (decls, mprev) of
      ((unXRec @p -> DocD _ (DocCommentNext s)) : ds, Nothing)   -> go (unLoc s:docs) Nothing ds
      ((unXRec @p -> DocD _ (DocCommentNext s)) : ds, Just prev) -> finished prev docs $ go [unLoc s] Nothing ds
      ((unXRec @p -> DocD _ (DocCommentPrev s)) : ds, mprev)     -> go (unLoc s:docs) mprev ds
      (d                                  : ds, Nothing)   -> go docs (Just d) ds
      (d                                  : ds, Just prev) -> finished prev docs $ go [] (Just d) ds
      ([]                                     , Nothing)   -> []
      ([]                                     , Just prev) -> finished prev docs []

    finished decl docs rest = (decl, reverse docs) : rest

-- | Filter out declarations that we don't handle in Haddock
filterDecls :: forall p doc. UnXRec p => [(LHsDecl p, doc)] -> [(LHsDecl p, doc)]
filterDecls = filter (isHandled . unXRec @p . fst)
  where
    isHandled (ForD _ (ForeignImport {})) = True
    isHandled (TyClD {})  = True
    isHandled (InstD {})  = True
    isHandled (DerivD {}) = True
    isHandled (SigD _ d)  = isUserSig d
    isHandled (ValD {})   = True
    -- we keep doc declarations to be able to get at named docs
    isHandled (DocD {})   = True
    isHandled _ = False


-- | Go through all class declarations and filter their sub-declarations
filterClasses :: forall p doc. (IsPass p) => [(LHsDecl (GhcPass p), doc)] -> [(LHsDecl (GhcPass p), doc)]
filterClasses = map (first (fmap filterClass))
  where
    filterClass (TyClD x c@(ClassDecl {})) =
      TyClD x $ c { tcdSigs =
        filter (liftA2 (||) (isUserSig . unLoc) isMinimalLSig) (tcdSigs c) }
    filterClass d = d

-- | Was this signature given by the user?
isUserSig :: Sig name -> Bool
isUserSig TypeSig {}    = True
isUserSig ClassOpSig {} = True
isUserSig PatSynSig {}  = True
isUserSig _             = False

-- | Take a field of declarations from a data structure and create HsDecls
-- using the given constructor
mkDecls :: (struct -> [GenLocated l decl])
        -> (decl -> hsDecl)
        -> struct
        -> [GenLocated l hsDecl]
mkDecls field con = map (fmap con) . field

-- | Extracts out individual maps of documentation added via Template Haskell's
-- @putDoc@.
extractTHDocs :: THDocs
              -> ExtractedTHDocs
extractTHDocs docs =
  -- Split up docs into separate maps for each 'DocLoc' type
  ExtractedTHDocs
    { ethd_mod_header = docHeader
    , ethd_decl_docs  = searchDocs decl
    , ethd_arg_docs   = searchDocs args
    , ethd_inst_docs  = searchDocs insts
    }
  where
    docHeader :: Maybe (HsDoc GhcRn)
    docHeader
      | ((_, s):_) <- filter isModDoc (M.toList docs) = Just s
      | otherwise = Nothing

    isModDoc (ModuleDoc, _) = True
    isModDoc _ = False

    -- Folds over the docs, applying 'f' as the accumulating function.
    -- We use different accumulating functions to sift out the specific types of
    -- documentation
    searchDocs :: (UniqMap Name a -> (DocLoc, HsDoc GhcRn) -> UniqMap Name a) -> UniqMap Name a
    searchDocs f = foldl' f emptyUniqMap $ M.toList docs

    -- Pick out the declaration docs
    decl acc ((DeclDoc name), s) = addToUniqMap acc name s
    decl acc _ = acc

    -- Pick out the instance docs
    insts acc ((InstDoc name), s) = addToUniqMap acc name s
    insts acc _ = acc

    -- Pick out the argument docs
    args :: UniqMap Name (IntMap (HsDoc GhcRn))
         -> (DocLoc, HsDoc GhcRn)
         -> UniqMap Name (IntMap (HsDoc GhcRn))
    args acc ((ArgDoc name i), s) =
      -- Insert the doc for the arg into the argument map for the function. This
      -- means we have to search to see if an map already exists for the
      -- function, and insert the new argument if it exists, or create a new map
       addToUniqMap_C (\_ m -> IM.insert i s m) acc name (IM.singleton i s)
    args acc _ = acc

-- | Unions together two 'ArgDocMaps' (or ArgMaps in haddock-api), such that two
-- maps with values for the same key merge the inner map as well.
-- Left biased so @unionArgMaps a b@ prefers @a@ over @b@.

unionArgMaps :: forall b . UniqMap Name (IntMap b)
             -> UniqMap Name (IntMap b)
             -> UniqMap Name (IntMap b)
unionArgMaps a b = nonDetFoldUniqMap go b a
  where
    go :: (Name, IntMap b)
            -> UniqMap Name (IntMap b) -> UniqMap Name (IntMap b)
    go (n, newArgMap) acc
      | Just oldArgMap <- lookupUniqMap acc n =
          addToUniqMap acc n (newArgMap `IM.union` oldArgMap)
      | otherwise = addToUniqMap acc n newArgMap
