-- | Extract docs from the renamer output so they can be serialized.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.HsToCore.Docs where

import GHC.Prelude
import GHC.Data.Bag
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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup
import GHC.IORef (readIORef)

-- | Extract docs from renamer output.
-- This is monadic since we need to be able to read documentation added from
-- Template Haskell's @putDoc@, which is stored in 'tcg_th_docs'.
extractDocs :: MonadIO m
            => TcGblEnv
            -> m (Maybe HsDocString, DeclDocMap, ArgDocMap)
            -- ^
            -- 1. Module header
            -- 2. Docs on top level declarations
            -- 3. Docs on arguments
extractDocs TcGblEnv { tcg_semantic_mod = mod
                     , tcg_rn_decls = mb_rn_decls
                     , tcg_insts = insts
                     , tcg_fam_insts = fam_insts
                     , tcg_doc_hdr = mb_doc_hdr
                     , tcg_th_docs = th_docs_var
                     } = do
    th_docs <- liftIO $ readIORef th_docs_var
    let doc_hdr = th_doc_hdr <|> (unLoc <$> mb_doc_hdr)
        ExtractedTHDocs
          th_doc_hdr
          (DeclDocMap th_doc_map)
          (ArgDocMap th_arg_map)
          (DeclDocMap th_inst_map) = extractTHDocs th_docs
    return
      ( doc_hdr
      , DeclDocMap (th_doc_map <> th_inst_map <> doc_map)
      , ArgDocMap (th_arg_map `unionArgMaps` arg_map)
      )
  where
    (doc_map, arg_map) = maybe (M.empty, M.empty)
                               (mkMaps local_insts)
                               mb_decls_with_docs
    mb_decls_with_docs = topDecls <$> mb_rn_decls
    local_insts = filter (nameIsLocalOrFrom mod)
                         $ map getName insts ++ map getName fam_insts

-- | Create decl and arg doc-maps by looping through the declarations.
-- For each declaration, find its names, its subordinates, and its doc strings.
mkMaps :: [Name]
       -> [(LHsDecl GhcRn, [HsDocString])]
       -> (Map Name (HsDocString), Map Name (IntMap HsDocString))
mkMaps instances decls =
    ( f' (map (nubByName fst) decls')
    , f  (filterMapping (not . IM.null) args)
    )
  where
    (decls', args) = unzip (map mappings decls)

    f :: (Ord a, Semigroup b) => [[(a, b)]] -> Map a b
    f = M.fromListWith (<>) . concat

    f' :: Ord a => [[(a, HsDocString)]] -> Map a HsDocString
    f' = M.fromListWith appendDocs . concat

    filterMapping :: (b -> Bool) ->  [[(a, b)]] -> [[(a, b)]]
    filterMapping p = map (filter (p . snd))

    mappings :: (LHsDecl GhcRn, [HsDocString])
             -> ( [(Name, HsDocString)]
                , [(Name, IntMap HsDocString)]
                )
    mappings (L (SrcSpanAnn _ (RealSrcSpan l _)) decl, docStrs) =
           (dm, am)
      where
        doc = concatDocs docStrs
        args = declTypeDocs decl

        subs :: [(Name, [HsDocString], IntMap HsDocString)]
        subs = subordinates instanceMap decl

        (subDocs, subArgs) =
          unzip (map (\(_, strs, m) -> (concatDocs strs, m)) subs)

        ns = names l decl
        subNs = [ n | (n, _, _) <- subs ]
        dm = [(n, d) | (n, Just d) <- zip ns (repeat doc) ++ zip subNs subDocs]
        am = [(n, args) | n <- ns] ++ zip subNs subArgs
    mappings (L (SrcSpanAnn _ (UnhelpfulSpan _)) _, _) = ([], [])

    instanceMap :: Map RealSrcSpan Name
    instanceMap = M.fromList [(l, n) | n <- instances, RealSrcSpan l _ <- [getSrcSpan n] ]

    names :: RealSrcSpan -> HsDecl GhcRn -> [Name]
    names _ (InstD _ d) = maybeToList $ lookupSrcSpan (getInstLoc d) instanceMap
    names l (DerivD {}) = maybeToList (M.lookup l instanceMap) -- See Note [1].
    names _ decl = getMainDeclBinder decl

{-
Note [1]:
---------
We relate ClsInsts to InstDecls and DerivDecls using the SrcSpans buried
inside them. That should work for normal user-written instances (from
looking at GHC sources). We can assume that commented instances are
user-written. This lets us relate Names (from ClsInsts) to comments
(associated with InstDecls and DerivDecls).
-}
getMainDeclBinder :: (Anno (IdGhcP p) ~ SrcSpanAnnN, CollectPass (GhcPass p))
                  => HsDecl (GhcPass p) -> [IdP (GhcPass p)]
getMainDeclBinder (TyClD _ d) = [tcdName d]
getMainDeclBinder (ValD _ d) =
  case collectHsBindBinders CollNoDictBinders d of
    []       -> []
    (name:_) -> [name]
getMainDeclBinder (SigD _ d) = sigNameNoLoc d
getMainDeclBinder (ForD _ (ForeignImport _ name _ _)) = [unLoc name]
getMainDeclBinder (ForD _ (ForeignExport _ _ _ _)) = []
getMainDeclBinder _ = []


sigNameNoLoc :: forall pass. UnXRec pass => Sig pass -> [IdP pass]
sigNameNoLoc (TypeSig    _   ns _)         = map (unXRec @pass) ns
sigNameNoLoc (ClassOpSig _ _ ns _)         = map (unXRec @pass) ns
sigNameNoLoc (PatSynSig  _   ns _)         = map (unXRec @pass) ns
sigNameNoLoc (SpecSig    _   n _ _)        = [unXRec @pass n]
sigNameNoLoc (InlineSig  _   n _)          = [unXRec @pass n]
sigNameNoLoc (FixSig _ (FixitySig _ ns _)) = map (unXRec @pass) ns
sigNameNoLoc _                             = []

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
subordinates :: Map RealSrcSpan Name
             -> HsDecl GhcRn
             -> [(Name, [HsDocString], IntMap HsDocString)]
subordinates instMap decl = case decl of
  InstD _ (ClsInstD _ d) -> do
    DataFamInstDecl { dfid_eqn =
      FamEqn { feqn_tycon = L l _
             , feqn_rhs   = defn }} <- unLoc <$> cid_datafam_insts d
    [ (n, [], IM.empty) | Just n <- [lookupSrcSpan (locA l) instMap] ] ++ dataSubs defn

  InstD _ (DataFamInstD _ (DataFamInstDecl d))
    -> dataSubs (feqn_rhs d)
  TyClD _ d | isClassDecl d -> classSubs d
            | isDataDecl  d -> dataSubs (tcdDataDefn d)
  _ -> []
  where
    classSubs dd = [ (name, doc, declTypeDocs d)
                   | (L _ d, doc) <- classDecls dd
                   , name <- getMainDeclBinder d, not (isValD d)
                   ]
    dataSubs :: HsDataDefn GhcRn
             -> [(Name, [HsDocString], IntMap HsDocString)]
    dataSubs dd = constrs ++ fields ++ derivs
      where
        cons = map unLoc $ (dd_cons dd)
        constrs = [ ( unLoc cname
                    , maybeToList $ fmap unLoc $ con_doc c
                    , conArgDocs c)
                  | c <- cons, cname <- getConNames c ]
        fields  = [ (foExt n, maybeToList $ fmap unLoc doc, IM.empty)
                  | Just flds <- map getRecConArgs_maybe cons
                  , (L _ (ConDeclField _ ns _ doc)) <- (unLoc flds)
                  , (L _ n) <- ns ]
        derivs  = [ (instName, [unLoc doc], IM.empty)
                  | (l, doc) <- concatMap (extract_deriv_clause_tys .
                                           deriv_clause_tys . unLoc) $
                                -- unLoc $ dd_derivs dd
                                dd_derivs dd
                  , Just instName <- [lookupSrcSpan l instMap] ]

        extract_deriv_clause_tys :: LDerivClauseTys GhcRn -> [(SrcSpan, LHsDocString)]
        extract_deriv_clause_tys (L _ dct) =
          case dct of
            DctSingle _ ty -> maybeToList $ extract_deriv_ty ty
            DctMulti _ tys -> mapMaybe extract_deriv_ty tys

        extract_deriv_ty :: LHsSigType GhcRn -> Maybe (SrcSpan, LHsDocString)
        extract_deriv_ty (L l (HsSig{sig_body = L _ ty})) =
          case ty of
            -- deriving (C a {- ^ Doc comment -})
            HsDocTy _ _ doc -> Just (locA l, doc)
            _               -> Nothing

-- | Extract constructor argument docs from inside constructor decls.
conArgDocs :: ConDecl GhcRn -> IntMap HsDocString
conArgDocs (ConDeclH98{con_args = args}) =
  h98ConArgDocs args
conArgDocs (ConDeclGADT{con_g_args = args, con_res_ty = res_ty}) =
  gadtConArgDocs args (unLoc res_ty)

h98ConArgDocs :: HsConDeclH98Details GhcRn -> IntMap HsDocString
h98ConArgDocs con_args = case con_args of
  PrefixCon _ args   -> con_arg_docs 0 $ map (unLoc . hsScaledThing) args
  InfixCon arg1 arg2 -> con_arg_docs 0 [ unLoc (hsScaledThing arg1)
                                       , unLoc (hsScaledThing arg2) ]
  RecCon _           -> IM.empty

gadtConArgDocs :: HsConDeclGADTDetails GhcRn -> HsType GhcRn -> IntMap HsDocString
gadtConArgDocs con_args res_ty = case con_args of
  PrefixConGADT args -> con_arg_docs 0 $ map (unLoc . hsScaledThing) args ++ [res_ty]
  RecConGADT _       -> con_arg_docs 1 [res_ty]

con_arg_docs :: Int -> [HsType GhcRn] -> IntMap HsDocString
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
classDecls :: TyClDecl GhcRn -> [(LHsDecl GhcRn, [HsDocString])]
classDecls class_ = filterDecls . collectDocs . sortLocatedA $ decls
  where
    decls = docs ++ defs ++ sigs ++ ats
    docs  = mkDecls tcdDocs (DocD noExtField) class_
    defs  = mkDecls (bagToList . tcdMeths) (ValD noExtField) class_
    sigs  = mkDecls tcdSigs (SigD noExtField) class_
    ats   = mkDecls tcdATs (TyClD noExtField . FamDecl noExtField) class_

-- | Extract function argument docs from inside top-level decls.
declTypeDocs :: HsDecl GhcRn -> IntMap (HsDocString)
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
typeDocs :: HsType GhcRn -> IntMap HsDocString
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
sigTypeDocs :: HsSigType GhcRn -> IntMap HsDocString
sigTypeDocs (HsSig{sig_body = body}) = typeDocs (unLoc body)

-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup GhcRn -> [(LHsDecl GhcRn, [HsDocString])]
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
      concatMap bagToList . snd . unzip $ binds
    valbinds ValBinds{} = error "expected XValBindsLR"

-- | Collect docs and attach them to the right declarations.
--
-- A declaration may have multiple doc strings attached to it.
collectDocs :: forall p. UnXRec p => [LHsDecl p] -> [(LHsDecl p, [HsDocString])]
-- ^ This is an example.
collectDocs = go [] Nothing
  where
    go docs mprev decls = case (decls, mprev) of
      ((unXRec @p -> DocD _ (DocCommentNext s)) : ds, Nothing)   -> go (s:docs) Nothing ds
      ((unXRec @p -> DocD _ (DocCommentNext s)) : ds, Just prev) -> finished prev docs $ go [s] Nothing ds
      ((unXRec @p -> DocD _ (DocCommentPrev s)) : ds, mprev)     -> go (s:docs) mprev ds
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
filterClasses = map (first (mapLoc filterClass))
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
mkDecls field con = map (mapLoc con) . field

-- | Extracts out individual maps of documentation added via Template Haskell's
-- @putDoc@.
extractTHDocs :: THDocs
              -> ExtractedTHDocs
extractTHDocs docs =
  -- Split up docs into separate maps for each 'DocLoc' type
  ExtractedTHDocs
    docHeader
    (DeclDocMap (searchDocs decl))
    (ArgDocMap (searchDocs args))
    (DeclDocMap (searchDocs insts))
  where
    docHeader :: Maybe HsDocString
    docHeader
      | ((_, s):_) <- filter isModDoc (M.toList docs) = Just (mkHsDocString s)
      | otherwise = Nothing

    isModDoc (ModuleDoc, _) = True
    isModDoc _ = False

    -- Folds over the docs, applying 'f' as the accumulating function.
    -- We use different accumulating functions to sift out the specific types of
    -- documentation
    searchDocs :: Monoid a => (a -> (DocLoc, String) -> a) -> a
    searchDocs f = foldl' f mempty $ M.toList docs

    -- Pick out the declaration docs
    decl acc ((DeclDoc name), s) = M.insert name (mkHsDocString s) acc
    decl acc _ = acc

    -- Pick out the instance docs
    insts acc ((InstDoc name), s) = M.insert name (mkHsDocString s) acc
    insts acc _ = acc

    -- Pick out the argument docs
    args :: Map Name (IntMap HsDocString)
         -> (DocLoc, String)
         -> Map Name (IntMap HsDocString)
    args acc ((ArgDoc name i), s) =
      -- Insert the doc for the arg into the argument map for the function. This
      -- means we have to search to see if an map already exists for the
      -- function, and insert the new argument if it exists, or create a new map
      let ds = mkHsDocString s
       in M.insertWith (\_ m -> IM.insert i ds m) name (IM.singleton i ds) acc
    args acc _ = acc

-- | Unions together two 'ArgDocMaps' (or ArgMaps in haddock-api), such that two
-- maps with values for the same key merge the inner map as well.
-- Left biased so @unionArgMaps a b@ prefers @a@ over @b@.
unionArgMaps :: Map Name (IntMap b)
             -> Map Name (IntMap b)
             -> Map Name (IntMap b)
unionArgMaps a b = M.foldlWithKey go b a
  where
    go acc n newArgMap
      | Just oldArgMap <- M.lookup n acc =
          M.insert n (newArgMap `IM.union` oldArgMap) acc
      | otherwise = M.insert n newArgMap acc
