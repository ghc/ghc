-- | Extract docs from the renamer output so they can be be serialized.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module ExtractDocs (extractDocs) where

import GhcPrelude
import Bag
import GHC.Hs.Binds
import GHC.Hs.Doc
import GHC.Hs.Decls
import GHC.Hs.Extension
import GHC.Hs.Types
import GHC.Hs.Utils
import Name
import NameSet
import SrcLoc
import TcRnTypes

import Control.Applicative
import Data.Bifunctor (first)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup

-- | Extract docs from renamer output.
extractDocs :: TcGblEnv
            -> (Maybe HsDocString, DeclDocMap, ArgDocMap)
            -- ^
            -- 1. Module header
            -- 2. Docs on top level declarations
            -- 3. Docs on arguments
extractDocs TcGblEnv { tcg_semantic_mod = mod
                     , tcg_rn_decls = mb_rn_decls
                     , tcg_insts = insts
                     , tcg_fam_insts = fam_insts
                     , tcg_doc_hdr = mb_doc_hdr
                     } =
    (unLoc <$> mb_doc_hdr, DeclDocMap doc_map, ArgDocMap arg_map)
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
       -> (Map Name (HsDocString), Map Name (Map Int (HsDocString)))
mkMaps instances decls =
    ( f' (map (nubByName fst) decls')
    , f  (filterMapping (not . M.null) args)
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
                , [(Name, Map Int (HsDocString))]
                )
    mappings (L l decl, docStrs) =
           (dm, am)
      where
        doc = concatDocs docStrs
        args = declTypeDocs decl

        subs :: [(Name, [(HsDocString)], Map Int (HsDocString))]
        subs = subordinates instanceMap decl

        (subDocs, subArgs) =
          unzip (map (\(_, strs, m) -> (concatDocs strs, m)) subs)

        ns = names l decl
        subNs = [ n | (n, _, _) <- subs ]
        dm = [(n, d) | (n, Just d) <- zip ns (repeat doc) ++ zip subNs subDocs]
        am = [(n, args) | n <- ns] ++ zip subNs subArgs

    instanceMap :: Map SrcSpan Name
    instanceMap = M.fromList [(getSrcSpan n, n) | n <- instances]

    names :: SrcSpan -> HsDecl GhcRn -> [Name]
    names l (InstD _ d) = maybeToList (M.lookup loc instanceMap) -- See
                                                                 -- Note [1].
      where loc = case d of
              TyFamInstD _ _ -> l -- The CoAx's loc is the whole line, but only
                                  -- for TFs
              _ -> getInstLoc d
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

getMainDeclBinder
  :: (XCollectPat (GhcPass p))
  => HsDecl (GhcPass p)
  -> [IdP (GhcPass p)]
getMainDeclBinder (TyClD _ d) = [tcdName d]
getMainDeclBinder (ValD _ d) =
  case collectHsBindBinders d of
    []       -> []
    (name:_) -> [name]
getMainDeclBinder (SigD _ d) = sigNameNoLoc d
getMainDeclBinder (ForD _ (ForeignImport _ name _ _)) = [unLoc name]
getMainDeclBinder (ForD _ (ForeignExport _ _ _ _)) = []
getMainDeclBinder _ = []

sigNameNoLoc :: Sig pass -> [IdP pass]
sigNameNoLoc (TypeSig    _   ns _)         = map unLoc ns
sigNameNoLoc (ClassOpSig _ _ ns _)         = map unLoc ns
sigNameNoLoc (PatSynSig  _   ns _)         = map unLoc ns
sigNameNoLoc (SpecSig    _   n _ _)        = [unLoc n]
sigNameNoLoc (InlineSig  _   n _)          = [unLoc n]
sigNameNoLoc (FixSig _ (FixitySig _ ns _)) = map unLoc ns
sigNameNoLoc _                             = []

-- Extract the source location where an instance is defined. This is used
-- to correlate InstDecls with their Instance/CoAxiom Names, via the
-- instanceMap.
getInstLoc :: InstDecl (GhcPass p) -> SrcSpan
getInstLoc = \case
  ClsInstD _ (ClsInstDecl { cid_poly_ty = ty }) -> getLoc (hsSigType ty)
  DataFamInstD _ (DataFamInstDecl
    { dfid_eqn = HsIB { hsib_body = FamEqn { feqn_tycon = L l _ }}}) -> l
  TyFamInstD _ (TyFamInstDecl
    -- Since CoAxioms' Names refer to the whole line for type family instances
    -- in particular, we need to dig a bit deeper to pull out the entire
    -- equation. This does not happen for data family instances, for some
    -- reason.
    { tfid_eqn = HsIB { hsib_body = FamEqn { feqn_rhs = L l _ }}}) -> l
  ClsInstD _ (XClsInstDecl _) -> error "getInstLoc"
  DataFamInstD _ (DataFamInstDecl (HsIB _ (XFamEqn _))) -> error "getInstLoc"
  TyFamInstD _ (TyFamInstDecl (HsIB _ (XFamEqn _))) -> error "getInstLoc"
  XInstDecl _ -> error "getInstLoc"
  DataFamInstD _ (DataFamInstDecl (XHsImplicitBndrs _)) -> error "getInstLoc"
  TyFamInstD _ (TyFamInstDecl (XHsImplicitBndrs _)) -> error "getInstLoc"

-- | Get all subordinate declarations inside a declaration, and their docs.
-- A subordinate declaration is something like the associate type or data
-- family of a type class.
subordinates :: Map SrcSpan Name
             -> HsDecl GhcRn
             -> [(Name, [(HsDocString)], Map Int (HsDocString))]
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
    classSubs dd = [ (name, doc, declTypeDocs d)
                   | (L _ d, doc) <- classDecls dd
                   , name <- getMainDeclBinder d, not (isValD d)
                   ]
    dataSubs :: HsDataDefn GhcRn
             -> [(Name, [HsDocString], Map Int (HsDocString))]
    dataSubs dd = constrs ++ fields ++ derivs
      where
        cons = map unLoc $ (dd_cons dd)
        constrs = [ ( unLoc cname
                    , maybeToList $ fmap unLoc $ con_doc c
                    , conArgDocs c)
                  | c <- cons, cname <- getConNames c ]
        fields  = [ (extFieldOcc n, maybeToList $ fmap unLoc doc, M.empty)
                  | RecCon flds <- map getConArgs cons
                  , (L _ (ConDeclField _ ns _ doc)) <- (unLoc flds)
                  , (L _ n) <- ns ]
        derivs  = [ (instName, [unLoc doc], M.empty)
                  | (l, doc) <- mapMaybe (extract_deriv_ty . hsib_body) $
                                concatMap (unLoc . deriv_clause_tys . unLoc) $
                                unLoc $ dd_derivs dd
                  , Just instName <- [M.lookup l instMap] ]

        extract_deriv_ty :: LHsType GhcRn -> Maybe (SrcSpan, LHsDocString)
        extract_deriv_ty (L l ty) =
          case ty of
            -- deriving (forall a. C a {- ^ Doc comment -})
            HsForAllTy{ hst_fvf = ForallInvis
                      , hst_body = L _ (HsDocTy _ _ doc) }
                            -> Just (l, doc)
            -- deriving (C a {- ^ Doc comment -})
            HsDocTy _ _ doc -> Just (l, doc)
            _               -> Nothing

-- | Extract constructor argument docs from inside constructor decls.
conArgDocs :: ConDecl GhcRn -> Map Int (HsDocString)
conArgDocs con = case getConArgs con of
                   PrefixCon args -> go 0 (map unLoc args ++ ret)
                   InfixCon arg1 arg2 -> go 0 ([unLoc arg1, unLoc arg2] ++ ret)
                   RecCon _ -> go 1 ret
  where
    go n = M.fromList . catMaybes . zipWith f [n..]
      where
        f n (HsDocTy _ _ lds) = Just (n, unLoc lds)
        f _ _ = Nothing

    ret = case con of
            ConDeclGADT { con_res_ty = res_ty } -> [ unLoc res_ty ]
            _ -> []

isValD :: HsDecl a -> Bool
isValD (ValD _ _) = True
isValD _ = False

-- | All the sub declarations of a class (that we handle), ordered by
-- source location, with documentation attached if it exists.
classDecls :: TyClDecl GhcRn -> [(LHsDecl GhcRn, [HsDocString])]
classDecls class_ = filterDecls . collectDocs . sortByLoc $ decls
  where
    decls = docs ++ defs ++ sigs ++ ats
    docs  = mkDecls tcdDocs (DocD noExtField) class_
    defs  = mkDecls (bagToList . tcdMeths) (ValD noExtField) class_
    sigs  = mkDecls tcdSigs (SigD noExtField) class_
    ats   = mkDecls tcdATs (TyClD noExtField . FamDecl noExtField) class_

-- | Extract function argument docs from inside top-level decls.
declTypeDocs :: HsDecl GhcRn -> Map Int (HsDocString)
declTypeDocs = \case
  SigD  _ (TypeSig _ _ ty)          -> typeDocs (unLoc (hsSigWcType ty))
  SigD  _ (ClassOpSig _ _ _ ty)     -> typeDocs (unLoc (hsSigType ty))
  SigD  _ (PatSynSig _ _ ty)        -> typeDocs (unLoc (hsSigType ty))
  ForD  _ (ForeignImport _ _ ty _)  -> typeDocs (unLoc (hsSigType ty))
  TyClD _ (SynDecl { tcdRhs = ty }) -> typeDocs (unLoc ty)
  _                                 -> M.empty

nubByName :: (a -> Name) -> [a] -> [a]
nubByName f ns = go emptyNameSet ns
  where
    go _ [] = []
    go s (x:xs)
      | y `elemNameSet` s = go s xs
      | otherwise         = let s' = extendNameSet s y
                            in x : go s' xs
      where
        y = f x

-- | Extract function argument docs from inside types.
typeDocs :: HsType GhcRn -> Map Int (HsDocString)
typeDocs = go 0
  where
    go n = \case
      HsForAllTy { hst_body = ty }        -> go n (unLoc ty)
      HsQualTy   { hst_body = ty }        -> go n (unLoc ty)
      HsFunTy _ (unLoc->HsDocTy _ _ x) ty -> M.insert n (unLoc x) $ go (n+1) (unLoc ty)
      HsFunTy _ _ ty                      -> go (n+1) (unLoc ty)
      HsDocTy _ _ doc                     -> M.singleton n (unLoc doc)
      _                                   -> M.empty

-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup GhcRn -> [(LHsDecl GhcRn, [HsDocString])]
topDecls = filterClasses . filterDecls . collectDocs . sortByLoc . ungroup

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
    typesigs (XValBindsLR (NValBinds _ sig)) = filter (isUserSig . unLoc) sig
    typesigs ValBinds{} = error "expected XValBindsLR"

    valbinds (XValBindsLR (NValBinds binds _)) =
      concatMap bagToList . snd . unzip $ binds
    valbinds ValBinds{} = error "expected XValBindsLR"

-- | Sort by source location
sortByLoc :: [Located a] -> [Located a]
sortByLoc = sortOn getLoc

-- | Collect docs and attach them to the right declarations.
--
-- A declaration may have multiple doc strings attached to it.
collectDocs :: [LHsDecl pass] -> [(LHsDecl pass, [HsDocString])]
-- ^ This is an example.
collectDocs = go [] Nothing
  where
    go docs mprev decls = case (decls, mprev) of
      ((unLoc->DocD _ (DocCommentNext s)) : ds, Nothing)   -> go (s:docs) Nothing ds
      ((unLoc->DocD _ (DocCommentNext s)) : ds, Just prev) -> finished prev docs $ go [s] Nothing ds
      ((unLoc->DocD _ (DocCommentPrev s)) : ds, mprev)     -> go (s:docs) mprev ds
      (d                                  : ds, Nothing)   -> go docs (Just d) ds
      (d                                  : ds, Just prev) -> finished prev docs $ go [] (Just d) ds
      ([]                                     , Nothing)   -> []
      ([]                                     , Just prev) -> finished prev docs []

    finished decl docs rest = (decl, reverse docs) : rest

-- | Filter out declarations that we don't handle in Haddock
filterDecls :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterDecls = filter (isHandled . unLoc . fst)
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
filterClasses :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
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
mkDecls :: (struct -> [Located decl])
        -> (decl -> hsDecl)
        -> struct
        -> [Located hsDecl]
mkDecls field con = map (mapLoc con) . field
