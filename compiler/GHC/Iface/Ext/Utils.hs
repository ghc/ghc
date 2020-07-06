{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module GHC.Iface.Ext.Utils where

import GHC.Prelude

import GHC.Core.Map.Type
import GHC.Driver.Session    ( DynFlags )
import GHC.Driver.Ppr
import GHC.Data.FastString   ( FastString, mkFastString )
import GHC.Iface.Type
import GHC.Core.Multiplicity
import GHC.Types.Name hiding (varName)
import GHC.Types.Name.Set
import GHC.Utils.Outputable hiding ( (<>) )
import qualified GHC.Utils.Outputable as O
import GHC.Types.SrcLoc
import GHC.CoreToIface
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Parser.Annotation

import GHC.Iface.Ext.Types

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import qualified Data.Array as A
import Data.Data                  ( typeOf, typeRepTyCon, Data(toConstr) )
import Data.Maybe                 ( maybeToList, mapMaybe)
import Data.Monoid
import Data.List                  (find)
import Data.Traversable           ( for )
import Data.Coerce
import Control.Monad.Trans.State.Strict hiding (get)
import Control.Monad.Trans.Reader
import qualified Data.Tree as Tree

type RefMap a = M.Map Identifier [(Span, IdentifierDetails a)]

generateReferencesMap
  :: Foldable f
  => f (HieAST a)
  -> RefMap a
generateReferencesMap = foldr (\ast m -> M.unionWith (++) (go ast) m) M.empty
  where
    go ast = M.unionsWith (++) (this : map go (nodeChildren ast))
      where
        this = fmap (pure . (nodeSpan ast,)) $ sourcedNodeIdents $ sourcedNodeInfo ast

renderHieType :: DynFlags -> HieTypeFix -> String
renderHieType dflags ht = showSDoc dflags (ppr $ hieTypeToIface ht)

resolveVisibility :: Type -> [Type] -> [(Bool,Type)]
resolveVisibility kind ty_args
  = go (mkEmptyTCvSubst in_scope) kind ty_args
  where
    in_scope = mkInScopeSet (tyCoVarsOfTypes ty_args)

    go _   _                   []     = []
    go env ty                  ts
      | Just ty' <- coreView ty
      = go env ty' ts
    go env (ForAllTy (Bndr tv vis) res) (t:ts)
      | isVisibleArgFlag vis = (True , t) : ts'
      | otherwise            = (False, t) : ts'
      where
        ts' = go (extendTvSubst env tv t) res ts

    go env (FunTy { ft_res = res }) (t:ts) -- No type-class args in tycon apps
      = (True,t) : (go env res ts)

    go env (TyVarTy tv) ts
      | Just ki <- lookupTyVar env tv = go env ki ts
    go env kind (t:ts) = (True, t) : (go env kind ts) -- Ill-kinded

foldType :: (HieType a -> a) -> HieTypeFix -> a
foldType f (Roll t) = f $ fmap (foldType f) t

selectPoint :: HieFile -> (Int,Int) -> Maybe (HieAST Int)
selectPoint hf (sl,sc) = getFirst $
  flip foldMap (M.toList (getAsts $ hie_asts hf)) $ \(HiePath fs,ast) -> First $
      case selectSmallestContaining (sp fs) ast of
        Nothing -> Nothing
        Just ast' -> Just ast'
 where
   sloc fs = mkRealSrcLoc fs sl sc
   sp fs = mkRealSrcSpan (sloc fs) (sloc fs)

findEvidenceUse :: NodeIdentifiers a -> [Name]
findEvidenceUse ni = [n | (Right n, dets) <- xs, any isEvidenceUse (identInfo dets)]
 where
   xs = M.toList ni

data EvidenceInfo a
  = EvidenceInfo
  { evidenceVar :: Name
  , evidenceSpan :: RealSrcSpan
  , evidenceType :: a
  , evidenceDetails :: Maybe (EvVarSource, Scope, Maybe Span)
  } deriving (Eq,Ord,Functor)

instance (Outputable a) => Outputable (EvidenceInfo a) where
  ppr (EvidenceInfo name span typ dets) =
    hang (ppr name <+> text "at" <+> ppr span O.<> text ", of type:" <+> ppr typ) 4 $
      pdets $$ (pprDefinedAt name)
    where
      pdets = case dets of
        Nothing -> text "is a usage of an external evidence variable"
        Just (src,scp,spn) -> text "is an" <+> ppr (EvidenceVarBind src scp spn)

getEvidenceTreesAtPoint :: HieFile -> RefMap a -> (Int,Int) -> Tree.Forest (EvidenceInfo a)
getEvidenceTreesAtPoint hf refmap point =
  [t | Just ast <- pure $ selectPoint hf point
     , n        <- findEvidenceUse (sourcedNodeIdents $ sourcedNodeInfo ast)
     , Just t   <- pure $ getEvidenceTree refmap n
     ]

getEvidenceTree :: RefMap a -> Name -> Maybe (Tree.Tree (EvidenceInfo a))
getEvidenceTree refmap var = go emptyNameSet var
  where
    go seen var
      | var `elemNameSet` seen = Nothing
      | otherwise = do
          xs <- M.lookup (Right var) refmap
          case find (any isEvidenceBind . identInfo . snd) xs of
            Just (sp,dets) -> do
              typ <- identType dets
              (evdet,children) <- getFirst $ foldMap First $ do
                 det <- S.toList $ identInfo dets
                 case det of
                   EvidenceVarBind src@(EvLetBind (getEvBindDeps -> xs)) scp spn ->
                     pure $ Just ((src,scp,spn),mapMaybe (go $ extendNameSet seen var) xs)
                   EvidenceVarBind src scp spn -> pure $ Just ((src,scp,spn),[])
                   _ -> pure Nothing
              pure $ Tree.Node (EvidenceInfo var sp typ (Just evdet)) children
            -- It is externally bound
            Nothing -> getFirst $ foldMap First $ do
              (sp,dets) <- xs
              if (any isEvidenceUse $ identInfo dets)
                then do
                  case identType dets of
                    Nothing -> pure Nothing
                    Just typ -> pure $ Just $ Tree.Node (EvidenceInfo var sp typ Nothing) []
                else pure Nothing

hieTypeToIface :: HieTypeFix -> IfaceType
hieTypeToIface = foldType go
  where
    go (HTyVarTy n) = IfaceTyVar $ occNameFS $ getOccName n
    go (HAppTy a b) = IfaceAppTy a (hieToIfaceArgs b)
    go (HLitTy l) = IfaceLitTy l
    go (HForAllTy ((n,k),af) t) = let b = (occNameFS $ getOccName n, k)
                                  in IfaceForAllTy (Bndr (IfaceTvBndr b) af) t
    go (HFunTy w a b)   = IfaceFunTy VisArg   w       a    b
    go (HQualTy pred b) = IfaceFunTy InvisArg many_ty pred b
    go (HCastTy a) = a
    go HCoercionTy = IfaceTyVar "<coercion type>"
    go (HTyConApp a xs) = IfaceTyConApp a (hieToIfaceArgs xs)

    -- This isn't fully faithful - we can't produce the 'Inferred' case
    hieToIfaceArgs :: HieArgs IfaceType -> IfaceAppArgs
    hieToIfaceArgs (HieArgs xs) = go' xs
      where
        go' [] = IA_Nil
        go' ((True ,x):xs) = IA_Arg x Required $ go' xs
        go' ((False,x):xs) = IA_Arg x Specified $ go' xs

data HieTypeState
  = HTS
    { tyMap      :: !(TypeMap TypeIndex)
    , htyTable   :: !(IM.IntMap HieTypeFlat)
    , freshIndex :: !TypeIndex
    }

initialHTS :: HieTypeState
initialHTS = HTS emptyTypeMap IM.empty 0

freshTypeIndex :: State HieTypeState TypeIndex
freshTypeIndex = do
  index <- gets freshIndex
  modify' $ \hts -> hts { freshIndex = index+1 }
  return index

compressTypes
  :: HieASTs Type
  -> (HieASTs TypeIndex, A.Array TypeIndex HieTypeFlat)
compressTypes asts = (a, arr)
  where
    (a, (HTS _ m i)) = flip runState initialHTS $
      for asts $ \typ ->
        getTypeIndex typ
    arr = A.array (0,i-1) (IM.toList m)

recoverFullType :: TypeIndex -> A.Array TypeIndex HieTypeFlat -> HieTypeFix
recoverFullType i m = go i
  where
    go i = Roll $ fmap go (m A.! i)

getTypeIndex :: Type -> State HieTypeState TypeIndex
getTypeIndex t
  | otherwise = do
      tm <- gets tyMap
      case lookupTypeMap tm t of
        Just i -> return i
        Nothing -> do
          ht <- go t
          extendHTS t ht
  where
    extendHTS t ht = do
      i <- freshTypeIndex
      modify' $ \(HTS tm tt fi) ->
        HTS (extendTypeMap tm t i) (IM.insert i ht tt) fi
      return i

    go (TyVarTy v) = return $ HTyVarTy $ varName v
    go ty@(AppTy _ _) = do
      let (head,args) = splitAppTys ty
          visArgs = HieArgs $ resolveVisibility (typeKind head) args
      ai <- getTypeIndex head
      argsi <- mapM getTypeIndex visArgs
      return $ HAppTy ai argsi
    go (TyConApp f xs) = do
      let visArgs = HieArgs $ resolveVisibility (tyConKind f) xs
      is <- mapM getTypeIndex visArgs
      return $ HTyConApp (toIfaceTyCon f) is
    go (ForAllTy (Bndr v a) t) = do
      k <- getTypeIndex (varType v)
      i <- getTypeIndex t
      return $ HForAllTy ((varName v,k),a) i
    go (FunTy { ft_af = af, ft_mult = w, ft_arg = a, ft_res = b }) = do
      ai <- getTypeIndex a
      bi <- getTypeIndex b
      wi <- getTypeIndex w
      return $ case af of
                 InvisArg -> case w of Many -> HQualTy ai bi; _ -> error "Unexpected non-unrestricted predicate"
                 VisArg   -> HFunTy wi ai bi
    go (LitTy a) = return $ HLitTy $ toIfaceTyLit a
    go (CastTy t _) = do
      i <- getTypeIndex t
      return $ HCastTy i
    go (CoercionTy _) = return HCoercionTy

resolveTyVarScopes :: M.Map HiePath (HieAST a) -> M.Map HiePath (HieAST a)
resolveTyVarScopes asts = M.map go asts
  where
    go ast = resolveTyVarScopeLocal ast asts

resolveTyVarScopeLocal :: HieAST a -> M.Map HiePath (HieAST a) -> HieAST a
resolveTyVarScopeLocal ast asts = go ast
  where
    resolveNameScope dets = dets{identInfo =
      S.map resolveScope (identInfo dets)}
    resolveScope (TyVarBind sc (UnresolvedScope names Nothing)) =
      TyVarBind sc $ ResolvedScopes
        [ LocalScope binding
        | name <- names
        , Just binding <- [getNameBinding name asts]
        ]
    resolveScope (TyVarBind sc (UnresolvedScope names (Just sp))) =
      TyVarBind sc $ ResolvedScopes
        [ LocalScope binding
        | name <- names
        , Just binding <- [getNameBindingInClass name sp asts]
        ]
    resolveScope scope = scope
    go (Node info span children) = Node info' span $ map go children
      where
        info' = SourcedNodeInfo (updateNodeInfo <$> getSourcedNodeInfo info)
        updateNodeInfo i = i { nodeIdentifiers = idents }
          where
            idents = M.map resolveNameScope $ nodeIdentifiers i

getNameBinding :: Name -> M.Map HiePath (HieAST a) -> Maybe Span
getNameBinding n asts = do
  (_,msp) <- getNameScopeAndBinding n asts
  msp

getNameScope :: Name -> M.Map HiePath (HieAST a) -> Maybe [Scope]
getNameScope n asts = do
  (scopes,_) <- getNameScopeAndBinding n asts
  return scopes

getNameBindingInClass
  :: Name
  -> Span
  -> M.Map HiePath (HieAST a)
  -> Maybe Span
getNameBindingInClass n sp asts = do
  ast <- M.lookup (HiePath (srcSpanFile sp)) asts
  getFirst $ foldMap First $ do
    child <- flattenAst ast
    dets <- maybeToList
      $ M.lookup (Right n) $ sourcedNodeIdents $ sourcedNodeInfo child
    let binding = foldMap (First . getBindSiteFromContext) (identInfo dets)
    return (getFirst binding)

getNameScopeAndBinding
  :: Name
  -> M.Map HiePath (HieAST a)
  -> Maybe ([Scope], Maybe Span)
getNameScopeAndBinding n asts = case nameSrcSpan n of
  RealSrcSpan sp _ -> do -- @Maybe
    ast <- M.lookup (HiePath (srcSpanFile sp)) asts
    defNode <- selectLargestContainedBy sp ast
    getFirst $ foldMap First $ do -- @[]
      node <- flattenAst defNode
      dets <- maybeToList
        $ M.lookup (Right n) $ sourcedNodeIdents $ sourcedNodeInfo node
      scopes <- maybeToList $ foldMap getScopeFromContext (identInfo dets)
      let binding = foldMap (First . getBindSiteFromContext) (identInfo dets)
      return $ Just (scopes, getFirst binding)
  _ -> Nothing

getScopeFromContext :: ContextInfo -> Maybe [Scope]
getScopeFromContext (ValBind _ sc _) = Just [sc]
getScopeFromContext (PatternBind a b _) = Just [a, b]
getScopeFromContext (ClassTyDecl _) = Just [ModuleScope]
getScopeFromContext (Decl _ _) = Just [ModuleScope]
getScopeFromContext (TyVarBind a (ResolvedScopes xs)) = Just $ a:xs
getScopeFromContext (TyVarBind a _) = Just [a]
getScopeFromContext (EvidenceVarBind _ a _) = Just [a]
getScopeFromContext _ = Nothing

getBindSiteFromContext :: ContextInfo -> Maybe Span
getBindSiteFromContext (ValBind _ _ sp) = sp
getBindSiteFromContext (PatternBind _ _ sp) = sp
getBindSiteFromContext _ = Nothing

flattenAst :: HieAST a -> [HieAST a]
flattenAst n =
  n : concatMap flattenAst (nodeChildren n)

smallestContainingSatisfying
  :: Span
  -> (HieAST a -> Bool)
  -> HieAST a
  -> Maybe (HieAST a)
smallestContainingSatisfying sp cond node
  | nodeSpan node `containsSpan` sp = getFirst $ mconcat
      [ foldMap (First . smallestContainingSatisfying sp cond) $
          nodeChildren node
      , First $ if cond node then Just node else Nothing
      ]
  | sp `containsSpan` nodeSpan node = Nothing
  | otherwise = Nothing

selectLargestContainedBy :: Span -> HieAST a -> Maybe (HieAST a)
selectLargestContainedBy sp node
  | sp `containsSpan` nodeSpan node = Just node
  | nodeSpan node `containsSpan` sp =
      getFirst $ foldMap (First . selectLargestContainedBy sp) $
        nodeChildren node
  | otherwise = Nothing

selectSmallestContaining :: Span -> HieAST a -> Maybe (HieAST a)
selectSmallestContaining sp node
  | nodeSpan node `containsSpan` sp = getFirst $ mconcat
      [ foldMap (First . selectSmallestContaining sp) $ nodeChildren node
      , First (Just node)
      ]
  | sp `containsSpan` nodeSpan node = Nothing
  | otherwise = Nothing

definedInAsts :: M.Map HiePath (HieAST a) -> Name -> Bool
definedInAsts asts n = case nameSrcSpan n of
  RealSrcSpan sp _ -> M.member (HiePath (srcSpanFile sp)) asts
  _ -> False

getEvidenceBindDeps :: ContextInfo -> [Name]
getEvidenceBindDeps (EvidenceVarBind (EvLetBind xs) _ _) =
  getEvBindDeps xs
getEvidenceBindDeps _ = []

isEvidenceBind :: ContextInfo -> Bool
isEvidenceBind EvidenceVarBind{} = True
isEvidenceBind _ = False

isEvidenceContext :: ContextInfo -> Bool
isEvidenceContext EvidenceVarUse = True
isEvidenceContext EvidenceVarBind{} = True
isEvidenceContext _ = False

isEvidenceUse :: ContextInfo -> Bool
isEvidenceUse EvidenceVarUse = True
isEvidenceUse _ = False

isOccurrence :: ContextInfo -> Bool
isOccurrence Use = True
isOccurrence EvidenceVarUse = True
isOccurrence _ = False

scopeContainsSpan :: Scope -> Span -> Bool
scopeContainsSpan NoScope _ = False
scopeContainsSpan ModuleScope _ = True
scopeContainsSpan (LocalScope a) b = a `containsSpan` b

-- | One must contain the other. Leaf nodes cannot contain anything
combineAst :: HieAST Type -> HieAST Type -> HieAST Type
combineAst a@(Node aInf aSpn xs) b@(Node bInf bSpn ys)
  | aSpn == bSpn = Node (aInf `combineSourcedNodeInfo` bInf) aSpn (mergeAsts xs ys)
  | aSpn `containsSpan` bSpn = combineAst b a
combineAst a (Node xs span children) = Node xs span (insertAst a children)

-- | Insert an AST in a sorted list of disjoint Asts
insertAst :: HieAST Type -> [HieAST Type] -> [HieAST Type]
insertAst x = mergeAsts [x]

nodeInfo :: HieAST Type -> NodeInfo Type
nodeInfo = foldl' combineNodeInfo emptyNodeInfo . getSourcedNodeInfo . sourcedNodeInfo

emptyNodeInfo :: NodeInfo a
emptyNodeInfo = NodeInfo S.empty [] M.empty

sourcedNodeIdents :: SourcedNodeInfo a -> NodeIdentifiers a
sourcedNodeIdents = M.unionsWith (<>) . fmap nodeIdentifiers . getSourcedNodeInfo

combineSourcedNodeInfo :: SourcedNodeInfo Type -> SourcedNodeInfo Type -> SourcedNodeInfo Type
combineSourcedNodeInfo = coerce $ M.unionWith combineNodeInfo

-- | Merge two nodes together.
--
-- Precondition and postcondition: elements in 'nodeType' are ordered.
combineNodeInfo :: NodeInfo Type -> NodeInfo Type -> NodeInfo Type
(NodeInfo as ai ad) `combineNodeInfo` (NodeInfo bs bi bd) =
  NodeInfo (S.union as bs) (mergeSorted ai bi) (M.unionWith (<>) ad bd)
  where
    mergeSorted :: [Type] -> [Type] -> [Type]
    mergeSorted la@(a:as) lb@(b:bs) = case nonDetCmpType a b of
                                        LT -> a : mergeSorted as lb
                                        EQ -> a : mergeSorted as bs
                                        GT -> b : mergeSorted la bs
    mergeSorted as [] = as
    mergeSorted [] bs = bs


{- | Merge two sorted, disjoint lists of ASTs, combining when necessary.

In the absence of position-altering pragmas (ex: @# line "file.hs" 3@),
different nodes in an AST tree should either have disjoint spans (in
which case you can say for sure which one comes first) or one span
should be completely contained in the other (in which case the contained
span corresponds to some child node).

However, since Haskell does have position-altering pragmas it /is/
possible for spans to be overlapping. Here is an example of a source file
in which @foozball@ and @quuuuuux@ have overlapping spans:

@
module Baz where

# line 3 "Baz.hs"
foozball :: Int
foozball = 0

# line 3 "Baz.hs"
bar, quuuuuux :: Int
bar = 1
quuuuuux = 2
@

In these cases, we just do our best to produce sensible `HieAST`'s. The blame
should be laid at the feet of whoever wrote the line pragmas in the first place
(usually the C preprocessor...).
-}
mergeAsts :: [HieAST Type] -> [HieAST Type] -> [HieAST Type]
mergeAsts xs [] = xs
mergeAsts [] ys = ys
mergeAsts xs@(a:as) ys@(b:bs)
  | span_a `containsSpan`   span_b = mergeAsts (combineAst a b : as) bs
  | span_b `containsSpan`   span_a = mergeAsts as (combineAst a b : bs)
  | span_a `rightOf`        span_b = b : mergeAsts xs bs
  | span_a `leftOf`         span_b = a : mergeAsts as ys

  -- These cases are to work around ASTs that are not fully disjoint
  | span_a `startsRightOf`  span_b = b : mergeAsts as ys
  | otherwise                      = a : mergeAsts as ys
  where
    span_a = nodeSpan a
    span_b = nodeSpan b

rightOf :: Span -> Span -> Bool
rightOf s1 s2
  = (srcSpanStartLine s1, srcSpanStartCol s1)
       >= (srcSpanEndLine s2, srcSpanEndCol s2)
    && (srcSpanFile s1 == srcSpanFile s2)

leftOf :: Span -> Span -> Bool
leftOf s1 s2
  = (srcSpanEndLine s1, srcSpanEndCol s1)
       <= (srcSpanStartLine s2, srcSpanStartCol s2)
    && (srcSpanFile s1 == srcSpanFile s2)

startsRightOf :: Span -> Span -> Bool
startsRightOf s1 s2
  = (srcSpanStartLine s1, srcSpanStartCol s1)
       >= (srcSpanStartLine s2, srcSpanStartCol s2)

-- | combines and sorts ASTs using a merge sort
mergeSortAsts :: [HieAST Type] -> [HieAST Type]
mergeSortAsts = go . map pure
  where
    go [] = []
    go [xs] = xs
    go xss = go (mergePairs xss)
    mergePairs [] = []
    mergePairs [xs] = [xs]
    mergePairs (xs:ys:xss) = mergeAsts xs ys : mergePairs xss

simpleNodeInfo :: FastString -> FastString -> NodeInfo a
simpleNodeInfo cons typ = NodeInfo (S.singleton (NodeAnnotation cons typ)) [] M.empty

locOnly :: Monad m => SrcSpan -> ReaderT NodeOrigin m [HieAST a]
locOnly (RealSrcSpan span _) = do
  org <- ask
  let e = mkSourcedNodeInfo org $ emptyNodeInfo
  pure [Node e span []]
locOnly _ = pure []

mkScopeA :: SrcSpanAnn' ann -> Scope
mkScopeA l = mkScope (locA l)

mkScope :: SrcSpan -> Scope
mkScope (RealSrcSpan sp _) = LocalScope sp
mkScope _ = NoScope

mkLScope :: Located a -> Scope
mkLScope = mkScope . getLoc

-- mkLScopeA :: LocatedA a -> Scope
mkLScopeA :: GenLocated (SrcSpanAnn' a) e -> Scope
mkLScopeA = mkScope . locA . getLoc

mkLScopeN :: LocatedN a -> Scope
mkLScopeN = mkScope . getLocA

combineScopes :: Scope -> Scope -> Scope
combineScopes ModuleScope _ = ModuleScope
combineScopes _ ModuleScope = ModuleScope
combineScopes NoScope x = x
combineScopes x NoScope = x
combineScopes (LocalScope a) (LocalScope b) =
  mkScope $ combineSrcSpans (RealSrcSpan a Nothing) (RealSrcSpan b Nothing)

mkSourcedNodeInfo :: NodeOrigin -> NodeInfo a -> SourcedNodeInfo a
mkSourcedNodeInfo org ni = SourcedNodeInfo $ M.singleton org ni

{-# INLINEABLE makeNodeA #-}
makeNodeA
  :: (Monad m, Data a)
  => a                       -- ^ helps fill in 'nodeAnnotations' (with 'Data')
  -> SrcSpanAnn' ann         -- ^ return an empty list if this is unhelpful
  -> ReaderT NodeOrigin m [HieAST b]
makeNodeA x spn = makeNode x (locA spn)

{-# INLINEABLE makeNode #-}
makeNode
  :: (Monad m, Data a)
  => a                       -- ^ helps fill in 'nodeAnnotations' (with 'Data')
  -> SrcSpan                 -- ^ return an empty list if this is unhelpful
  -> ReaderT NodeOrigin m [HieAST b]
makeNode x spn = do
  org <- ask
  pure $ case spn of
    RealSrcSpan span _ -> [Node (mkSourcedNodeInfo org $ simpleNodeInfo cons typ) span []]
    _ -> []
  where
    cons = mkFastString . show . toConstr $ x
    typ = mkFastString . show . typeRepTyCon . typeOf $ x

{-# INLINEABLE makeTypeNodeA #-}
makeTypeNodeA
  :: (Monad m, Data a)
  => a                       -- ^ helps fill in 'nodeAnnotations' (with 'Data')
  -> SrcSpanAnnA             -- ^ return an empty list if this is unhelpful
  -> Type                    -- ^ type to associate with the node
  -> ReaderT NodeOrigin m [HieAST Type]
makeTypeNodeA x spn etyp = makeTypeNode x (locA spn) etyp

{-# INLINEABLE makeTypeNode #-}
makeTypeNode
  :: (Monad m, Data a)
  => a                       -- ^ helps fill in 'nodeAnnotations' (with 'Data')
  -> SrcSpan                 -- ^ return an empty list if this is unhelpful
  -> Type                    -- ^ type to associate with the node
  -> ReaderT NodeOrigin m [HieAST Type]
makeTypeNode x spn etyp = do
  org <- ask
  pure $ case spn of
    RealSrcSpan span _ ->
      [Node (mkSourcedNodeInfo org $ NodeInfo (S.singleton (NodeAnnotation cons typ)) [etyp] M.empty) span []]
    _ -> []
  where
    cons = mkFastString . show . toConstr $ x
    typ = mkFastString . show . typeRepTyCon . typeOf $ x
