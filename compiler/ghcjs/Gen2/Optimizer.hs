{-# Language CPP,
             GADTs,
             DeriveDataTypeable,
             NoMonomorphismRestriction,
             TupleSections,
             OverloadedStrings
  #-}
{- |

  Optimizer:

  Basic optimization of the generated JavaScript to
  reduce file size and improve readability

  assumptions:
  - getProperty is pure

-}
module Gen2.Optimizer where

-- import           Control.Lens
import           Control.Monad

import           Data.Array
import qualified Data.Bits as Bits
import           Data.Data
-- import           Data.Data.Lens
-- import           Data.Default
import qualified Data.Foldable as F
import qualified Data.IntSet as IS
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List (foldl')
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import Prelude
import Data.Function

import qualified Data.Text as T
import           Data.Word

import           Panic

import           Compiler.JMacro
import           Compiler.JMacro.Lens
import           Compiler.JMacro.Symbols

import           Gen2.Base
import           Gen2.Dataflow
import           Gen2.Rts

optimize :: JStat -> JStat
#ifdef DISABLE_OPTIMIZER
optimize = id
#else
optimize = id
-- fixme
-- optimize = renameLocalVars . removeDeadVars . dataflow
#endif

renameLocalVars :: JStat -> JStat
renameLocalVars = thisFunction . nestedFuns %~ renameLocalsFun newLocals

-- traverse all expressions in the statement (no recursion into nested statements)
statExprs :: Traversal' JStat JExpr
statExprs f (ReturnStat e) = ReturnStat <$> f e
statExprs f (IfStat e s1 s2) = IfStat <$> f e <*> pure s1 <*> pure s2
statExprs f (ForInStat b i e s) = ForInStat b i <$> f e <*> pure s
statExprs f (SwitchStat e es s) = SwitchStat <$> f e <*> (traverse . _1) f es <*> pure s
statExprs f (ApplStat e1 es) = ApplStat <$> f e1 <*> traverse f es
statExprs f (UOpStat o e) = UOpStat o <$> f e
statExprs f (AssignStat e1 e2) = AssignStat <$> f e1 <*> f e2
statExprs _ s = pure s

subStats :: Traversal' JStat JStat
subStats f (IfStat e s1 s2) = IfStat e <$> f s1 <*> f s2
subStats f (WhileStat b e s) = WhileStat b e <$> f s
subStats f (ForInStat b i e s) = ForInStat b i e <$> f s
subStats f (SwitchStat e es s) = SwitchStat e <$> (traverse . _2) f es <*> f s
subStats f (TryStat s1 i s2 s3) = TryStat <$> f s1 <*> pure i <*> f s2 <*> f s3
subStats f (BlockStat ss) = BlockStat <$> traverse f ss
subStats f (LabelStat l s) = LabelStat l <$> f s
subStats _ s = pure s

subExprs :: Traversal' JExpr JExpr
subExprs f (SelExpr e i) = SelExpr <$> f e <*> pure i
subExprs f (IdxExpr e1 e2) = IdxExpr <$> f e1 <*> f e2
subExprs f (InfixExpr o e1 e2) = InfixExpr o <$> f e1 <*> f e2
subExprs f (UOpExpr xs e) = UOpExpr xs <$> f e
subExprs f (IfExpr e1 e2 e3) = IfExpr <$> f e1 <*> f e2 <*> f e3
subExprs f (ApplExpr e es) = ApplExpr <$> f e <*> traverse f es
subExprs f e = f e

-- traverse all 'leaf' statements in this function
thisFunction :: Traversal' JStat JStat
thisFunction f (IfStat e s1 s2) = IfStat e <$> thisFunction f s1
                                           <*> thisFunction f s2
thisFunction f (WhileStat b e s) = WhileStat b e <$> thisFunction f s
thisFunction f (ForInStat b i e s) = ForInStat b i e <$> thisFunction f s
thisFunction f (SwitchStat e es s) = SwitchStat e <$> (traverse . _2 . thisFunction) f es
                                                  <*> thisFunction f s
thisFunction f (TryStat s1 i s2 s3) = TryStat <$> thisFunction f s1
                                              <*> pure i
                                              <*> thisFunction f s2
                                              <*> thisFunction f s3
thisFunction f (BlockStat ss) = BlockStat <$> tinplate (thisFunction f) ss
thisFunction f (LabelStat l s) = LabelStat l <$> thisFunction f s
thisFunction f s = f s

localVars :: JStat -> [Ident]
localVars = universeOf subStats >=> getLocals
  where
    getLocals (DeclStat i)        = [i]
    getLocals (ForInStat _ i _ _) = [i] -- remove ident?
    getLocals (TryStat _ _i _ _)  = []  -- is this correct?
    getLocals _ = []

localIdents :: Traversal' JStat Ident
localIdents = tinplate . localFunctionVals . _JVar

allIdents :: Traversal' JStat Ident
allIdents = tinplate . functionVals . _JVar

nestedFuns :: (Applicative f, Data s)
           => (([Ident], JStat) -> f ([Ident], JStat)) -> s -> f s
nestedFuns = tinplate . localFunctionVals . _JFunc

-- all idents not in expressions in this function, including in declarations
nonExprLocalIdents :: Traversal' JStat Ident
nonExprLocalIdents f (IfStat e s1 s2) = IfStat e <$> nonExprLocalIdents f s1
                                                 <*> nonExprLocalIdents f s2
nonExprLocalIdents f (DeclStat i)   = DeclStat <$> f i
nonExprLocalIdents f (WhileStat b e s) = WhileStat b e <$> nonExprLocalIdents f s
nonExprLocalIdents f (ForInStat b i e s) = ForInStat b <$> f i
                                                       <*> pure e
                                                       <*> nonExprLocalIdents f s
nonExprLocalIdents f (SwitchStat e es s) = SwitchStat e <$> (traverse . _2 . nonExprLocalIdents) f es
                                                        <*> nonExprLocalIdents f s
nonExprLocalIdents f (TryStat s1 i s2 s3) = TryStat <$> nonExprLocalIdents f s1
                                                    <*> f i
                                                    <*> nonExprLocalIdents f s2
                                                    <*> nonExprLocalIdents f s3
nonExprLocalIdents f (BlockStat ss) = BlockStat <$> (traverse . nonExprLocalIdents) f ss
nonExprLocalIdents f (LabelStat l s) = LabelStat l <$> nonExprLocalIdents f s
nonExprLocalIdents _ s = pure s

functionVals :: Traversal' JVal JVal
functionVals f (JList es)    = JList <$> tinplate (functionVals f) es
functionVals f (JHash m)     = JHash <$> tinplate (functionVals f) m
functionVals f (JFunc as es) = JFunc as <$> tinplate (functionVals f) es
functionVals f v             = f v

localFunctionVals :: Traversal' JVal JVal
localFunctionVals f (JList es)   = JList <$> tinplate (localFunctionVals f) es
localFunctionVals f (JHash m)    = JHash <$> tinplate (localFunctionVals f) m  -- lens bug?
localFunctionVals f v            = f v

-- fixme: check that try/catch is handled correctly, with separate local vars for the caught things
renameLocalsFun :: [Ident] -> ([Ident], JStat) -> ([Ident], JStat)
renameLocalsFun newNames f = ( map renameVar args
                             , s' & localIdents %~ renameVar & nonExprLocalIdents %~ renameVar
                             )
  where
    (_,   s')   = nestedFuns %~ renameGlobals rename $ (args, s)
    (args, s)   = nestedFuns %~ renameLocalsFun newNames $ f
    renameVar i = fromMaybe i (M.lookup i rename)
    rename      = M.fromList $ zip locals (filter (`S.notMember` globals) newNames)
    globals     = idents S.\\ (S.fromList locals)
    idents      = S.fromList (s ^.. allIdents ++ args)
    locals      = L.nub $ localVars s -- args ++ s ^.. localVars

-- propagate renaming of variables global relative to this function body
renameGlobals :: M.Map Ident Ident -> ([Ident],JStat) -> ([Ident],JStat)
renameGlobals m f@(args,s) = (args, localIdents %~ renameVar $ s')
  where
    (_, s')     = nestedFuns %~ renameGlobals m' $ f
    renameVar i = fromMaybe i (M.lookup i m')
    locals      = L.nub $ args ++ localVars s
    m'          = m M.\\ (M.fromList $ zip locals (repeat (TxtI "_")))

tup :: a -> (a,a)
tup x = (x,x)

-----------------------------------------------------
-- dead variable elimination:
--  a dead var is a local variable that is never read or assigned
-----------------------------------------------------

removeDeadVars :: JStat -> JStat
removeDeadVars s = s & thisFunction . nestedFuns . _2 %~ removeDeadVars'

removeDeadVars' :: JStat -> JStat
removeDeadVars' s = transformOf subStats (removeDead dead) s
  where
    dead    = locals S.\\ nonDead
    nonDead = panic "Optimizer removeDeadVars'" -- XXX remove lens S.fromList (universeOf subStats s ^.. traverse . liveVarsS)
    locals  = S.fromList (localVars s)

removeDead :: S.Set Ident -> JStat -> JStat
removeDead dead (DeclStat i)
   | i `S.member` dead = mempty
removeDead _ s = s

liveVarsS :: Traversal' JStat Ident
liveVarsS f s = (statExprs . liveVarsE) f s

liveVarsE :: Traversal' JExpr Ident
liveVarsE f (ValExpr (JVar i))   = ValExpr . JVar <$> f i
liveVarsE _ v@(ValExpr JFunc {}) = pure v
liveVarsE f e                    = tinplate (liveVarsE f) e

----------------------------------------------------------

data Annot = Annot { aIdents      :: IdSet -- identifiers in the expression
                   , aSideEffects :: Bool  -- can the expression have side effects
                   , aUse         :: IntMap Int -- identifiers with number of occurrences
                   } deriving (Data, Typeable, Eq, Show)

type Graph'      = Graph Annot
type Node'       = Node  Annot
type AExpr'      = AExpr Annot
type SimpleStat' = SimpleStat Annot

-- things that we do not want to calculate multiple times for the graph
data Cache = Cache { cachedKnownFuns :: IntMap IdSet
                   , cachedKnownPure :: IdSet
                   }

cache :: Graph' -> Cache
cache g = Cache (knownFuns g)
                (knownPureFuns g)

-- empty annotation to get started, replace this with a proper annotations before rewriting
emptyAnnot :: Annot
emptyAnnot = Annot (error "emptyAnnot: empty annotation")
                   (error "emptyAnnot: empty annotation")
                   (error "emptyAnnot: empty annotation")

annotate :: Cache -> Expr -> AExpr'
annotate c e = AExpr (Annot (exprIdents e) (mightHaveSideeffects c e) (exprIdents' e)) e

noAnnot :: Graph' -> Expr -> AExpr'
noAnnot _ e = AExpr emptyAnnot e

normalizeAnnot :: Cache -> Graph' -> Graph'
normalizeAnnot c g = g & nodes . traverse %~ normalizeAExprs
  where
    normalizeAExprs :: Node' -> Node'
    normalizeAExprs = rewriteNodeExprs
      (\isCond -> annotate c . normalize isCond c g . fromA)

dataflow :: JStat -> JStat
dataflow = thisFunction . nestedFuns %~ f
  where f d@(args, stat)
          | noDataflow stat = d
          | otherwise       = ung stat . optimizeSequences c
                                       . liveness args' locals
                                       . constants args' locals c
                                       . liveness args' locals
                                       . constants args' locals c
                                       . liveness args' locals
                                       . constants args' locals c
--                                       . propagateStack args' locals c  -- fixme this analysis is buggy and we run into bugs now that we generate branches that don't return
                                       $ g1
          where
            g0 = flattenSequences (cfg noAnnot stat)
            c  = cache g0
            g1 :: Graph'
            g1 = normalizeAnnot c g0
            args' :: IdSet
            args' = IS.fromList $ mapMaybe (lookupId g0) args
            locals = localVarsGraph g0
            ung _ g = (args, unCfg g)
            -- ung _ g = (args, [j| if(runOptimized) { `unCfg g` } else { `stat` } |]) -- use this for utils/testOptimizer.hs

-----------------------------------------------------------
-- detect some less frequently used constructs that we
-- do not support in the dataflow analyzer yet
-----------------------------------------------------------

noDataflow :: JStat -> Bool
noDataflow stat = any unsupportedExpr (universeOnOf tinplate tinplate stat)
  where
    unsupportedExpr (ValExpr JFunc {}) = True
    unsupportedExpr _                  = False

-----------------------------------------------------------
-- liveness: remove unnecessary assignments
-----------------------------------------------------------

liveness :: IdSet -> IdSet -> Graph' -> Graph'
liveness args locals g = g & nodes %~ IM.mapWithKey updNode
  where
    la = args `IS.union` locals
    lf = livenessFacts g
    updNode :: NodeId -> Node' -> Node'
    updNode nid (SimpleNode (AssignS (AExpr _ (ValE (Var i))) e))
      | not (live i (lookupFact def nid 0 lf))
         && not (mightHaveSideeffects' e)
         && i `IS.member` la = SequenceNode []
    updNode _  n = n

    live :: Id -> Liveness -> Bool
    live i (Liveness x y) = i `IM.member` x || i `IM.member` y

-- Id -> use
data Liveness = Liveness { lUse   :: IntMap Use -- normal uses
                         , lExcep :: IntMap Use -- if the statement generates an exception uses
                         } deriving (Eq, Show)

instance Default Liveness where
  def = Liveness IM.empty IM.empty

data Use = Once | Multiple deriving (Show, Eq, Bounded, Ord, Enum)

-- fixme for trystat probably

livenessFacts :: Graph Annot -> Facts Liveness
livenessFacts g = {- dumpLive g $ -} foldBackward c f def def g
  where
    c :: Liveness -> Liveness -> Liveness
    c (Liveness x1 y1) (Liveness x2 y2) = Liveness (IM.unionWith lUb x1 x2) (IM.unionWith lUb y1 y2)
    c' :: IntMap Use -> IntMap Use -> IntMap Use
    c' = IM.unionWith lUb

    p :: Liveness -> Liveness -> Liveness
    p (Liveness x1 y1) (Liveness x2 y2) = Liveness (IM.unionWith lPlus x1 x2) (IM.unionWith lPlus y1 y2)
--    p' :: IntMap Use -> IntMap Use -> IntMap Use
--    p' = IM.unionWith lUb

    r :: Id -> Liveness -> Liveness
    r i (Liveness x y) = Liveness (IM.delete i x) y

    f :: Backward Annot Liveness
    f = Backward { bIf       = \_ -> cb
                 , bWhile    = \_ -> cb
                 , bDoWhile  = \_ e x -> x `p` exprL e
                 , bSimple   = \_ -> simple
                 , bBreak    = \_ -> id
                 , bContinue = \_ -> id
                 , bReturn   = \_ e -> exprL e
                 , bTry      = \_ _ -> try
                 , bForIn    = \_ _ _ -> cb
                 , bSwitch   = \_ e x -> x `p` exprL e
                 }

    cb :: AExpr' -> (Liveness, Liveness) -> Liveness
    cb e (x1, x2) = (x1 `c` x2) `p` exprL e

    exprL :: AExpr' -> Liveness
    exprL e = Liveness (fmap (\x -> if x <= 1 then Once else Multiple) . aUse . getAnnot $ e) IM.empty

    simple :: SimpleStat' -> Liveness -> Liveness
    simple (DeclS _) l = l
    simple (AssignS e1 e2) l
      | (ValE (Var i)) <-  fromA e1 = r i l `p` exprL e2
      | otherwise                   = (l `p` exprL e1) `p` exprL e2
    simple (ExprS e) l = l `p` exprL e

    try :: (Liveness, Liveness, Liveness) -> (Liveness, Liveness)
    try (bt, bc, bf) = let et  = Liveness (lUse bf) (lUse bc `c'` lExcep bc) -- is this correct?
                           bt' = Liveness (lUse bt) (lExcep bf)
                       in (bt', et)

lPlus :: Use -> Use -> Use
lPlus _ _ = Multiple

lUb :: Use -> Use -> Use
lUb Once Once = Once
lUb _    _    = Multiple

-----------------------------------------------------------
-- constants and assignment propagation
--   removes unreachable branches and statements
-----------------------------------------------------------

data CVals = CUnreached
           | CReached (IntMap CFact)
  deriving (Eq, Data, Typeable, Show)

newtype CMods = CMods IdSet
  deriving (Eq, Data, Typeable, Show)

-- known value, and whether this value may be used multiple times
data CFact = CKnown AExpr' Bool
  deriving (Eq, Data, Typeable, Show)

(%%) :: (IntMap CFact -> IntMap CFact) -> CVals -> CVals
_ %% CUnreached = CUnreached
f %% CReached m = CReached (f m)

constants :: IdSet -> IdSet -> Cache -> Graph' -> Graph'
constants args locals c g = g & nodes %~ IM.mapWithKey rewriteNode
  where
    fs  = constantsFacts lfs c g
    lfs = livenessFacts g

    -- fact index 2 is what flows into the expression
    nodeFact :: NodeId -> CVals
    nodeFact nid = lookupFact CUnreached nid 2 fs

    rewriteNode :: NodeId -> Node' -> Node'
    rewriteNode nid _
      | lookupFact CUnreached nid 0 fs == CUnreached = SequenceNode [] -- unreachable code
    rewriteNode nid (IfNode e s1 s2)
      | Just True  <- c, not s = SequenceNode [s1] -- else branch unreachable
      | Just False <- c, not s = SequenceNode [s2] -- if branch unreachable
      where
        s  = mightHaveSideeffects' e'
        c  = exprCond' e'
        e' = propagateExpr (nodeFact nid) True e
    rewriteNode nid (WhileNode e _s1)  -- loop body unreachable
      | Just False <- exprCond' e', not (mightHaveSideeffects' e') = SequenceNode []
      where
        e' = propagateExpr (nodeFact nid) True e
    rewriteNode nid (SimpleNode (AssignS e1 e2))
      | (ValE (Var x)) <- fromA e1, (ValE (Var y)) <- fromA e2, x == y
          = SequenceNode []
      | (ValE (Var x)) <- fromA e1, not (live x (lookupFact def nid 0 lfs))
         && not (mightHaveSideeffects' e2)
         && x `IS.member` la = SequenceNode []
      | (ValE (Var _)) <- fromA e1
          = SimpleNode (AssignS e1 (propagateExpr (nodeFact nid) False e2))
    rewriteNode nid n = rewriteNodeExprs (propagateExpr (nodeFact nid)) n

    la = args `IS.union` locals

    live :: Id -> Liveness -> Bool
    live i (Liveness x y) = i `IM.member` x || i `IM.member` y

    -- apply our facts to an expression
    propagateExpr :: CVals -> Bool -> AExpr' -> AExpr'
    propagateExpr CUnreached _isCond ae = ae
    propagateExpr cv@CReached{} isCond ae
      | not hasKnownDeps = ae
      | e@(ApplE (ValE _) args) <- fromA ae = annotate c . normalize isCond c g $
          propagateExpr' (maybe (CReached IM.empty) (`deleteConstants` cv) (IS.unions <$> mapM (mutated c) args)) e
      | otherwise = annotate c . normalize isCond c g $
          propagateExpr' (maybe (CReached IM.empty) (`deleteConstants` cv) (mutated c (fromA ae))) (fromA ae)
      where
        hasKnownDeps = not . IS.null $ exprDeps ae `IS.intersection` knownValues cv

    propagateExpr' :: CVals -> Expr -> Expr
    propagateExpr' (CReached cv) e = let e' = transformOf tinplate f e
                                     in {- Debug.Trace.trace (p e') -} e'
      where
        f :: Expr -> Expr
        f (ValE (Var i)) | Just (CKnown ae _) <- IM.lookup i cv = fromA ae
        f x = x
        --p e' = "propagating:\n" ++ show e ++ "\n" ++ show (e'::Expr)
    propagateExpr' _ _ = panic "propagateExpr'"

-- set of identifiers with known values
knownValues :: CVals -> IdSet
knownValues (CReached cv) = IS.fromList . (\xs -> [ x | (x, CKnown _ _) <- xs]) . IM.toList $ cv
knownValues _             = IS.empty

-- constant and variable assignment propagation
constantsFacts :: Facts Liveness -> Cache -> Graph' -> Facts CVals
constantsFacts lfs c g = {- dumpFacts g $ -} foldForward combineConstants f0 (CReached IM.empty) CUnreached g
  where
    f0 :: Forward Annot CVals
    f0 = def { fIf      = removeMutated1t
             , fWhile   = removeMutated1t
             , fDoWhile = removeMutated1t
             , fSwitch  = switched
             , fReturn  = removeMutated1
             , fForIn   = \_ _ -> removeMutated1t
             , fSimple  = simple -- \nid s m -> simple nid s m
             , fTry     = \_ x -> (x, CReached IM.empty, CReached IM.empty)
             }
    usedOnce :: NodeId -> Id -> Bool
    usedOnce nid i = let (Liveness u excep) = lookupFact def nid 0 lfs
                     in  IM.lookup i u == Just Once && isNothing (IM.lookup i excep)

    switched :: NodeId -> AExpr' -> [AExpr'] -> CVals -> ([CVals], CVals)
    switched _ e es m = let m' = removeMutated e m in (replicate (length es) m', m')
    simple :: NodeId -> SimpleStat' -> CVals -> CVals
    simple _ _ CUnreached = CUnreached
    simple _   DeclS{} m = m
    simple _   (ExprS e) m = removeMutated e m
    simple nid (AssignS e1 e2) m
       | (ValE (Var i)) <- fromA e1, (ValE (IntV _))                <- fromA e2 = IM.insert i (CKnown e2 True) %% dc i m'
       | (ValE (Var i)) <- fromA e1, (ValE (DoubleV _))             <- fromA e2 = IM.insert i (CKnown e2 True) %% dc i m'
       -- start experimental
       | (ValE (Var i)) <- fromA e1, (ValE (Var j))                 <- fromA e2,
         Nothing <- vfact j                                                 = IM.insert i (CKnown e2 True) %% dc i m'
       -- end experimental
       | (ValE (Var i)) <- fromA e1, (ValE (Var j))                 <- fromA e2,
         Just c@(CKnown _ b) <- vfact j , (usedOnce nid i || b)             = IM.insert i c {- (CKnown e2 b) -} %% dc i m'
       | (ValE (Var i)) <- fromA e1, (ApplE (ValE (Var fun)) args)  <- fromA e2, isKnownPureFun c fun
           && usedOnce nid i && i `IS.notMember` (IS.unions $ map exprDeps' args) = IM.insert i (CKnown e2 False) %% dc i m'
       | (ValE (Var i)) <- fromA e1, not (mightHaveSideeffects' e2)
           && usedOnce nid i && i `IS.notMember` exprDeps e2                    = IM.insert i (CKnown e2 False) %% dc i m'
       | (ValE (Var i)) <- fromA e1, (SelE (ValE (Var _)) _field) <- fromA e2,
           not (mightHaveSideeffects' e2) && usedOnce nid i                     = IM.insert i (CKnown e1 False) %% dc i m'
       | (ValE (Var i)) <- fromA e1                                             = dc i m'
--       | (IdxE (ValE (Var i)) _) <- fromA e1                                    = dc i m'
--       | (SelE (ValE (Var i)) _) <- fromA e1                                    = dc i m'
       | otherwise = removeMutated e1 m'
       where m' = removeMutated e2 m
             vfact v = case m' of
                         CUnreached -> Nothing
                         CReached x -> IM.lookup v x
    tup x = (x,x)
    removeMutated1t _ s    = tup . removeMutated s
    removeMutated1 _       = removeMutated
    removeMutated :: Data a => a -> CVals -> CVals
    removeMutated s = maybe (const $ CReached IM.empty) deleteConstants mut
      where
        mut = IS.unions <$> sequence (Just IS.empty : map (mutated c) (s ^.. tinplate))

    dc :: Id -> CVals -> CVals
    dc i m = deleteConstants (IS.singleton i) m

    -- propagate constants if we have the same from both branches
    combineConstants :: CVals -> CVals -> CVals
    combineConstants (CReached m1) (CReached m2) =
      CReached (IM.mergeWithKey f (const IM.empty) (const IM.empty) m1 m2)
        where
          f _ (CKnown x dx)  (CKnown y dy) | x == y    = Just (CKnown x (dx || dy))
                  | otherwise = Nothing
    combineConstants CUnreached x = x
    combineConstants x          _ = x


-- all variables referenced by expression
exprDeps :: AExpr' -> IdSet
exprDeps = aIdents . getAnnot

exprDeps' :: Expr -> IdSet
exprDeps' e = exprIdents e

deleteConstants :: IdSet -> CVals -> CVals
deleteConstants s (CReached m) = CReached (IM.filterWithKey f m)
  where
    f :: Id -> CFact -> Bool
    f k _             | k `IS.member` s                                 = False
    f _ (CKnown ae _) | not . IS.null $ exprDeps ae `IS.intersection` s = False
    f _ _                                                               = True
deleteConstants _ CUnreached = CUnreached

-- returns Nothing if it does things that we aren't sure of
mutated :: Cache -> Expr -> Maybe IdSet
mutated c expr = foldl' f (Just IS.empty) (universeOf tinplate expr)
   where
    f :: Maybe IdSet -> Expr -> Maybe IdSet
    f Nothing _                    = Nothing
    f s (UOpE op (ValE (Var i)))
      | op `S.member` uOpMutated   = IS.insert i <$> s
      | otherwise                  = s
    f s (ApplE (ValE (Var i)) _)
      | Just m <- knownMutated i   = IS.union m <$> s
    -- f s (SelE {})                  = s -- fixme might not be true with properties
    -- f s (BOpE{})                   = s
    -- f s (IdxE{})                   = s
    f s ValE{}                     = s
    f _ _                          = Nothing
    knownMutated i
      | isKnownPureFun c i         = Just IS.empty
      | Just s <- IM.lookup i (cachedKnownFuns c) = Just s
    knownMutated _ = Nothing
    uOpMutated = S.fromList [PreInc, PostInc, PreDec, PostDec]

mutated' :: Data a => Cache -> a -> Maybe IdSet
mutated' c a = IS.unions <$> (mapM (mutated c) $ a ^.. tinplate)

-- common RTS functions that we know touch only a few global variables
knownFuns :: Graph a -> IntMap IdSet
knownFuns g = IM.fromList . mapMaybe f $
               [ ("h$bh", [ "h$r1" ])
               ] ++ map (\n -> ("h$p"  <> T.pack (show n), ["h$stack", "h$sp"])) [(1::Int)..32]
                 ++ map (\n -> ("h$pp" <> T.pack (show n), ["h$stack", "h$sp"])) [(1::Int)..255]
  where
    f (fun, vals) = fmap (,IS.fromList [x | Just x <- map (lookupId g . TxtI) vals]) (lookupId g (TxtI fun))

-- pure RTS functions that we know we can safely move around or remove
knownPureFuns :: Graph a -> IdSet
knownPureFuns g = IS.fromList $ mapMaybe (lookupId g) knownPure
  where
    knownPure = map (TxtI . T.pack . ("h$"++)) $
          ("c" : map (('c':).show) [(1::Int)..32]) ++
          map (('d':).show) [(1::Int)..32] ++
          [ "tagToEnum"
          , "mulInt32"
          , "mulWord32", "quotWord32", "remWord32"
          , "popCnt32", "popCnt64"
          , "bswap64"
          , "newByteArray", "newArray"
          , "hs_eqWord64", "hs_neWord64", "hs_leWord64", "hs_ltWord64", "hs_geWord64", "hs_gtWord64"
          , "hs_eqInt64",  "hs_neInt64",  "hs_leInt64",  "hs_ltInt64",  "hs_geInt64",  "hs_gtInt64"
          , "hs_word64ToWord", "hs_int64ToInt"
          ]

isKnownPureFun :: Cache -> Id -> Bool
isKnownPureFun c i = IS.member i (cachedKnownPure c)

mightHaveSideeffects' :: AExpr' -> Bool
mightHaveSideeffects' = aSideEffects . getAnnot

mightHaveSideeffects :: Cache -> Expr -> Bool
mightHaveSideeffects c e = any f (universeOf tinplate e)
  where
    f ValE{}            = False
    f SelE{}            = False  -- might be wrong with defineProperty
    f IdxE{}            = False
    f BOpE{}            = False
    f (UOpE op _ )  = op `S.member` sideEffectOps
    f CondE{}           = False
    f (ApplE (ValE (Var i)) _) | isKnownPureFun c i = False
    f _                 = True
    sideEffectOps = S.fromList [DeleteOp, NewOp, PreInc, PostInc, PreDec, PostDec]

-- constant folding and other bottom up rewrites
foldExpr :: Cache -> Expr -> Expr
foldExpr _c = transformOf tinplate f
  where
    f (BOpE LOrOp v1 v2) = lorOp v1 v2
    f (BOpE LAndOp v1 v2) = landOp v1 v2
    f (BOpE op (ValE (IntV i1)) (ValE (IntV i2)))       = intInfixOp op i1 i2
    f (BOpE op (ValE (DoubleV d1)) (ValE (DoubleV d2))) = doubleInfixOp op d1 d2
    f (BOpE op (ValE (IntV x)) (ValE (DoubleV d)))
      | abs x < 10000 = doubleInfixOp op (fromIntegral x) d -- conservative estimate to prevent loss of precision?
    f (BOpE op (ValE (DoubleV d)) (ValE (IntV x)))
      | abs x < 10000 = doubleInfixOp op d (fromIntegral x)
    f (BOpE AddOp (ValE (StrV s1)) (ValE (StrV s2))) = ValE (StrV (s1<>s2))
    f (BOpE AddOp e (ValE (IntV i))) | i < 0 = BOpE SubOp e (ValE (IntV $ negate i))
    f (BOpE SubOp e (ValE (IntV i))) | i < 0 = BOpE AddOp e (ValE (IntV $ negate i))
    f (UOpE NegOp (ValE (IntV i))) = ValE (IntV $ negate i)
    f (UOpE NegOp (ValE (DoubleV i))) = ValE (DoubleV $ negate i)
    f (UOpE NotOp e) | Just b <- exprCond e = eBool (not b)
    -- y ? a : b === a  -> y == true
    f (BOpE eqOp (CondE c (ValE (IntV v1)) (ValE (IntV v2))) (ValE (IntV v3)))
       | isEqOp eqOp = f (constIfEq eqOp c v1 v2 v3)
    f (BOpE eqOp (ValE (IntV v3)) (CondE c (ValE (IntV v1)) (ValE (IntV v2))))
       | isEqOp eqOp = f (constIfEq eqOp c v1 v2 v3)
    f (BOpE eqOp (BOpE relOp x y) (ValE (BoolV b))) | isEqOp eqOp && isBoolOp relOp = f (boolRelEq eqOp relOp x y b)
    f (BOpE eqOp (ValE (BoolV b)) (BOpE relOp x y)) | isEqOp eqOp && isBoolOp relOp = f (boolRelEq eqOp relOp x y b)
    f (UOpE NotOp (BOpE eqOp x y)) | isEqOp eqOp = BOpE (negateEqOp eqOp) x y
    f e = e

-- v3 `op` (c ? v1 : v2)
constIfEq :: JOp -> Expr -> Integer -> Integer -> Integer -> Expr
constIfEq op c v1 v2 v3
  | not (neqOp || eqOp) = error ("constIfEq: not an equality operator: " ++ show op)
  | v1 == v2  = ValE (BoolV $ eqOp == (v1 == v3))
  | v3 == v1  = if eqOp then BOpE EqOp c (ValE (BoolV True)) else UOpE NotOp c
  | v3 == v2  = if eqOp then UOpE NotOp c else BOpE EqOp c (ValE (BoolV True))
  | otherwise = ValE (BoolV False)
  where
    neqOp = op == NeqOp || op == StrictNeqOp
    eqOp = op == EqOp || op == StrictEqOp

-- only use if rel always produces a bool result
boolRelEq :: JOp -> JOp -> Expr -> Expr -> Bool -> Expr
boolRelEq eq rel x y b | eq == EqOp  || eq == StrictEqOp  =
  if b     then BOpE rel x y else UOpE NotOp (BOpE rel x y)
boolRelEq eq rel x y b | eq == NeqOp || eq == StrictNeqOp =
  if not b then BOpE rel x y else UOpE NotOp (BOpE rel x y)
boolRelEq op _   _ _ _ = error ("boolRelEq: not an equality operator: " ++ show op)

negateEqOp :: JOp -> JOp
negateEqOp EqOp        = NeqOp
negateEqOp NeqOp       = EqOp
negateEqOp StrictEqOp  = StrictNeqOp
negateEqOp StrictNeqOp = StrictEqOp
negateEqOp op          = error ("negateEqOp: not an equality operator: " ++ show op)

-- is expression a numeric or string constant?
isConst :: Expr -> Bool
isConst (ValE (DoubleV _)) = True
isConst (ValE (IntV _)) = True
isConst (ValE (StrV _)) = True
isConst _ = False

isVar :: Expr -> Bool
isVar (ValE (Var _)) = True
isVar _ = False

isConstOrVar :: Expr -> Bool
isConstOrVar x = isConst x || isVar x

normalize :: Bool -> Cache -> Graph' -> Expr -> Expr
normalize True = normalizeCond
normalize _    = normalizeReg

-- somewhat stronger normalize for expressions in conditions where the
-- top level is forced to be bool
-- if(x == true) -> if(x)
-- if(c ? 1 : 0) -> if(c)
normalizeCond :: Cache -> Graph' -> Expr -> Expr
normalizeCond c g e = go0 e
  where
    go0 e = go (normalizeReg c g e)
    go e
      | Just b <- exprCond e = ValE (BoolV b)
    go (BOpE op (ValE (BoolV b)) e)
      | (op == EqOp && b) || (op == NeqOp && not b) = go0 e
    go (BOpE op e (ValE (BoolV b)))
      | (op == EqOp && b) || (op == NeqOp && not b) = go0 e
    go (CondE c e1 e2)
      | Just True <- exprCond e1, Just False <- exprCond e2 = go0 c
    go e = e

normalizeReg :: Cache -> Graph' -> Expr -> Expr
normalizeReg cache g = rewriteOf tinplate assoc . rewriteOf tinplate comm . foldExpr cache
  where
    comm :: Expr -> Maybe Expr
    comm e | not (allowed e) = Nothing
    comm (BOpE op e1 e2)
      |  commutes op && isConst e1 && not (isConst e2)
      || commutes op && isVar e1 && not (isConstOrVar e2) = f (BOpE op e2 e1)
    comm (BOpE op1 e1 (BOpE op2 e2 e3))
      |   op1 == op2 && commutes op1 && isConst e1 && not (isConst e2)
      ||  op1 == op2 && commutes op1 && isVar e1 && not (isConstOrVar e2)
      || commutes2a op1 op2 && isConst e1 && not (isConst e2)
      || commutes2a op1 op2 && isVar e1 && not (isConstOrVar e2)
          = f (BOpE op1 e2 (BOpE op2 e1 e3))
    comm _ = Nothing
    assoc :: Expr -> Maybe Expr
    assoc e | not (allowed e) = Nothing
    assoc (BOpE op1 (BOpE op2 e1 e2) e3)
      | op1 == op2 && associates op1 = f (BOpE op1 e1 (cf $ BOpE op1 e2 e3))
      | associates2a op1 op2 = f (BOpE op2 e1 (cf $ BOpE op1 e2 e3))
      | associates2b op1 op2 = f (BOpE op1 e1 (cf $ BOpE op2 e3 e2))
    assoc _ = Nothing
    commutes   op  = op `elem` [AddOp] -- ["*", "+"]                            --  a * b       = b * a
    commutes2a op1 op2 = (op1,op2) `elem` [(AddOp, SubOp)] -- ,("*", "/")]     --  a + (b - c) = b + (a - c)
    associates op  = op `elem` [AddOp] -- ["*", "+"]                            -- (a * b) * cv = a * (b * c)
    associates2a op1 op2 = (op1,op2) `elem` [(SubOp, AddOp)] -- , ("/", "*")] -- (a + b) - c  = a + (b - c)  -- (a*b)/c = a*(b/c)
    associates2b op1 op2 = (op1,op2) `elem` [(AddOp, SubOp)] -- , ("*", "/")] -- (a - b) + c  = a + (c - b)
    cf = rewriteOf tinplate comm . foldExpr cache
    f  = Just . foldExpr cache
    allowed e
        | IdxE (ValE (Var st)) e' <- e, Just st == stack = allowed' e'
        | otherwise                                      = allowed' e
       where
         allowed' e = IS.null (exprDeps' e IS.\\ x) && all isSmall (e ^.. tinplate)
         stack = lookupId g (TxtI "h$stack")
         sp    = lookupId g (TxtI "h$sp")
         x = IS.fromList $ catMaybes [sp]
         isSmall (DoubleV (SaneDouble d)) = abs d < 10000
         isSmall (IntV x)                 = abs x < 10000
         isSmall (StrV _)                 = False
         isSmall _                        = True

lorOp :: Expr -> Expr -> Expr
lorOp e1 e2 | Just b <- exprCond e1 = if b then eBool True else e2
lorOp e1 e2 = BOpE LOrOp e1 e2

landOp :: Expr -> Expr -> Expr
landOp e1 e2 | Just b <- exprCond e1 = if b then e2 else eBool False
landOp e1 e2 = BOpE LAndOp e1 e2

-- if expression is used in a condition, can we statically determine its result?
exprCond :: Expr -> Maybe Bool
exprCond (ValE (IntV x))    = Just (x /= 0)
exprCond (ValE (DoubleV x)) = Just (x /= 0)
exprCond (ValE (StrV xs))   = Just (not $ T.null xs)
exprCond (ValE (BoolV b))   = Just b
exprCond (ValE NullV)       = Just False
exprCond (ValE UndefinedV)  = Just False
exprCond _                  = Nothing

exprCond' :: AExpr a -> Maybe Bool
exprCond' = exprCond . fromA

-- constant folding and other bottom up rewrites
intInfixOp :: JOp -> Integer -> Integer -> Expr
intInfixOp AddOp         i1 i2 = ValE (IntV (i1+i2))
intInfixOp SubOp         i1 i2 = ValE (IntV (i1-i2))
intInfixOp MulOp         i1 i2 = ValE (IntV (i1*i2))
intInfixOp DivOp         i1 i2
  | i2 /= 0 && d * i2 == i1 = ValE (IntV d)
  where
    d = i1 `div` i2
intInfixOp ModOp         i1 i2 = ValE (IntV (i1 `mod` i2)) -- not rem?
intInfixOp LeftShiftOp   i1 i2 = bitwiseInt' Bits.shiftL i1 (i2 Bits..&. 31)
intInfixOp RightShiftOp  i1 i2 = bitwiseInt' Bits.shiftR i1 (i2 Bits..&. 31)
intInfixOp ZRightShiftOp i1 i2 = bitwiseWord Bits.shiftR i1 (i2 Bits..&. 31)
intInfixOp BAndOp        i1 i2 = bitwiseInt (Bits..&.) i1 i2
intInfixOp BOrOp         i1 i2 = bitwiseInt (Bits..|.) i1 i2
intInfixOp BXorOp        i1 i2 = bitwiseInt Bits.xor   i1 i2
intInfixOp GtOp          i1 i2 = eBool (i1 > i2)
intInfixOp LtOp          i1 i2 = eBool (i1 < i2)
intInfixOp LeOp          i1 i2 = eBool (i1 <= i2)
intInfixOp GeOp          i1 i2 = eBool (i1 >= i2)
intInfixOp StrictEqOp    i1 i2 = eBool (i1 == i2)
intInfixOp StrictNeqOp   i1 i2 = eBool (i1 /= i2)
intInfixOp EqOp          i1 i2 = eBool (i1 == i2)
intInfixOp NeqOp         i1 i2 = eBool (i1 /= i2)
intInfixOp op            i1 i2 = BOpE op (ValE (IntV i1)) (ValE (IntV i2))

doubleInfixOp :: JOp -> SaneDouble -> SaneDouble -> Expr
doubleInfixOp AddOp (SaneDouble d1) (SaneDouble d2) = ValE (DoubleV $ SaneDouble (d1+d2))
doubleInfixOp SubOp (SaneDouble d1) (SaneDouble d2) = ValE (DoubleV $ SaneDouble (d1-d2))
doubleInfixOp MulOp (SaneDouble d1) (SaneDouble d2) = ValE (DoubleV $ SaneDouble (d1*d2))
doubleInfixOp DivOp (SaneDouble d1) (SaneDouble d2) = ValE (DoubleV $ SaneDouble (d1/d2))
doubleInfixOp GtOp  (SaneDouble d1) (SaneDouble d2) = eBool (d1 > d2)
doubleInfixOp LtOp  (SaneDouble d1) (SaneDouble d2) = eBool (d1 < d2)
doubleInfixOp LeOp  (SaneDouble d1) (SaneDouble d2) = eBool (d1 <= d2)
doubleInfixOp GeOp  (SaneDouble d1) (SaneDouble d2) = eBool (d1 >= d2)
doubleInfixOp StrictEqOp (SaneDouble d1) (SaneDouble d2) = eBool (d1 == d2)
doubleInfixOp StrictNeqOp (SaneDouble d1) (SaneDouble d2) = eBool (d1 /= d2)
doubleInfixOp EqOp  (SaneDouble d1) (SaneDouble d2) = eBool (d1 == d2)
doubleInfixOp NeqOp  (SaneDouble d1) (SaneDouble d2) = eBool (d1 /= d2)
doubleInfixOp op d1 d2 = BOpE op (ValE (DoubleV d1)) (ValE (DoubleV d2))

bitwiseInt :: (Int32 -> Int32 -> Int32) -> Integer -> Integer -> Expr
bitwiseInt op i1 i2 =
  let i = fromIntegral i1 `op` fromIntegral i2
  in ValE (IntV $ fromIntegral i)

bitwiseInt' :: (Int32 -> Int -> Int32) -> Integer -> Integer -> Expr
bitwiseInt' op i1 i2 =
  let i = fromIntegral i1 `op` fromIntegral i2
  in ValE (IntV $ fromIntegral i)

bitwiseWord :: (Word32 -> Int -> Word32) -> Integer -> Integer -> Expr
bitwiseWord op i1 i2 =
  let i = fromIntegral i1 `op` fromIntegral i2
      i' :: Int32
      i' = fromIntegral i
  in ValE (IntV $ fromIntegral i')

-----------------------------------------------------------
-- stack and stack pointer magic
-----------------------------------------------------------

data StackFacts = SFUnknownB
                | SFOffset Integer
                | SFUnknownT
  deriving (Eq, Ord, Show, Data, Typeable)

sfKnown :: StackFacts -> Bool
sfKnown (SFOffset _) = True
sfKnown _            = False

sfUnknown :: StackFacts -> Bool
sfUnknown = not . sfKnown

propagateStack :: IdSet -> IdSet -> Cache -> Graph' -> Graph'
propagateStack _args _locals c g
  | nrets > 4 || stackAccess < nrets - 1 || stackAccess == 0 = g
  | otherwise                                                = g'
  where
    stackI = fromMaybe (-1) (lookupId g (TxtI "h$stack"))
    spI    = fromMaybe (-2) (lookupId g (TxtI "h$sp"))
    z      = SFOffset 0

    allNodes = IM.elems (g ^. nodes)
    nrets = length [() | ReturnNode{} <- allNodes]
    stackAccess = length $ filter (stackI `IS.member`) (map exprDeps (allNodes ^.. tinplate))

    sf = stackFacts c g
    g' :: Graph'
    g' = normalizeAnnot c
       $ foldl' (\g (k,v) -> updNode k v g) g (IM.toList $ g^.nodes)

    updNode :: NodeId -> Node' -> Graph' -> Graph'
    updNode nid e@(IfNode _ n1 _) g
      | known nid && unknown n1 = replace nid e g
    updNode nid e@(WhileNode _ n1) g
      | known nid && unknown n1 = replace nid e g
    updNode nid e@(ForInNode _ _ _ n1) g
      | known nid && unknown n1 = replace nid e g
    updNode nid e@(SwitchNode _ _ n1) g
      | known nid && unknown n1 = replace nid e g
    updNode nid e@(TryNode _ _ _ _) g
      | known nid = replace nid e g
    updNode nid e@ReturnNode{} g
      | known nid = replace nid e g
    updNode nid e@SimpleNode{} g
      | known nid && sfUnknown (lookupFact SFUnknownB nid 1 sf) = replace nid e g -- fixme is this right?
    updNode nid   SimpleNode{} g
      | SFOffset x <- lookupFact SFUnknownB nid 0 sf,
        SFOffset y <- lookupFact SFUnknownB nid 1 sf,
         x /= y = nodes %~ IM.insert nid (SequenceNode []) $ g
    updNode nid n g = updExprs nid n g

    updExprs :: NodeId -> Node' -> Graph' -> Graph'
    updExprs nid n g
      | SFOffset offset <- lookupFact z nid 2 sf =
         let n' =  (tinplate %~ updExpr offset) n
         in  nodes %~ IM.insert nid n' $ g
      | otherwise = g

    updExpr :: Integer -> Expr -> Expr
    updExpr 0 e = e
    updExpr n e = transformOf tinplate sp e
      where
        sp e@(ValE (Var i)) | i == spI = BOpE AddOp e (ValE (IntV n))
        sp e = e

    known :: NodeId -> Bool
    known nid = sfKnown (lookupFact SFUnknownB nid 0 sf)

    unknown :: NodeId -> Bool
    unknown = not . known

    replace :: NodeId -> Node' -> Graph' -> Graph'
    replace nid n g
      | SFOffset x <- lookupFact SFUnknownB nid 0 sf, x /= 0 =
          let newId = g^.nodeid
              sp = ValE (Var spI)
              newNodes = IM.fromList
                [ (nid,     SequenceNode [newId, newId+1])
                , (newId,   SimpleNode (AssignS (annotate c sp) (annotate c . normalize False c g $ BOpE AddOp sp (ValE (IntV x)))))
                , (newId+1, n)
                ]
          in  (nodeid %~ (+2)) . (nodes %~ IM.union newNodes) $ g
      | otherwise = g

stackFacts :: Cache -> Graph' -> Facts StackFacts
stackFacts c g = foldForward combineStack f0 (SFOffset 0) SFUnknownB g
  where
    -- stackI = fromMaybe (-1) (lookupId g (TxtI "h$stack"))
    spI    = fromMaybe (-2) (lookupId g (TxtI "h$sp"))
    f0 = def { fIf      = updSfT
             , fWhile   = updSfT
             , fDoWhile = updSfT
             , fSwitch  = switched
             , fReturn  = updSf1
             , fForIn   = \_ _ -> updSfT
             , fSimple  = simple
             , fTry     = \_ _ -> (SFUnknownT, SFUnknownT, SFUnknownT)
             }
    updSfT _ e sf = tup (updSf e sf)
    updSf1 _ = updSf
    updSf :: AExpr' -> StackFacts -> StackFacts
    updSf _ SFUnknownT = SFUnknownT
    updSf e sf | Just m <- mutated c (fromA e), spI `IS.notMember` m = sf
               | otherwise                                           = SFUnknownT

    updSf' :: SimpleStat' -> StackFacts -> StackFacts
    updSf' _ SFUnknownT = SFUnknownT
    updSf' DeclS{} sf = sf
    updSf' (ExprS e) sf
      | Just s <- mutated c (fromA e),  spI `IS.notMember` s = sf
    updSf' (AssignS e1 e2) sf
      | Just s1 <- mutated c (fromA e1), Just s2 <- mutated c (fromA e2),
        spI `IS.notMember` s1 && spI `IS.notMember` s2 = sf
    updSf' _ _ = SFUnknownT

    switched :: NodeId -> AExpr' -> [AExpr'] -> StackFacts -> ([StackFacts], StackFacts)
    switched _ e es sf = let sf' = updSf e sf in (replicate (length es) sf', sf')

    adjSf :: Integer -> StackFacts -> StackFacts
    adjSf n (SFOffset x) = SFOffset (x+n)
    adjSf _ _            = SFUnknownT

    adjSfE :: AExpr' -> StackFacts -> StackFacts
    adjSfE e sf
      | (ValE (Var i)) <- fromA e, i == spI = sf
      | (BOpE op (ValE (Var i)) (ValE (IntV x))) <- fromA e, i == spI =
                   case op of
                     AddOp -> adjSf x sf
                     SubOp -> adjSf (-x) sf
                     _     -> SFUnknownT
                | otherwise = SFUnknownT

    simple :: NodeId -> SimpleStat' -> StackFacts -> StackFacts
    simple _nid (ExprS (AExpr _ (UOpE op (ValE (Var i)))))
      | i == spI && op == PreInc                                = adjSf 1
      | i == spI && op == PreDec                                = adjSf (-1)
      | i == spI && op == PostInc                               = adjSf 1
      | i == spI && op == PostDec                               = adjSf (-1)
    simple _nid (AssignS (AExpr _ (ValE (Var i))) e) | i == spI = adjSfE e
    simple _nid s                                               = updSf' s

    combineStack :: StackFacts -> StackFacts -> StackFacts
    combineStack SFUnknownT _ = SFUnknownT
    combineStack _ SFUnknownT = SFUnknownT
    combineStack SFUnknownB x = x
    combineStack x SFUnknownB = x
    combineStack ox@(SFOffset x) (SFOffset y)
      | x == y    = ox
      | otherwise = SFUnknownT

-- apply the rewriter to the expressions in this node
-- first argument is true when the expression is used in a boolean condition
rewriteNodeExprs :: (Bool -> AExpr' -> AExpr') -> Node' -> Node'
rewriteNodeExprs f (SimpleNode s)       = SimpleNode (rewriteSimpleStatExprs (f False) s)
rewriteNodeExprs f (IfNode e s1 s2)     = IfNode (f True e) s1 s2
rewriteNodeExprs f (WhileNode e i)      = WhileNode (f True e) i
rewriteNodeExprs f (DoWhileNode e i)    = DoWhileNode (f True e) i
rewriteNodeExprs f (ForInNode b i e s ) = ForInNode b i (f False e) s
rewriteNodeExprs f (SwitchNode e cs i)  = SwitchNode (f False e) (cs & traverse . _1 %~ f False) i
rewriteNodeExprs f (ReturnNode e)       = ReturnNode (f False e)
rewriteNodeExprs _ n                    = n -- other nodes don't contain expressions

rewriteSimpleStatExprs :: (AExpr' -> AExpr') -> SimpleStat' -> SimpleStat'
rewriteSimpleStatExprs f (ExprS e)       = ExprS (f e)
rewriteSimpleStatExprs f (AssignS e1 e2) = AssignS (f e1) (f e2)
rewriteSimpleStatExprs _ s               = s


-- optimize sequences of assignments:
-- h$r3 = x
-- h$r2 = y
-- h$r1 = z
-- -> h$l3(x,y,z)
-- (only if y,z don't refer to the new value of h$r3)
optimizeSequences :: Cache -> Graph' -> Graph'
optimizeSequences c g0 = foldl' transformNodes g (IM.elems $ g ^. nodes)
  where
    (ls, g1) = addIdents (map (assignRegs'!) [1..32]) g0
    g = flattenSequences g1

    transformNodes :: Graph' -> Node' -> Graph'
    transformNodes g' (SequenceNode xs) =
      let xs' = f (map (\nid -> fromMaybe (error $ "optimizeSequences, invalid node: " ++ show nid)
                                (IM.lookup nid $ g ^. nodes)) xs)
      in  foldr (\(nid,n) -> nodes %~ IM.insert nid n) g' (zip xs xs')
    transformNodes g' _ = g'

    regN :: IntMap Int
    regN = IM.fromList . catMaybes $
       map (\r -> (,1 + fromEnum r) <$> lookupId g (registersI!r)) (enumFromTo R1 R32)
    reg x = IM.lookup x regN
    regBetween :: Int -> Int -> Int -> Bool
    regBetween x y r = maybe False (\n -> n >= x && n <= y) (IM.lookup r regN)

    f :: [Node'] -> [Node']
    f (x@(SimpleNode (AssignS (AExpr _ (ValE (Var r))) e)):xs)
      | Just n <- reg r, n > 1 && not (mightHaveSideeffects' e) = f' (n-1) n [(fromA e,x)] xs
    f (x:xs) = x : f xs
    f [] = []

    f' :: Int -> Int -> [(Expr, Node')] -> [Node'] -> [Node']
    f' 0 start exprs xs = mkAssign exprs : replicate (start-1) (SequenceNode []) ++ f xs
    f' n start exprs (x@(SimpleNode (AssignS ae@(AExpr _ (ValE (Var r))) e)):xs)
       |  Just k <- reg r, k == n && not (mightHaveSideeffects' e)
              && not (F.any (regBetween (k+1) start) (IS.toList $ exprDeps ae))
           = f' (n-1) start ((fromA e,x):exprs) xs
    f' _ _start exprs xs = map snd (reverse exprs) ++ f xs

    mkAssign :: [(Expr, Node')] -> Node'
    mkAssign xs = SimpleNode (ExprS . annotate c $ ApplE (ValE (Var i)) (reverse $ map fst xs))
      where i = ls !! (length xs - 1)

flattenSequences :: Graph' -> Graph'
flattenSequences g = g & nodes %~ (\n -> foldl' flatten n (IM.toList n))
  where
    flatten :: IntMap Node' -> (NodeId, Node') -> IntMap Node'
    flatten ns (nid, SequenceNode xs) =
      case IM.lookup nid ns of
        Nothing -> ns -- already removed
        Just _  -> let (xs', ns') = foldl' f ([], ns) xs
                   in  IM.insert nid (SequenceNode $ reverse xs') ns'
    flatten ns _ = ns
    f :: ([NodeId], IntMap Node') -> NodeId -> ([NodeId], IntMap Node')
    f (xs, ns) nid
      | Just (SequenceNode ys) <- IM.lookup nid ns =
           let (xs', ns') = foldl' f (xs, ns) ys
           in  (xs', IM.delete nid ns')
      | otherwise = (nid : xs, ns)


--------------------------------------------------------------------------
-- some debugging things, remove later or add proper reporting mechanism
--------------------------------------------------------------------------
{-
dumpLive :: Graph' -> Facts Liveness -> Facts Liveness
dumpLive g l@(Facts l0) = Debug.Trace.trace (show g ++ "\n" ++ l') l
  where
    l' = concatMap f (M.toList l0)
    f ((nid,0),v) = show nid ++ " -> " ++ showLive g v ++ "\n"
    f _           = ""

dumpFacts :: Graph' -> Facts CVals -> Facts CVals
dumpFacts g c@(Facts c0) = Debug.Trace.trace ({- show g ++ "\n" ++ -} c') c
  where
    c' = concatMap f (M.toList c0)
    f ((nid,0),v) = show nid ++ " -> " ++ showCons g v ++ "\n"
    f _           = ""

showLive :: Graph' -> Liveness -> String
showLive gr (Liveness l le) = "live: " ++ f l ++ " excep: " ++ f le
  where
    f im = L.intercalate ", " $  map g (IM.toList im)
    g (i,u) = show i ++ ":" ++ showId gr i ++ ":" ++ show u

showId :: Graph' -> Id -> String
showId g i = case lookupIdent g i of
                 Nothing        -> "<unknown>"
                 Just (TxtI i') -> T.unpack i'

showCons :: Graph' -> CVals -> String
showCons g CUnreached = "<unreached>"
showCons g (CReached imf) = "\n" ++ (L.unlines $ map (\x -> "   " ++ f x) (IM.toList imf))
  where
    f (i,v) = showId g i ++ ":" ++ show v
-}
