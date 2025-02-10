-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.JS.Opt.Simple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--
-- * Simple optimizer for the JavaScript IR
--
--     This is a simple optimizer for the JavaScript IR. It is intended to be
--     the first optimization pass after generating the JavaScript IR.
--
-- * Design
--
--     The optimizer is invoked on the top-level JStat. It leaves the top-level
--     scope alone, but traverses into each function body and optimizes it.
--     Nested functions are mostly left alone, since they are uncommon in
--     generated code.
--
--     The optimizations are:
--
--       - rename local variables to shorter names
--       - remove unused variables
--       - remove trivial assignments: x = x
--       - "float" expressions without side effects:
--       -   var x = 1; var y = x + 1; -> var y = 1 + 1;
--
-- * Limitations
--
--     The simple optimization pass is intended to be fast and applicable to
--     almost all generated JavaScript code. Limitations are:
--
--       - optimization is disabled if an `eval` statement is encountered
--       - variables declared in nested scopes are not renamed
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
module GHC.JS.Opt.Simple (simpleOpt) where

import GHC.Prelude

import GHC.JS.Opt.Expr
import GHC.JS.Syntax

import GHC.Data.FastString
import qualified GHC.Types.Unique.Map as UM
import GHC.Types.Unique.Map (UniqMap)
import qualified GHC.Types.Unique.Set as US

import Control.Monad
import Data.Function
import Data.List (sortBy)
import Data.Maybe
import qualified Data.Semigroup as Semi


data Multiplicity = Zero | One | Many
    deriving (Eq, Ord, Show)

data VarValue = Unassigned
              | AssignedOnce
              | AssignedOnceKnown !JExpr
              | AssignedMany

data VarDecl = NoDecl -- not declared in analyzed scope (possibly deeper or global)
             | ArgDecl !Int -- argument in analyzed scope
             | LocalDecl !Int -- local variable in analyzed scope
  deriving (Eq, Show)

isLocalOrArg :: VarDecl -> Bool
isLocalOrArg (LocalDecl {}) = True
isLocalOrArg (ArgDecl {})   = True
isLocalOrArg _              = False

isDecl :: VarDecl -> Bool
isDecl NoDecl = False
isDecl _      = True

instance Semi.Semigroup VarDecl where
  NoDecl <> x = x
  x <> NoDecl = x
  ArgDecl n <> ArgDecl m = ArgDecl (min n m)
  LocalDecl n <> LocalDecl m = LocalDecl (min n m)
  ArgDecl n <> _ = ArgDecl n
  _ <> ArgDecl n = ArgDecl n

instance Ord VarDecl where
  compare NoDecl NoDecl = EQ
  compare NoDecl _      = LT
  compare _      NoDecl = GT
  compare (ArgDecl n) (ArgDecl m) = compare n m
  compare (ArgDecl {}) _ = LT
  compare _ (ArgDecl {}) = GT
  compare (LocalDecl n) (LocalDecl m) = compare n m

data JFunction = JFunction [Ident] JStat

instance Semi.Semigroup VarValue where
  Unassigned <> x = x
  x <> Unassigned = x
  _ <> _ = AssignedMany

instance Monoid VarValue where
  mempty = Unassigned
  mappend = (Semi.<>)

instance Semigroup Multiplicity where
    Zero <> x = x
    x <> Zero = x
    _ <> _    = Many

instance Monoid Multiplicity where
    mempty = Zero
    mappend = (Semi.<>)

data VarUsage = VarUsage
    { varUsed :: !Multiplicity
    , varAssigned :: !VarValue
    , varDeclared :: !VarDecl
    , varDeepDeclared :: !Bool
    }

assignedMultiple :: VarUsage -> Bool
assignedMultiple VarUsage { varAssigned = AssignedMany } = True
assignedMultiple _                                       = False

data SimpleRewrite = SimpleRewrite
    { renameVar :: UniqMap Ident Ident
    , varUsage :: UniqMap Ident VarUsage
    }

instance Semigroup VarUsage where
    x <> y = VarUsage
        { varUsed = varUsed x Semi.<> varUsed y
        , varAssigned = varAssigned x Semi.<> varAssigned y
        , varDeclared = varDeclared x Semi.<> varDeclared y
        , varDeepDeclared = varDeepDeclared x || varDeepDeclared y
        }

instance Monoid VarUsage where
    mempty = VarUsage Zero Unassigned NoDecl False

disableOpt :: Bool
-- disableOpt = True
disableOpt = False

simpleOpt :: JStat -> JStat
simpleOpt x | disableOpt = x
simpleOpt (BlockStat xs) = BlockStat (map simpleOpt xs)
simpleOpt (AssignStat lhs AssignOp (ValExpr (JFunc args body)))  =
     let JFunction args' body' = simpleOptFunction (JFunction args body)
     in AssignStat lhs AssignOp (ValExpr (JFunc args' body'))
simpleOpt (FuncStat name args body) =
     let JFunction args' body' = simpleOptFunction (JFunction args body)
     in  FuncStat name args' body'
simpleOpt s = s

simpleOptFunction :: JFunction -> JFunction
simpleOptFunction jf = s_opt
  where
    -- we need to run it twice since floating in the first pass can
    -- cause unused variables that can be removed in the second pass
    s_opt  = functionOptExprs $ maybe jf (`simpleRewrite` s_opt0) mb_rw
    mb_rw  = mkRewrite True (simpleAnalyze s_opt0)
    s_opt0 = functionOptExprs $ maybe jf (`simpleRewrite` jf) mb_rw0
    mb_rw0 = mkRewrite False (simpleAnalyze jf)

functionOptExprs :: JFunction -> JFunction
functionOptExprs (JFunction args s) = JFunction args (optExprs s)

newLocals :: [Ident]
newLocals = filter (not . isReserved  ) $
            map (TxtI . mkFastString) $
            map (:[]) chars0 ++ concatMap mkIdents [1..]
  where
    mkIdents n = [c0:cs | c0 <- chars0, cs <- replicateM n chars]
    chars0 = ['a'..'z']++['A'..'Z']
    chars = chars0++['0'..'9']
    isReserved (TxtI i) = i `US.elementOfUniqSet` reservedSet
    reservedSet = US.mkUniqSet reserved
    reserved = [  -- reserved words
                  "abstract", "arguments", "await", "boolean"
                , "break", "byte", "case", "catch"
                , "char", "class", "const", "continue"
                , "debugger", "default", "delete", "do"
                , "double", "else", "enum", "eval"
                , "export", "extends", "false", "final"
                , "finally", "float", "for", "function"
                , "goto", "if", "implements", "import"
                , "in", "instanceof", "int", "interface"
                , "let", "long", "native", "new"
                , "null", "package", "private", "protected"
                , "public", "return", "short", "static"
                , "super", "switch", "synchronized", "this"
                , "throw", "throws", "transient", "true"
                , "try", "typeof", "var", "void"
                , "volatile", "while", "with", "yield"
                -- some special values
                , "as", "async", "from", "get"
                , "of", "NaN", "prototype", "undefined"
                ]

mkRewrite :: Bool -> AnalysisResult -> Maybe SimpleRewrite
mkRewrite do_rename a
  | arBailout a = Nothing
  | otherwise  = Just $
  SimpleRewrite { renameVar = if do_rename
                              then UM.listToUniqMap (zip localVars newVars)
                              else UM.emptyUniqMap
                , varUsage = vu
                }
  where
    vu :: UM.UniqMap Ident VarUsage
    vu = arVarUsage a

    -- local variables in the order that they were declared
    localVars :: [Ident]
    localVars =
        map fst
         -- recover original order and remove non-determinism
      . sortBy (compare `on` snd)
      . map (\(v, u) -> (v, varDeclared u))
      . filter (isDecl . varDeclared . snd)
      -- non-determinism is removed by sorting afterwards
      $ UM.nonDetUniqMapToList vu
    -- we can't rename variables that are used in the global scope
    blockedNames :: US.UniqSet Ident
    blockedNames =
      US.mkUniqSet $
      map fst (
      filter (\(_k,v) -> (not . isDecl) (varDeclared v) || varDeepDeclared v)
             (UM.nonDetUniqMapToList vu))


    newVars :: [Ident]
    newVars = filter (not . (`US.elementOfUniqSet` blockedNames)) newLocals

simpleRewrite :: SimpleRewrite -> JFunction -> JFunction
simpleRewrite rw (JFunction args stat)= JFunction (map varReplace args) (go stat)
  where
    zeroUsed :: JExpr -> Bool
    zeroUsed (ValExpr (JVar v)) =
      maybe True ((== Zero) . varUsed) (UM.lookupUniqMap (varUsage rw) v) &&
      maybe False (isDecl . varDeclared) (UM.lookupUniqMap (varUsage rw) v)
    zeroUsed _                  = False

    varReplace :: Ident -> Ident
    varReplace v = fromMaybe v (UM.lookupUniqMap (renameVar rw) v)

    {-
      We can sometimes float down an expression to avoid an assignment:

      var x = e;
      f(x);
        ==>
      f(e);

      This can only be done if the expression has no side effects and x is
      only used once.

      Heap object property lookups cannot be floated just yet, since we
      don't know whether an object is mutable or not. For example a thunk
      can be blackholed, which would change the result if we float the lookup
      after the blackholing.
     -}

    mayBeFloated :: JExpr -> Bool
    mayBeFloated (ValExpr v) = mayBeFloatedV v
    mayBeFloated (SelExpr _e _) = False
    mayBeFloated (IdxExpr _e1 _e2) = False
    mayBeFloated (InfixExpr _ e1 e2)= mayBeFloated e1 && mayBeFloated e2
    mayBeFloated (UOpExpr _ _e) = False
    mayBeFloated (IfExpr e1 e2 e3) = mayBeFloated e1 &&
                                     mayBeFloated e2 &&
                                     mayBeFloated e3
    mayBeFloated (ApplExpr e es)
      | ValExpr (JVar (TxtI i)) <- e, isClosureAllocator i = all mayBeFloated es
      | otherwise                                          = False

    mayBeFloatedV :: JVal -> Bool
    mayBeFloatedV (JVar v)
      | Just vu <- UM.lookupUniqMap (varUsage rw) v
      = isDecl (varDeclared vu) && not (assignedMultiple vu)
      | otherwise = False
    mayBeFloatedV (JList es) = all mayBeFloated es
    mayBeFloatedV (JDouble {}) = True
    mayBeFloatedV (JInt {}) = True
    mayBeFloatedV (JStr {}) = True
    mayBeFloatedV (JRegEx {}) = True
    mayBeFloatedV (JBool {}) = True
    mayBeFloatedV (JHash ps) = all (mayBeFloated . snd)
                                   (UM.nonDetUniqMapToList ps)
    mayBeFloatedV (JFunc {}) = False

    {-
       we allow small literals and local variables and arguments to be
       duplicated, since they tend to take up little space.
      -}
    mayDuplicate :: JExpr -> Bool
    mayDuplicate (ValExpr (JVar i))
      | Just vu <- (UM.lookupUniqMap (varUsage rw) i)
      = isLocalOrArg (varDeclared vu)
    mayDuplicate (ValExpr (JInt n))     = abs n < 1000000
    mayDuplicate (ValExpr (JDouble {})) = True
    mayDuplicate _                      = False

    zeroAssigned :: Ident -> Bool
    zeroAssigned v
       | Just vu <- UM.lookupUniqMap (varUsage rw) v
       = case varAssigned vu of
          Unassigned -> True
          _          -> False
      | otherwise = False

    assignedAtMostOnce :: Ident -> Bool
    assignedAtMostOnce v
      | Just vu <- UM.lookupUniqMap (varUsage rw) v =
        case varAssigned vu of
          Unassigned           -> True
          AssignedOnce         -> True
          AssignedOnceKnown {} -> True
          AssignedMany         -> False
      | otherwise = False

    go :: JStat -> JStat
    go (DeclStat v mb_e)
        | zeroUsed (ValExpr (JVar v)) =
          case mb_e of
            Nothing | zeroAssigned v -> BlockStat []
                    | otherwise      -> DeclStat (varReplace v) Nothing
            Just e | not (mayHaveSideEffects e) && assignedAtMostOnce v
                                                -> BlockStat []
                   | otherwise                  -> DeclStat (varReplace v) (Just (goE True e))
        | otherwise = DeclStat (varReplace v) (fmap (goE True) mb_e)
    go (AssignStat lhs aop e)
        | ValExpr (JVar i) <- lhs, isTrivialAssignment i aop e = BlockStat []
        | zeroUsed lhs && not (mayHaveSideEffects e) = BlockStat []
        | zeroUsed lhs = AssignStat (goE False lhs) aop (goE True e)
        | otherwise = AssignStat (goE False lhs) aop (goE True e)
    go (ReturnStat e) = ReturnStat (goE True e)
    go (BlockStat ss) = flattenBlock (map go ss)
    go (IfStat e s1 s2) = IfStat (goE True e) (go s1) (go s2)
    go (WhileStat b e s) = WhileStat b (goE True e) (go s)
    go (ForStat s1 e s2 s3) = ForStat (go s1) (goE True e) (go s2) (go s3)
    go (ForInStat b v e s) = ForInStat b (varReplace v) (goE True e) (go s)
    go (SwitchStat e cases s) = SwitchStat (goE True e)
                                           (map (\(c,cs) -> (c, go cs)) cases)
                                           (go s)
    go (TryStat s1 v s2 s3) = TryStat (go s1) (varReplace v) (go s2) (go s3)
    go (ApplStat e es) = ApplStat (goE True e) (map (goE True) es)
    go (UOpStat uop e) = UOpStat uop (goE False e)
    go (LabelStat lbl s) = LabelStat lbl (go s)
    go s@(BreakStat {}) = s
    go s@(ContinueStat {}) = s
    go (FuncStat i args s) = FuncStat i (map varReplace args) (go s)

    goE :: Bool -> JExpr -> JExpr
    goE rhs (ValExpr (JVar v))
      | rhs
      , Just vu <- UM.lookupUniqMap (varUsage rw) v
      , AssignedOnceKnown ee <- varAssigned vu
      , varUsed vu == One || mayDuplicate ee
      , isDecl (varDeclared vu)
      , mayBeFloated ee
      = goE rhs ee
    goE _rhs (ValExpr v) = ValExpr (goV v)
    goE rhs (SelExpr e i) = SelExpr (goE rhs e) i
    goE rhs (IdxExpr e1 e2) = IdxExpr (goE rhs e1) (goE rhs e2)
    goE rhs (InfixExpr op e1 e2) = InfixExpr op (goE rhs e1) (goE rhs e2)
    goE rhs (UOpExpr op e) = UOpExpr op (goE rhs e)
    goE rhs (IfExpr e1 e2 e3) = IfExpr (goE rhs e1) (goE rhs e2) (goE rhs e3)
    goE rhs (ApplExpr e es) = ApplExpr (goE rhs e) (map (goE rhs) es)

    goV :: JVal -> JVal
    goV (JVar v) = JVar (varReplace v)
    goV (JList es) = JList (map (goE True) es)
    goV (JHash ps) = JHash (fmap (goE True) ps)
    goV v@(JFunc {}) = v
    goV v@(JDouble {}) = v
    goV v@(JInt {}) = v
    goV v@(JStr {}) = v
    goV v@(JRegEx {}) = v
    goV v@(JBool {}) = v

flattenBlock :: [JStat] -> JStat
flattenBlock stats =
    case filter (/= BlockStat []) stats of
        []  -> BlockStat []
        [s] -> s
        ss  -> BlockStat ss

data AnalysisResult = AnalysisResult
  { arBailout       :: !Bool
  , arVarUsage      :: !(UniqMap Ident VarUsage)
  , arDeclaredCount :: !Int
  }

simpleAnalyze :: JFunction -> AnalysisResult
simpleAnalyze (JFunction args body) = go False (AnalysisResult False start 0) body
  where
    start :: UniqMap Ident VarUsage
    start = UM.listToUniqMap
          $ zipWith (\n v -> (v, VarUsage Zero Unassigned (ArgDecl n) False))
                    [0..]
                    args

    add :: Ident -> VarUsage -> AnalysisResult -> AnalysisResult
    add i vu m = m { arVarUsage = UM.addToUniqMap_C (Semi.<>) (arVarUsage m) i vu }


    declare :: Bool -> Ident -> Maybe JExpr -> AnalysisResult -> AnalysisResult
    declare True i _assign m = -- declaration in deeper scope
      let vu = VarUsage Zero AssignedMany NoDecl True
      in  m { arVarUsage = UM.addToUniqMap_C (Semi.<>) (arVarUsage m) i vu}
    declare False i assign m = -- declaration in analyzed scope
      let count  = arDeclaredCount m
          !newCount
            | Just (VarUsage _ _ (LocalDecl _) _) <-
                UM.lookupUniqMap (arVarUsage m) i = count -- already declared
            | otherwise                           = count + 1
          vassign | Just e <- assign = AssignedOnceKnown e
                  | otherwise        = Unassigned
          !vu = VarUsage Zero vassign (LocalDecl count) False
      in m { arDeclaredCount = newCount
           , arVarUsage = UM.addToUniqMap_C (Semi.<>) (arVarUsage m) i vu
           }

    go :: Bool -> AnalysisResult -> JStat -> AnalysisResult
    go deep u (DeclStat v mb_e) =
        case mb_e of
            Nothing -> declare deep v mb_e u
            Just e  -> declare deep v mb_e (goE u e)
    go _deep u (AssignStat (ValExpr (JVar v)) aop e) =
        let use = case aop of
                    AssignOp -> Zero
                    _        -> One
        in add v (VarUsage use (AssignedOnceKnown e) NoDecl False) (goE u e)
    go _deep u (AssignStat lhs _aop rhs) = goE (goE u lhs) rhs
    go _deep u (ReturnStat e) = goE u e
    go deep u (BlockStat ss) = foldl' (go deep) u ss
    go deep u (IfStat e s1 s2) = go deep (go deep (goE u e) s1) s2
    go deep u (WhileStat _b e s) = go deep (goE u e) s
    go deep u (ForStat s1 e s2 s3)
      = go deep (go deep (goE (go deep u s1) e) s2) s3
    go deep u (ForInStat b v e s) =
      let !u' = if b then declare deep v Nothing u else u
      in  add v (VarUsage Zero AssignedMany NoDecl True)
                (go deep (go deep (goE u' e) s) s)
    go deep u (SwitchStat e cases s)
      = go deep (goE (foldl' (go deep) u (map snd cases)) e) s
    go deep u (TryStat s1 v s2 s3)
      = add v (VarUsage Zero AssignedMany NoDecl True)
              (go deep (go deep (go deep u s1) s2) s3)
    go _deep u (ApplStat e es)
      | (ValExpr (JVar (TxtI i))) <- e, i == "eval" = u { arBailout = True }
      | otherwise = foldl' goE (goE u e) es
    go _deep u (UOpStat op e)
      | ValExpr (JVar v) <- e
      , op `elem` [PreIncOp, PostIncOp, PreDecOp, PostDecOp] =
          add v (VarUsage One AssignedOnce NoDecl False) u
      | otherwise = goE u e
    go deep u (LabelStat _ s) = go deep u s
    go _deep u (BreakStat _) = u
    go _deep u (ContinueStat _) = u
    go _deep u (FuncStat _ vs s)
      = go True (foldl' (\u v -> add v (VarUsage Zero AssignedOnce NoDecl True) u) u vs) s

    goE :: AnalysisResult -> JExpr -> AnalysisResult
    goE u (ValExpr v) = goV u v
    goE u (SelExpr e _i) = goE u e
    goE u (IdxExpr e1 e2) = goE (goE u e1) e2
    goE u (InfixExpr _ e1 e2) = goE (goE u e1) e2
    goE u (UOpExpr _ e) = goE u e
    goE u (IfExpr e1 e2 e3) = goE (goE (goE u e1) e2) e3
    goE u (ApplExpr e es)
      | (ValExpr (JVar (TxtI i))) <- e, i == "eval" = u { arBailout = True }
      | otherwise = foldl' goE (goE u e) es

    goV :: AnalysisResult -> JVal -> AnalysisResult
    goV u (JVar v)   = add v (VarUsage One Unassigned NoDecl False) u
    goV u (JList es) = foldl' goE u es
    goV u (JDouble _) = u
    goV u (JInt _) = u
    goV u (JStr _) = u
    goV u (JRegEx _) = u
    goV u (JBool _) = u
    goV u (JHash ps) = foldl' goE u (map snd $ UM.nonDetUniqMapToList ps)
    goV u (JFunc vs s)
      = go True (foldl (\u v -> add v (VarUsage Zero AssignedOnce NoDecl True) u) u vs) s

-- | A trivial assignment is an assignment of a variable to itself: x = x
isTrivialAssignment :: Ident -> AOp -> JExpr -> Bool
isTrivialAssignment v AssignOp (ValExpr (JVar v')) = v == v'
isTrivialAssignment _ _ _                          = False

-- | Does the expression have side effects?
--
-- This only returns False if the expression definitely does not have side
-- effects, i.e. it can be removed without changing the semantics if the
-- result is not used.
--
-- Note: We have some assumptions here about Haskell RTS related values, which
--       may not be true for all JavaScript code. We should really replace
--       these with explicit nodes or annotations in the AST.
--
mayHaveSideEffects :: JExpr -> Bool
-- special cases for Haskell things. These should really be special operations
-- in the AST:
-- 1. stack indexing does not have side effects
mayHaveSideEffects (IdxExpr (ValExpr (JVar (TxtI i))) e)
  | i == "h$stack" = mayHaveSideEffects e
-- 2. we assume that x.d1, x.d2, ... are heap object property lookups,
--    which do not have side effects
mayHaveSideEffects (SelExpr e (TxtI i))
  | isHeapObjectProperty i = mayHaveSideEffects e

-- general cases (no Haskell RTS specific assumptions here):
mayHaveSideEffects (ValExpr v) = mayHaveSideEffectsV v
mayHaveSideEffects (SelExpr {}) = True
mayHaveSideEffects (IdxExpr {}) = True
mayHaveSideEffects (UOpExpr uop e) = uo || mayHaveSideEffects e
    where
        uo = case uop of
              NotOp    -> False
              BNotOp   -> False
              NegOp    -> False
              PlusOp   -> False
              TypeofOp -> False
              _        -> True
mayHaveSideEffects (InfixExpr _o e1 e2) =
  mayHaveSideEffects e1 || mayHaveSideEffects e2
mayHaveSideEffects (IfExpr e1 e2 e3) =
  mayHaveSideEffects e1 || mayHaveSideEffects e2 || mayHaveSideEffects e3
mayHaveSideEffects (ApplExpr {}) = True

mayHaveSideEffectsV :: JVal -> Bool
mayHaveSideEffectsV (JVar {}) = False
mayHaveSideEffectsV (JList es) = any mayHaveSideEffects es
mayHaveSideEffectsV (JDouble {}) = False
mayHaveSideEffectsV (JInt {}) = False
mayHaveSideEffectsV (JStr {}) = False
mayHaveSideEffectsV (JRegEx {}) = False
mayHaveSideEffectsV (JBool {}) = False
mayHaveSideEffectsV (JHash ps) = UM.anyUniqMap mayHaveSideEffects ps
mayHaveSideEffectsV (JFunc {}) = True

isHeapObjectProperty :: FastString -> Bool
isHeapObjectProperty "d1"  = True
isHeapObjectProperty "d2"  = True
isHeapObjectProperty "d3"  = True
isHeapObjectProperty "d4"  = True
isHeapObjectProperty "d5"  = True
isHeapObjectProperty "d6"  = True
isHeapObjectProperty "d7"  = True
isHeapObjectProperty "d8"  = True
isHeapObjectProperty "d9"  = True
isHeapObjectProperty "d10" = True
isHeapObjectProperty "d11" = True
isHeapObjectProperty "d12" = True
isHeapObjectProperty "d13" = True
isHeapObjectProperty "d14" = True
isHeapObjectProperty "d15" = True
isHeapObjectProperty "d16" = True
isHeapObjectProperty "d17" = True
isHeapObjectProperty "d18" = True
isHeapObjectProperty "d19" = True
isHeapObjectProperty "d20" = True
isHeapObjectProperty "d21" = True
isHeapObjectProperty "d22" = True
isHeapObjectProperty "d23" = True
isHeapObjectProperty "d24" = True

isHeapObjectProperty _     = False

isClosureAllocator :: FastString -> Bool
isClosureAllocator "h$c1" = True
isClosureAllocator "h$c2" = True
isClosureAllocator "h$c3" = True
isClosureAllocator "h$c4" = True
isClosureAllocator "h$c5" = True
isClosureAllocator "h$c6" = True
isClosureAllocator "h$c7" = True
isClosureAllocator "h$c8" = True
isClosureAllocator "h$c9" = True
isClosureAllocator "h$c10" = True
isClosureAllocator "h$c11" = True
isClosureAllocator "h$c12" = True
isClosureAllocator "h$c13" = True
isClosureAllocator "h$c14" = True
isClosureAllocator "h$c15" = True
isClosureAllocator "h$c16" = True
isClosureAllocator "h$c17" = True
isClosureAllocator "h$c18" = True
isClosureAllocator "h$c19" = True
isClosureAllocator "h$c20" = True
isClosureAllocator "h$c21" = True
isClosureAllocator "h$c22" = True
isClosureAllocator "h$c23" = True
isClosureAllocator "h$c24" = True
isClosureAllocator _       = False
