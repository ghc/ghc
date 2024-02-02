{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module GHC.JS.Transform
  ( identsS
  , identsV
  , identsE
  , jStgExprToJS
  , jStgStatToJS
  )
where

import GHC.Prelude

import GHC.JS.Ident
import GHC.JS.JStg.Syntax
import qualified GHC.JS.Syntax as JS

import Data.List (sortBy)

import GHC.Data.FastString
import GHC.Types.Unique.Map
import GHC.Types.Unique.FM


{-# INLINE identsS #-}
identsS :: JStgStat -> [Ident]
identsS = \case
  DeclStat i e       -> [i] ++ maybe [] identsE e
  ReturnStat e       -> identsE e
  IfStat e s1 s2     -> identsE e ++ identsS s1 ++ identsS s2
  WhileStat _ e s    -> identsE e ++ identsS s
  ForStat init p step body -> identsS init ++ identsE p ++ identsS step ++ identsS body
  ForInStat _ i e s  -> [i] ++ identsE e ++ identsS s
  SwitchStat e xs s  -> identsE e ++ concatMap traverseCase xs ++ identsS s
                           where traverseCase (e,s) = identsE e ++ identsS s
  TryStat s1 i s2 s3 -> identsS s1 ++ [i] ++ identsS s2 ++ identsS s3
  BlockStat xs       -> concatMap identsS xs
  ApplStat e es      -> identsE e ++ concatMap identsE es
  UOpStat _op e      -> identsE e
  AssignStat e1 _op e2 -> identsE e1 ++ identsE e2
  LabelStat _l s     -> identsS s
  BreakStat{}        -> []
  ContinueStat{}     -> []
  FuncStat i args body -> [i] ++ args ++ identsS body

{-# INLINE identsE #-}
identsE :: JStgExpr -> [Ident]
identsE = \case
  ValExpr v         -> identsV v
  SelExpr e _i      -> identsE e -- do not rename properties
  IdxExpr e1 e2     -> identsE e1 ++ identsE e2
  InfixExpr _ e1 e2 -> identsE e1 ++ identsE e2
  UOpExpr _ e       -> identsE e
  IfExpr e1 e2 e3   -> identsE e1 ++ identsE e2 ++ identsE e3
  ApplExpr e es     -> identsE e  ++ concatMap identsE es

{-# INLINE identsV #-}
identsV :: JVal -> [Ident]
identsV = \case
  JVar i       -> [i]
  JList xs     -> concatMap identsE xs
  JDouble{}    -> []
  JInt{}       -> []
  JStr{}       -> []
  JRegEx{}     -> []
  JBool{}      -> []
  JHash m      -> concatMap identsE (nonDetEltsUniqMap m)
  JFunc args s -> args ++ identsS s

--------------------------------------------------------------------------------
--                            Translation
--
--------------------------------------------------------------------------------
jStgStatToJS :: JStgStat -> JS.JStat
jStgStatToJS  = \case
  DeclStat i rhs        -> JS.DeclStat i $ fmap jStgExprToJS rhs
  ReturnStat e          -> JS.ReturnStat $ jStgExprToJS e
  IfStat c t e          -> JS.IfStat (jStgExprToJS c) (jStgStatToJS t) (jStgStatToJS e)
  WhileStat is_do c e   -> JS.WhileStat is_do (jStgExprToJS c) (jStgStatToJS e)
  ForStat init p step body -> JS.ForStat  (jStgStatToJS init) (jStgExprToJS p)
                                           (jStgStatToJS step) (jStgStatToJS body)
  ForInStat is_each i iter body -> JS.ForInStat (is_each) i (jStgExprToJS iter) (jStgStatToJS body)
  SwitchStat struct ps def -> JS.SwitchStat
                              (jStgExprToJS struct)
                              (map (\(p1, p2) -> (jStgExprToJS p1, jStgStatToJS p2)) ps)
                              (jStgStatToJS def)
  TryStat t i c f       -> JS.TryStat (jStgStatToJS t) i (jStgStatToJS c) (jStgStatToJS f)
  BlockStat bs          -> JS.BlockStat $ map jStgStatToJS bs
  ApplStat rator rand   -> JS.ApplStat (jStgExprToJS rator) $ map jStgExprToJS rand
  UOpStat  rator rand   -> JS.UOpStat (jStgUOpToJS rator) (jStgExprToJS rand)
  AssignStat lhs op rhs -> JS.AssignStat (jStgExprToJS lhs) (jStgAOpToJS op) (jStgExprToJS rhs)
  LabelStat lbl stmt    -> JS.LabelStat lbl (jStgStatToJS stmt)
  BreakStat m_l         -> JS.BreakStat $! m_l
  ContinueStat m_l      -> JS.ContinueStat $! m_l
  FuncStat i args body  -> JS.FuncStat i args $ jStgStatToJS body

jStgExprToJS :: JStgExpr -> JS.JExpr
jStgExprToJS = \case
  ValExpr v            -> JS.ValExpr $ jStgValToJS v
  SelExpr obj i        -> JS.SelExpr (jStgExprToJS obj) i
  IdxExpr o i          -> JS.IdxExpr (jStgExprToJS o) (jStgExprToJS i)
  InfixExpr op l r     -> JS.InfixExpr (jStgOpToJS op) (jStgExprToJS l) (jStgExprToJS r)
  UOpExpr op r         -> JS.UOpExpr (jStgUOpToJS op) (jStgExprToJS r)
  IfExpr c t e         -> JS.IfExpr (jStgExprToJS c) (jStgExprToJS t) (jStgExprToJS e)
  ApplExpr rator rands -> JS.ApplExpr (jStgExprToJS rator) $ map jStgExprToJS rands

jStgValToJS :: JVal -> JS.JVal
jStgValToJS = \case
  JVar i   -> JS.JVar i
  JList xs -> JS.JList $ map jStgExprToJS xs
  JDouble d -> JS.JDouble d
  JInt i    -> JS.JInt   i
  JStr s    -> JS.JStr   s
  JRegEx f  -> JS.JRegEx f
  JBool b   -> JS.JBool  b
  JHash m   -> JS.JHash $ mapUniqMapM satHash m
    where
      satHash (i, x) = (i,) . (i,) $ jStgExprToJS x
      compareHash (i,_) (j,_) = lexicalCompareFS i j
      -- By lexically sorting the elements, the non-determinism introduced by nonDetEltsUFM is avoided
      mapUniqMapM f (UniqMap m) = UniqMap . listToUFM $ (map f . sortBy compareHash $ nonDetEltsUFM m)
  JFunc args body   -> JS.JFunc args $ jStgStatToJS body

jStgOpToJS :: Op -> JS.Op
jStgOpToJS = go
  where
    go EqOp         = JS.EqOp
    go StrictEqOp   = JS.StrictEqOp
    go NeqOp        = JS.NeqOp
    go StrictNeqOp  = JS.StrictNeqOp
    go GtOp         = JS.GtOp
    go GeOp         = JS.GeOp
    go LtOp         = JS.LtOp
    go LeOp         = JS.LeOp
    go AddOp        = JS.AddOp
    go SubOp        = JS.SubOp
    go MulOp        = JS.MulOp
    go DivOp        = JS.DivOp
    go ModOp        = JS.ModOp
    go LeftShiftOp  = JS.LeftShiftOp
    go RightShiftOp = JS.RightShiftOp
    go ZRightShiftOp = JS.ZRightShiftOp
    go BAndOp       = JS.BAndOp
    go BOrOp        = JS.BOrOp
    go BXorOp       = JS.BXorOp
    go LAndOp       = JS.LAndOp
    go LOrOp        = JS.LOrOp
    go InstanceofOp = JS.InstanceofOp
    go InOp         = JS.InOp

jStgUOpToJS :: UOp -> JS.UOp
jStgUOpToJS = go
  where
    go NotOp     = JS.NotOp
    go BNotOp    = JS.BNotOp
    go NegOp     = JS.NegOp
    go PlusOp    = JS.PlusOp
    go NewOp     = JS.NewOp
    go TypeofOp  = JS.TypeofOp
    go DeleteOp  = JS.DeleteOp
    go YieldOp   = JS.YieldOp
    go VoidOp    = JS.VoidOp
    go PreIncOp  = JS.PreIncOp
    go PostIncOp = JS.PostIncOp
    go PreDecOp  = JS.PreDecOp
    go PostDecOp = JS.PostDecOp

jStgAOpToJS :: AOp -> JS.AOp
jStgAOpToJS AssignOp    = JS.AssignOp
jStgAOpToJS AddAssignOp = JS.AddAssignOp
jStgAOpToJS SubAssignOp = JS.SubAssignOp
