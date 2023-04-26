{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
  -- * Saturation
  , satJStat
  , satJExpr
  -- * Generic traversal (via compos)
  , JMacro(..)
  , JMGadt(..)
  , Compos(..)
  , composOp
  , composOpM
  , composOpM_
  , composOpFold
  )
where

import GHC.Prelude

import qualified GHC.JS.Syntax as Sat
import GHC.JS.Unsat.Syntax

import Data.Functor.Identity
import Control.Monad
import Data.List (sortBy)

import GHC.Data.FastString
import GHC.Utils.Monad.State.Strict
import GHC.Types.Unique.Map
import GHC.Types.Unique.FM


{-# INLINE identsS #-}
identsS :: Sat.JStat -> [Ident]
identsS = \case
  Sat.DeclStat i e       -> [i] ++ maybe [] identsE e
  Sat.ReturnStat e       -> identsE e
  Sat.IfStat e s1 s2     -> identsE e ++ identsS s1 ++ identsS s2
  Sat.WhileStat _ e s    -> identsE e ++ identsS s
  Sat.ForStat init p step body -> identsS init ++ identsE p ++ identsS step ++ identsS body
  Sat.ForInStat _ i e s  -> [i] ++ identsE e ++ identsS s
  Sat.SwitchStat e xs s  -> identsE e ++ concatMap traverseCase xs ++ identsS s
                               where traverseCase (e,s) = identsE e ++ identsS s
  Sat.TryStat s1 i s2 s3 -> identsS s1 ++ [i] ++ identsS s2 ++ identsS s3
  Sat.BlockStat xs       -> concatMap identsS xs
  Sat.ApplStat e es      -> identsE e ++ concatMap identsE es
  Sat.UOpStat _op e      -> identsE e
  Sat.AssignStat e1 _op e2 -> identsE e1 ++ identsE e2
  Sat.LabelStat _l s     -> identsS s
  Sat.BreakStat{}        -> []
  Sat.ContinueStat{}     -> []
  Sat.FuncStat i args body -> [i] ++ args ++ identsS body

{-# INLINE identsE #-}
identsE :: Sat.JExpr -> [Ident]
identsE = \case
  Sat.ValExpr v         -> identsV v
  Sat.SelExpr e _i      -> identsE e -- do not rename properties
  Sat.IdxExpr e1 e2     -> identsE e1 ++ identsE e2
  Sat.InfixExpr _ e1 e2 -> identsE e1 ++ identsE e2
  Sat.UOpExpr _ e       -> identsE e
  Sat.IfExpr e1 e2 e3   -> identsE e1 ++ identsE e2 ++ identsE e3
  Sat.ApplExpr e es     -> identsE e  ++ concatMap identsE es

{-# INLINE identsV #-}
identsV :: Sat.JVal -> [Ident]
identsV = \case
  Sat.JVar i       -> [i]
  Sat.JList xs     -> concatMap identsE xs
  Sat.JDouble{}    -> []
  Sat.JInt{}       -> []
  Sat.JStr{}       -> []
  Sat.JRegEx{}     -> []
  Sat.JHash m      -> concatMap identsE (nonDetEltsUniqMap m)
  Sat.JFunc args s -> args ++ identsS s


{--------------------------------------------------------------------
  Compos
--------------------------------------------------------------------}
-- | Compos and ops for generic traversal as defined over
-- the JMacro ADT.

-- | Utility class to coerce the ADT into a regular structure.

class JMacro a where
    jtoGADT :: a -> JMGadt a
    jfromGADT :: JMGadt a -> a

instance JMacro Ident where
    jtoGADT = JMGId
    jfromGADT (JMGId x) = x

instance JMacro JStat where
    jtoGADT = JMGStat
    jfromGADT (JMGStat x) = x

instance JMacro JExpr where
    jtoGADT = JMGExpr
    jfromGADT (JMGExpr x) = x

instance JMacro JVal where
    jtoGADT = JMGVal
    jfromGADT (JMGVal x) = x

-- | Union type to allow regular traversal by compos.
data JMGadt a where
    JMGId   :: Ident -> JMGadt Ident
    JMGStat :: JStat -> JMGadt JStat
    JMGExpr :: JExpr -> JMGadt JExpr
    JMGVal  :: JVal  -> JMGadt JVal

composOp :: Compos t => (forall a. t a -> t a) -> t b -> t b
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t b -> m (t b)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t b -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }

class Compos t where
    compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
           -> (forall a. t a -> m (t a)) -> t c -> m (t c)

instance Compos JMGadt where
    compos = jmcompos

jmcompos :: forall m c. (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b) -> (forall a. JMGadt a -> m (JMGadt a)) -> JMGadt c -> m (JMGadt c)
jmcompos ret app f' v =
    case v of
     JMGId _ -> ret v
     JMGStat v' -> ret JMGStat `app` case v' of
           DeclStat i e -> ret DeclStat `app` f i `app` mapMaybeM' f e
           ReturnStat i -> ret ReturnStat `app` f i
           IfStat e s s' -> ret IfStat `app` f e `app` f s `app` f s'
           WhileStat b e s -> ret (WhileStat b) `app` f e `app` f s
           ForStat init p step body -> ret ForStat  `app` f init `app` f p
                                           `app` f step `app` f body
           ForInStat b i e s -> ret (ForInStat b) `app` f i `app` f e `app` f s
           SwitchStat e l d -> ret SwitchStat `app` f e `app` l' `app` f d
               where l' = mapM' (\(c,s) -> ret (,) `app` f c `app` f s) l
           BlockStat xs -> ret BlockStat `app` mapM' f xs
           ApplStat  e xs -> ret ApplStat `app` f e `app` mapM' f xs
           TryStat s i s1 s2 -> ret TryStat `app` f s `app` f i `app` f s1 `app` f s2
           UOpStat o e -> ret (UOpStat o) `app` f e
           AssignStat e e' -> ret AssignStat `app` f e `app` f e'
           UnsatBlock _ -> ret v'
           ContinueStat l -> ret (ContinueStat l)
           FuncStat i args body -> ret FuncStat `app` f i `app` mapM' f args `app` f body
           BreakStat l -> ret (BreakStat l)
           LabelStat l s -> ret (LabelStat l) `app` f s
     JMGExpr v' -> ret JMGExpr `app` case v' of
           ValExpr e -> ret ValExpr `app` f e
           SelExpr e e' -> ret SelExpr `app` f e `app` f e'
           IdxExpr e e' -> ret IdxExpr `app` f e `app` f e'
           InfixExpr o e e' -> ret (InfixExpr o) `app` f e `app` f e'
           UOpExpr o e -> ret (UOpExpr o) `app` f e
           IfExpr e e' e'' -> ret IfExpr `app` f e `app` f e' `app` f e''
           ApplExpr e xs -> ret ApplExpr `app` f e `app` mapM' f xs
           UnsatExpr _ -> ret v'
     JMGVal v' -> ret JMGVal `app` case v' of
           JVar i -> ret JVar `app` f i
           JList xs -> ret JList `app` mapM' f xs
           JDouble _ -> ret v'
           JInt    _ -> ret v'
           JStr    _ -> ret v'
           JRegEx  _ -> ret v'
           JHash   m -> ret JHash `app` m'
               -- nonDetEltsUniqMap doesn't introduce nondeterminism here because the
               -- elements are treated independently before being re-added to a UniqMap
               where (ls, vs) = unzip (nonDetUniqMapToList m)
                     m' = ret (listToUniqMap . zip ls) `app` mapM' f vs
           JFunc xs s -> ret JFunc `app` mapM' f xs `app` f s
           UnsatVal _ -> ret v'

  where
    mapM' :: forall a. (a -> m a) -> [a] -> m [a]
    mapM' g = foldr (app . app (ret (:)) . g) (ret [])
    mapMaybeM' :: forall a. (a -> m a) -> Maybe a -> m (Maybe a)
    mapMaybeM' g = \case
      Nothing -> ret Nothing
      Just a  -> app (ret Just) (g a)
    f :: forall b. JMacro b => b -> m b
    f x = ret jfromGADT `app` f' (jtoGADT x)

{--------------------------------------------------------------------
  Saturation
--------------------------------------------------------------------}

-- | Given an optional prefix, fills in all free variable names with a supply
-- of names generated by the prefix.
satJStat :: Maybe FastString -> JStat -> Sat.JStat
satJStat str x = evalState (jsSaturateS x) (newIdentSupply str)

satJExpr :: Maybe FastString -> JExpr -> Sat.JExpr
satJExpr str x = evalState (jsSaturateE x) (newIdentSupply str)

jsSaturateS :: JStat -> State [Ident] Sat.JStat
jsSaturateS  = \case
  DeclStat i rhs        -> Sat.DeclStat i <$> mapM jsSaturateE rhs
  ReturnStat e          -> Sat.ReturnStat <$> jsSaturateE e
  IfStat c t e          -> Sat.IfStat <$> jsSaturateE c <*> jsSaturateS t <*> jsSaturateS e
  WhileStat is_do c e   -> Sat.WhileStat is_do <$> jsSaturateE c <*> jsSaturateS e
  ForStat init p step body -> Sat.ForStat <$> jsSaturateS init <*> jsSaturateE p
                                          <*> jsSaturateS step <*> jsSaturateS body
  ForInStat is_each i iter body -> Sat.ForInStat is_each i <$> jsSaturateE iter <*> jsSaturateS body
  SwitchStat struct ps def -> Sat.SwitchStat <$> jsSaturateE struct
                                             <*> mapM (\(p1, p2) -> (,) <$> jsSaturateE p1 <*> jsSaturateS p2) ps
                                             <*> jsSaturateS def
  TryStat t i c f       -> Sat.TryStat <$> jsSaturateS t <*> pure i <*> jsSaturateS c <*> jsSaturateS f
  BlockStat bs          -> fmap Sat.BlockStat $! mapM jsSaturateS bs
  ApplStat rator rand   -> Sat.ApplStat <$> jsSaturateE rator <*> mapM jsSaturateE rand
  UOpStat  rator rand   -> Sat.UOpStat (satJUOp rator) <$> jsSaturateE rand
  AssignStat lhs rhs    -> Sat.AssignStat <$> jsSaturateE lhs <*> pure Sat.AssignOp <*> jsSaturateE rhs
  LabelStat lbl stmt    -> Sat.LabelStat lbl <$> jsSaturateS stmt
  BreakStat m_l         -> return $ Sat.BreakStat $! m_l
  ContinueStat m_l      -> return $ Sat.ContinueStat $! m_l
  FuncStat i args body  -> Sat.FuncStat i args <$> jsSaturateS body
  UnsatBlock us         -> jsSaturateS =<< runIdentSupply us

jsSaturateE :: JExpr -> State [Ident] Sat.JExpr
jsSaturateE = \case
  ValExpr v            -> Sat.ValExpr <$> jsSaturateV v
  SelExpr obj i        -> Sat.SelExpr <$> jsSaturateE obj <*> pure i
  IdxExpr o i          -> Sat.IdxExpr <$> jsSaturateE o <*> jsSaturateE i
  InfixExpr op l r     -> Sat.InfixExpr (satJOp op) <$> jsSaturateE l <*> jsSaturateE r
  UOpExpr op r         -> Sat.UOpExpr (satJUOp op) <$> jsSaturateE r
  IfExpr c t e         -> Sat.IfExpr <$> jsSaturateE c <*> jsSaturateE t <*> jsSaturateE e
  ApplExpr rator rands -> Sat.ApplExpr <$> jsSaturateE rator <*> mapM jsSaturateE rands
  UnsatExpr us         -> jsSaturateE =<< runIdentSupply us

jsSaturateV :: JVal -> State [Ident] Sat.JVal
jsSaturateV = \case
  JVar i   -> return $ Sat.JVar i
  JList xs -> Sat.JList <$> mapM jsSaturateE xs
  JDouble d -> return $ Sat.JDouble (Sat.SaneDouble (unSaneDouble d))
  JInt i    -> return $ Sat.JInt   i
  JStr s    -> return $ Sat.JStr   s
  JRegEx f  -> return $ Sat.JRegEx f
  JHash m   -> Sat.JHash <$> mapUniqMapM satHash m
    where
      satHash (i, x) = (i,) . (i,) <$> jsSaturateE x
      compareHash (i,_) (j,_) = lexicalCompareFS i j
      -- By lexically sorting the elements, the non-determinism introduced by nonDetEltsUFM is avoided
      mapUniqMapM f (UniqMap m) = UniqMap . listToUFM <$> (mapM f . sortBy compareHash $ nonDetEltsUFM m)
  JFunc args body   -> Sat.JFunc args <$> jsSaturateS body
  UnsatVal us       -> jsSaturateV =<< runIdentSupply us

satJOp :: JOp -> Sat.Op
satJOp = go
  where
    go EqOp         = Sat.EqOp
    go StrictEqOp   = Sat.StrictEqOp
    go NeqOp        = Sat.NeqOp
    go StrictNeqOp  = Sat.StrictNeqOp
    go GtOp         = Sat.GtOp
    go GeOp         = Sat.GeOp
    go LtOp         = Sat.LtOp
    go LeOp         = Sat.LeOp
    go AddOp        = Sat.AddOp
    go SubOp        = Sat.SubOp
    go MulOp        = Sat.MulOp
    go DivOp        = Sat.DivOp
    go ModOp        = Sat.ModOp
    go LeftShiftOp  = Sat.LeftShiftOp
    go RightShiftOp = Sat.RightShiftOp
    go ZRightShiftOp = Sat.ZRightShiftOp
    go BAndOp       = Sat.BAndOp
    go BOrOp        = Sat.BOrOp
    go BXorOp       = Sat.BXorOp
    go LAndOp       = Sat.LAndOp
    go LOrOp        = Sat.LOrOp
    go InstanceofOp = Sat.InstanceofOp
    go InOp         = Sat.InOp

satJUOp :: JUOp -> Sat.UOp
satJUOp = go
  where
    go NotOp     = Sat.NotOp
    go BNotOp    = Sat.BNotOp
    go NegOp     = Sat.NegOp
    go PlusOp    = Sat.PlusOp
    go NewOp     = Sat.NewOp
    go TypeofOp  = Sat.TypeofOp
    go DeleteOp  = Sat.DeleteOp
    go YieldOp   = Sat.YieldOp
    go VoidOp    = Sat.VoidOp
    go PreIncOp  = Sat.PreIncOp
    go PostIncOp = Sat.PostIncOp
    go PreDecOp  = Sat.PreDecOp
    go PostDecOp = Sat.PostDecOp

