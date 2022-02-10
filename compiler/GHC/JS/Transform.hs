{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}

module GHC.JS.Transform
  ( mapIdent
  , mapStatIdent
  , mapExprIdent
  , identsS
  , identsV
  , identsE
  -- * Saturation
  , jsSaturate
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

import GHC.JS.Syntax

import Data.Functor.Identity
import Control.Monad
import Data.Bifunctor

import GHC.Data.FastString
import GHC.Utils.Monad.State.Strict
import GHC.Types.Unique.Map

mapExprIdent :: (Ident -> JExpr) -> JExpr -> JExpr
mapExprIdent f = fst (mapIdent f)

mapStatIdent :: (Ident -> JExpr) -> JStat -> JStat
mapStatIdent f = snd (mapIdent f)

-- | Map on every variable ident
mapIdent :: (Ident -> JExpr) -> (JExpr -> JExpr, JStat -> JStat)
mapIdent f = (map_expr, map_stat)
  where
    map_expr = \case
      ValExpr    v        -> map_val v
      SelExpr    e i      -> SelExpr (map_expr e) i
      IdxExpr    e1 e2    -> IdxExpr (map_expr e1) (map_expr e2)
      InfixExpr  o e1 e2  -> InfixExpr o (map_expr e1) (map_expr e2)
      UOpExpr    o e      -> UOpExpr o (map_expr e)
      IfExpr     e1 e2 e3 -> IfExpr (map_expr e1) (map_expr e2) (map_expr e3)
      ApplExpr   e es     -> ApplExpr (map_expr e) (fmap map_expr es)
      UnsatExpr  me       -> UnsatExpr (fmap map_expr me)

    map_val v = case v of
      JVar     i  -> f i
      JList    es -> ValExpr $ JList (fmap map_expr es)
      JDouble{}   -> ValExpr $ v
      JInt{}      -> ValExpr $ v
      JStr{}      -> ValExpr $ v
      JRegEx{}    -> ValExpr $ v
      JHash me    -> ValExpr $ JHash (fmap map_expr me)
      JFunc is s  -> ValExpr $ JFunc is (map_stat s)
      UnsatVal v2 -> ValExpr $ UnsatVal v2

    map_stat s = case s of
      DeclStat i e          -> DeclStat i (fmap map_expr e)
      ReturnStat e          -> ReturnStat (map_expr e)
      IfStat     e s1 s2    -> IfStat (map_expr e) (map_stat s1) (map_stat s2)
      WhileStat  b e s2     -> WhileStat b (map_expr e) (map_stat s2)
      ForInStat  b i e s2   -> ForInStat b i (map_expr e) (map_stat s2)
      SwitchStat e les s2   -> SwitchStat (map_expr e) (fmap (bimap map_expr map_stat) les) (map_stat s2)
      TryStat    s2 i s3 s4 -> TryStat (map_stat s2) i (map_stat s3) (map_stat s4)
      BlockStat  ls         -> BlockStat (fmap map_stat ls)
      ApplStat   e es       -> ApplStat (map_expr e) (fmap map_expr es)
      UOpStat    o e        -> UOpStat o (map_expr e)
      AssignStat e1 e2      -> AssignStat (map_expr e1) (map_expr e2)
      UnsatBlock ms         -> UnsatBlock (fmap map_stat ms)
      LabelStat  l s2       -> LabelStat l (map_stat s2)
      BreakStat{}           -> s
      ContinueStat{}        -> s

{-# INLINE identsS #-}
identsS :: JStat -> [Ident]
identsS = \case
  DeclStat i e       -> [i] ++ maybe [] identsE e
  ReturnStat e       -> identsE e
  IfStat e s1 s2     -> identsE e ++ identsS s1 ++ identsS s2
  WhileStat _ e s    -> identsE e ++ identsS s
  ForInStat _ i e s  -> [i] ++ identsE e ++ identsS s
  SwitchStat e xs s  -> identsE e ++ concatMap traverseCase xs ++ identsS s
                          where traverseCase (e,s) = identsE e ++ identsS s
  TryStat s1 i s2 s3 -> identsS s1 ++ [i] ++ identsS s2 ++ identsS s3
  BlockStat xs       -> concatMap identsS xs
  ApplStat e es      -> identsE e ++ concatMap identsE es
  UOpStat _op e      -> identsE e
  AssignStat e1 e2   -> identsE e1 ++ identsE e2
  UnsatBlock{}       -> error "identsS: UnsatBlock"
  LabelStat _l s     -> identsS s
  BreakStat{}        -> []
  ContinueStat{}     -> []

{-# INLINE identsE #-}
identsE :: JExpr -> [Ident]
identsE = \case
  ValExpr v         -> identsV v
  SelExpr e _i      -> identsE e -- do not rename properties
  IdxExpr e1 e2     -> identsE e1 ++ identsE e2
  InfixExpr _ e1 e2 -> identsE e1 ++ identsE e2
  UOpExpr _ e       -> identsE e
  IfExpr e1 e2 e3   -> identsE e1 ++ identsE e2 ++ identsE e3
  ApplExpr e es     -> identsE e  ++ concatMap identsE es
  UnsatExpr{}       -> error "identsE: UnsatExpr"

{-# INLINE identsV #-}
identsV :: JVal -> [Ident]
identsV = \case
  JVar i       -> [i]
  JList xs     -> concatMap identsE xs
  JDouble{}    -> []
  JInt{}       -> []
  JStr{}       -> []
  JRegEx{}     -> []
  JHash m      -> concatMap (identsE . snd) (nonDetEltsUniqMap m)
  JFunc args s -> args ++ identsS s
  UnsatVal{}   -> error "identsV: UnsatVal"


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
               where (ls, vs) = unzip (nonDetEltsUniqMap m)
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
jsSaturate :: (JMacro a) => Maybe FastString -> a -> a
jsSaturate str x = evalState (runIdentSupply $ jsSaturate_ x) (newIdentSupply str)

jsSaturate_ :: (JMacro a) => a -> IdentSupply a
jsSaturate_ e = IS $ jfromGADT <$> go (jtoGADT e)
    where
      go :: forall a. JMGadt a -> State [Ident] (JMGadt a)
      go v = case v of
               JMGStat (UnsatBlock us) -> go =<< (JMGStat <$> runIdentSupply us)
               JMGExpr (UnsatExpr  us) -> go =<< (JMGExpr <$> runIdentSupply us)
               JMGVal  (UnsatVal   us) -> go =<< (JMGVal  <$> runIdentSupply us)
               _ -> composOpM go v
