{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}

module GHC.JS.Transform
  ( identsS
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
  , satJExpr
  , satJStat
  )
where

import GHC.Prelude

import qualified GHC.JS.Syntax as Sat
import GHC.JS.Unsat.Syntax

import Data.Functor.Identity
import Control.Monad
import Control.Arrow ((***))

import GHC.Data.FastString
import GHC.Utils.Monad.State.Strict
import GHC.Types.Unique.Map


{-# INLINE identsS #-}
identsS :: Sat.JStat -> [Ident]
identsS = \case
  Sat.DeclStat i e       -> [i] ++ maybe [] identsE e
  Sat.ReturnStat e       -> identsE e
  Sat.IfStat e s1 s2     -> identsE e ++ identsS s1 ++ identsS s2
  Sat.WhileStat _ e s    -> identsE e ++ identsS s
  Sat.ForInStat _ i e s  -> [i] ++ identsE e ++ identsS s
  Sat.SwitchStat e xs s  -> identsE e ++ concatMap traverseCase xs ++ identsS s
                               where traverseCase (e,s) = identsE e ++ identsS s
  Sat.TryStat s1 i s2 s3 -> identsS s1 ++ [i] ++ identsS s2 ++ identsS s3
  Sat.BlockStat xs       -> concatMap identsS xs
  Sat.ApplStat e es      -> identsE e ++ concatMap identsE es
  Sat.UOpStat _op e      -> identsE e
  Sat.AssignStat e1 e2   -> identsE e1 ++ identsE e2
  Sat.LabelStat _l s     -> identsS s
  Sat.BreakStat{}        -> []
  Sat.ContinueStat{}     -> []

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


--------------------------------------------------------------------------------
--                            Translation
--
-- This will be moved after GHC.JS.Syntax is removed
--------------------------------------------------------------------------------
satJStat :: JStat -> Sat.JStat
satJStat = witness . proof
  where proof = jsSaturate Nothing

        -- This is an Applicative but we can't use it because no type variables :(
        witness :: JStat -> Sat.JStat
        witness (DeclStat i rhs)      = Sat.DeclStat i (fmap satJExpr rhs)
        witness (ReturnStat e)        = Sat.ReturnStat (satJExpr e)
        witness (IfStat c t e)        = Sat.IfStat (satJExpr c) (witness t) (witness e)
        witness (WhileStat is_do c e) = Sat.WhileStat is_do (satJExpr c) (witness e)
        witness (ForInStat is_each i iter body) = Sat.ForInStat is_each i
                                                  (satJExpr iter)
                                                  (witness body)
        witness (SwitchStat struct ps def) = Sat.SwitchStat
                                             (satJExpr struct)
                                             (map (satJExpr *** witness) ps)
                                             (witness def)
        witness (TryStat t i c f)     = Sat.TryStat (witness t) i (witness c) (witness f)
        witness (BlockStat bs)        = Sat.BlockStat $! fmap witness bs
        witness (ApplStat rator rand) = Sat.ApplStat (satJExpr rator) (satJExpr <$> rand)
        witness (UOpStat rator rand)  = Sat.UOpStat  (satJUOp rator) (satJExpr rand)
        witness (AssignStat lhs rhs)  = Sat.AssignStat (satJExpr lhs) (satJExpr rhs)
        witness (LabelStat lbl stmt)  = Sat.LabelStat lbl (witness stmt)
        witness (BreakStat Nothing)   = Sat.BreakStat Nothing
        witness (BreakStat (Just l))  = Sat.BreakStat $! Just l
        witness (ContinueStat Nothing)  = Sat.ContinueStat Nothing
        witness (ContinueStat (Just l)) = Sat.ContinueStat $! Just l
        witness UnsatBlock{}            = error "satJStat: discovered an Unsat...impossibly"


satJExpr :: JExpr -> Sat.JExpr
satJExpr = go
  where
    go (ValExpr v)        = Sat.ValExpr (satJVal v)
    go (SelExpr obj i)    = Sat.SelExpr (satJExpr obj) i
    go (IdxExpr o i)      = Sat.IdxExpr (satJExpr o) (satJExpr i)
    go (InfixExpr op l r) = Sat.InfixExpr (satJOp op) (satJExpr l) (satJExpr r)
    go (UOpExpr op r)     = Sat.UOpExpr (satJUOp op) (satJExpr r)
    go (IfExpr c t e)     = Sat.IfExpr (satJExpr c) (satJExpr t) (satJExpr e)
    go (ApplExpr rator rands) = Sat.ApplExpr (satJExpr rator) (satJExpr <$> rands)
    go UnsatExpr{}        = error "satJExpr: discovered an Unsat...impossibly"

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

satJVal :: JVal -> Sat.JVal
satJVal = go
  where
    go (JVar i)    = Sat.JVar i
    go (JList xs)  = Sat.JList (satJExpr <$> xs)
    go (JDouble d) = Sat.JDouble (Sat.SaneDouble (unSaneDouble d))
    go (JInt i)    = Sat.JInt   i
    go (JStr f)    = Sat.JStr   f
    go (JRegEx f)  = Sat.JRegEx f
    go (JHash m)   = Sat.JHash (satJExpr <$> m)
    go (JFunc args body) = Sat.JFunc args (satJStat body)
    go UnsatVal{} = error "jvalToSatVar: discovered an Sat...impossibly"
