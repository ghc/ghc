{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module T15186A (Ctx, Assignment, data EmptyAssn, data (:>)) where

import Data.Kind (Type)

data Ctx k
  = EmptyCtx
  | Ctx k ::> k

type SingleCtx x = 'EmptyCtx '::> x

type family (<+>) (x :: Ctx k) (y :: Ctx k) :: Ctx k where
  x <+> 'EmptyCtx = x
  x <+> (y '::> e) = (x <+> y) '::> e

data Height = Zero | Succ Height

data BinomialTree (h::Height) (f :: k -> Type) :: Ctx k -> Type where
  Empty :: BinomialTree h f 'EmptyCtx
  PlusOne  :: !Int
           -> !(BinomialTree ('Succ h) f x)
           -> !(BalancedTree h f y)
           -> BinomialTree h f (x <+> y)
  PlusZero  :: !Int
            -> !(BinomialTree ('Succ h) f x)
            -> BinomialTree h f x

newtype Assignment (f :: k -> Type) (ctx :: Ctx k)
  = Assignment (BinomialTree 'Zero f ctx)

data AssignView f ctx where
  AssignEmpty :: AssignView f 'EmptyCtx
  AssignExtend :: Assignment f ctx
               -> f tp
               -> AssignView f (ctx '::> tp)

data DropResult f (ctx :: Ctx k) where
  DropEmpty :: DropResult f 'EmptyCtx
  DropExt   :: BinomialTree 'Zero f x
            -> f y
            -> DropResult f (x '::> y)

data BalancedTree h (f :: k -> Type) (p :: Ctx k) where
  BalLeaf :: !(f x) -> BalancedTree 'Zero f (SingleCtx x)
  BalPair :: !(BalancedTree h f x)
          -> !(BalancedTree h f y)
          -> BalancedTree ('Succ h) f (x <+> y)

bal_drop :: forall h f x y
          . BinomialTree h f x
         -> BalancedTree h f y
         -> DropResult f (x <+> y)
bal_drop t (BalLeaf e) = DropExt t e
bal_drop _ (BalPair {}) = undefined

bin_drop :: forall h f ctx
          . BinomialTree h f ctx
         -> DropResult f ctx
bin_drop Empty = DropEmpty
bin_drop (PlusZero _ u) = bin_drop u
bin_drop (PlusOne s t u) =
  let m = case t of
            Empty -> Empty
            _ -> PlusZero s t
   in bal_drop m u

viewAssign :: forall f ctx . Assignment f ctx -> AssignView f ctx
viewAssign (Assignment x) =
  case bin_drop x of
    DropEmpty -> AssignEmpty
    DropExt t v -> AssignExtend (Assignment t) v

pattern EmptyAssn :: () => ctx ~ 'EmptyCtx => Assignment f ctx
pattern EmptyAssn <- (viewAssign -> AssignEmpty)

pattern (:>) :: () => ctx' ~ (ctx '::> tp) => Assignment f ctx -> f tp -> Assignment f ctx'
pattern (:>) a v <- (viewAssign -> AssignExtend a v)
