{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PatternSynonyms, TypeOperators,
             ViewPatterns #-}
module BundledPatterns (Vec(Nil,(:>)), RTree (LR,BR)) where

import Data.Kind (Type)

import GHC.TypeLits
import Prelude hiding (head, tail)
import Unsafe.Coerce

-- | Fixed size vectors.
--
-- * Lists with their length encoded in their type
-- * 'Vec'tor elements have an __ASCENDING__ subscript starting from 0 and
--   ending at @'length' - 1@.
data Vec :: Nat -> Type -> Type where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a

infixr 5 `Cons`

-- | Add an element to the head of a vector.
--
-- >>> 3:>4:>5:>Nil
-- <3,4,5>
-- >>> let x = 3:>4:>5:>Nil
-- >>> :t x
-- x :: Num a => Vec 3 a
--
-- Can be used as a pattern:
--
-- >>> let f (x :> y :> _) = x + y
-- >>> :t f
-- f :: Num a => Vec ((n + 1) + 1) a -> a
-- >>> f (3:>4:>5:>6:>7:>Nil)
-- 7
--
-- Also in conjunctions with (':<'):
--
-- >>> let g (a :> b :> (_ :< y :< x)) = a + b +  x + y
-- >>> :t g
-- g :: Num a => Vec ((((n + 1) + 1) + 1) + 1) a -> a
-- >>> g (1:>2:>3:>4:>5:>Nil)
-- 12
pattern (:>) :: a -> Vec n a -> Vec (n + 1) a
pattern (:>) x xs <- ((\ys -> (head ys,tail ys)) -> (x,xs))
  where
    (:>) x xs = Cons x xs

infixr 5 :>

head :: Vec (n + 1) a -> a
head (x `Cons` _) = x

tail :: Vec (n + 1) a -> Vec n a
tail (_ `Cons` xs) = unsafeCoerce xs

-- | Perfect depth binary tree.
--
-- * Only has elements at the leaf of the tree
-- * A tree of depth /d/ has /2^d/ elements.
data RTree :: Nat -> Type -> Type where
  LR_ :: a -> RTree 0 a
  BR_ :: RTree d a -> RTree d a -> RTree (d+1) a

textract :: RTree 0 a -> a
textract (LR_ x) = x
{-# NOINLINE textract #-}

tsplit :: RTree (d+1) a -> (RTree d a,RTree d a)
tsplit (BR_ l r) = (unsafeCoerce l, unsafeCoerce r)
{-# NOINLINE tsplit #-}

-- | Leaf of a perfect depth tree
--
-- >>> LR 1
-- 1
-- >>> let x = LR 1
-- >>> :t x
-- x :: Num a => RTree 0 a
--
-- Can be used as a pattern:
--
-- >>> let f (LR a) (LR b) = a + b
-- >>> :t f
-- f :: Num a => RTree 0 a -> RTree 0 a -> a
-- >>> f (LR 1) (LR 2)
-- 3
pattern LR :: a -> RTree 0 a
pattern LR x <- (textract -> x)
  where
    LR x = LR_ x

-- | Branch of a perfect depth tree
--
-- >>> BR (LR 1) (LR 2)
-- <1,2>
-- >>> let x = BR (LR 1) (LR 2)
-- >>> :t x
-- x :: Num a => RTree 1 a
--
-- Case be used a pattern:
--
-- >>> let f (BR (LR a) (LR b)) = LR (a + b)
-- >>> :t f
-- f :: Num a => RTree 1 a -> RTree 0 a
-- >>> f (BR (LR 1) (LR 2))
-- 3
pattern BR :: RTree d a -> RTree d a -> RTree (d+1) a
pattern BR l r <- ((\t -> (tsplit t)) -> (l,r))
  where
    BR l r = BR_ l r
