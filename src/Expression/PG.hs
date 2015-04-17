{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Expression.PG (
    module Expression.Simplify,
    module Expression.Predicate,
    PG (..),
    (|>), (?), (??), whenExists,
    msum, mproduct,
    fromList, fromOrderedList
    ) where

import Expression.Simplify
import Data.Functor
import Control.Monad
import Control.Applicative
import Expression.Predicate

-- A generic Parameterised Graph datatype
-- * p is the type of predicates
-- * v is the type of vertices
data PG p v = Epsilon
            | Vertex v
            | Overlay (PG p v) (PG p v)
            | Sequence (PG p v) (PG p v)
            | Condition p (PG p v)
            deriving Eq -- TODO: create a proper Eq instance

instance Functor (PG p) where
    fmap = liftM

instance Applicative (PG p) where
    pure = return
    (<*>) = ap

instance Monad (PG p) where
    return = Vertex

    Epsilon       >>= _ = Epsilon
    Vertex    v   >>= f = f v
    Overlay   l r >>= f = Overlay   (l >>= f) (r >>= f)
    Sequence  l r >>= f = Sequence  (l >>= f) (r >>= f)
    Condition l r >>= f = Condition l         (r >>= f)

instance MonadPlus (PG p) where
    mzero = Epsilon
    mplus = Overlay

instance Alternative (PG p) where
    empty = Epsilon
    (<|>) = Overlay

(|>) :: PG p v -> PG p v -> PG p v
(|>) = Sequence

mproduct :: [PG p v] -> PG p v
mproduct = foldr (|>) Epsilon

fromList :: [v] -> PG p v
fromList = msum . map return

fromOrderedList :: [v] -> PG p v
fromOrderedList = mproduct . map return

infixl 7 |>

(?) :: p -> PG p v -> PG p v
(?) = Condition

infixl 8 ?

(??) :: Predicate p => p -> (PG p v, PG p v) -> PG p v
(??) p (t, f) = Overlay (p ? t) (not p ? f)

infixl 8 ??

-- Given a vertex and a PG return a predicate, which tells when the vertex
-- exists in the PG.
whenExists :: (Predicate p, Eq v) => v -> PG p v -> p
whenExists _ Epsilon         = false
whenExists a (Vertex b)      = if a == b then true else false
whenExists a (Overlay   l r) = whenExists a l || whenExists a r
whenExists a (Sequence  l r) = whenExists a l || whenExists a r
whenExists a (Condition x r) = x              && whenExists a r

-- Map over all PG predicates, e.g., partially evaluate a given PG.
--mapP :: (p -> p) -> PG p v -> PG p v
--mapP _ Epsilon         = Epsilon
--mapP _ v @ (Vertex _)  = v
--mapP f (Overlay   l r) = Overlay   (mapP f l) (mapP f r)
--mapP f (Sequence  l r) = Sequence  (mapP f l) (mapP f r)
--mapP f (Condition x r) = Condition (f x     ) (mapP f r)

instance (Show p, Show v) => Show (PG p v) where
    showsPrec _ Epsilon       = showString "()"
    showsPrec _ (Vertex v)    = shows v

    showsPrec d (Overlay l r) =
        showParen (d > 0) $ shows l . showChar ' ' . shows r

    showsPrec d (Sequence l r) =
        showParen (d > 1) $ showsPrec 1 l . showString " -> " . showsPrec 1 r

    showsPrec d (Condition l r) =
        showChar '[' . shows l . showChar ']' . showsPrec 2 r

instance (Simplify p, Predicate p, Eq p, Eq v) => Simplify (PG p v) where
    simplify Epsilon = Epsilon
    simplify v @ (Vertex _) = v
    simplify (Overlay l r)
        | l' == Epsilon = r'
        | r' == Epsilon = l'
        | l' == r'      = l'
        | otherwise     = Overlay l' r'
      where
        l' = simplify l
        r' = simplify r
    simplify (Sequence l r)
        | l' == Epsilon = r'
        | r' == Epsilon = l'
        | otherwise     = Sequence l' r'
      where
        l' = simplify l
        r' = simplify r
    simplify (Condition l r)
        | l' == true    = r'
        | l' == false   = Epsilon
        | r' == Epsilon = Epsilon
        | otherwise     = Condition l' r'
      where
        l' = simplify l
        r' = simplify r


