{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Expression.PG (
    module Expression.Predicate,
    PG (..),
    bimap, (|>), (?), (??), whenExists, support,
    msum, mproduct,
    fromList, fromOrderedList
    ) where

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

bimap :: (p -> q) -> (v -> w) -> PG p v -> PG q w
bimap _ _ Epsilon = Epsilon
bimap f g (Vertex      v) = Vertex    (g v)
bimap f g (Overlay   l r) = Overlay   (bimap f g l) (bimap f g r)
bimap f g (Sequence  l r) = Sequence  (bimap f g l) (bimap f g r)
bimap f g (Condition l r) = Condition (f l)         (bimap f g r)

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

support :: Ord v => PG p v -> [v]
support Epsilon         = []
support (Vertex      v) = [v]
support (Overlay   l r) = support l `union` support r
support (Sequence  l r) = support l `union` support r
support (Condition _ r) = support r

union :: Ord v => [v] -> [v] -> [v]
union ls     []     = ls
union []     rs     = rs
union (l:ls) (r:rs) = case compare l r of
    LT -> l : union ls (r:rs)
    EQ -> l : union ls rs
    GT -> r : union (l:ls) rs

instance (Show p, Show v) => Show (PG p v) where
    showsPrec _ Epsilon       = showString "()"
    showsPrec _ (Vertex v)    = shows v

    showsPrec d (Overlay l r) =
        showParen (d > 0) $ shows l . showChar ' ' . shows r

    showsPrec d (Sequence l r) =
        showParen (d > 1) $ showsPrec 1 l . showString " -> " . showsPrec 1 r

    showsPrec d (Condition l r) =
        showChar '[' . shows l . showChar ']' . showsPrec 2 r
