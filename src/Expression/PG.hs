{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Expression.PG (
    PG,
    module Control.Monad,
    module Control.Applicative,
    module Expression.Predicate,
    rewrite, bimap, (|>), (?), (??),
    mproduct,
    support, whenExists,
    fromList, fromOrderedList
    ) where

import Control.Monad
import Control.Applicative
import Expression.Predicate

-- A basic Parameterised Graph datatype
-- * p is the type of predicates
-- * v is the type of vertices
data PG p v = Epsilon
            | Vertex v
            | Overlay (PG p v) (PG p v)
            | Sequence (PG p v) (PG p v)
            | Condition p (PG p v)
            deriving Eq -- TODO: create a proper Eq instance

(|>) :: PG p v -> PG p v -> PG p v
(|>) = Sequence

(?) :: p -> PG p v -> PG p v
(?) = Condition

(??) :: Predicate p => p -> (PG p v, PG p v) -> PG p v
(??) p (t, f) = Overlay (p ? t) (not p ? f)

infixl 7 |>
infixr 8 ?
infixr 8 ??

-- A (fold like) rewrite of a PG according to given instructions
rewrite ::                      r  -- how to rewrite epsilon
        -> (v                -> r) -- how to rewrite vertices
        -> (PG p v -> PG p v -> r) -- how to rewrite overlays
        -> (PG p v -> PG p v -> r) -- how to rewrite sequences
        -> (p      -> PG p v -> r) -- how to rewrite conditions
        -> PG p v                  -- PG to rewrite
        -> r                       -- result
rewrite fe fv fo fs fc pg = case pg of
    Epsilon       -> fe            -- Epsilon is preserved
    Vertex      v -> fv v
    Overlay   l r -> fo l r
    Sequence  l r -> fs l r
    Condition l r -> fc l r

instance Monad (PG p) where
    return   = Vertex
    pg >>= f = rewrite Epsilon f fo fs fc pg
      where
        fo l r = Overlay   (l >>= f) (r >>= f)
        fs l r = Sequence  (l >>= f) (r >>= f)
        fc l r = Condition l         (r >>= f)

instance Functor (PG p) where
    fmap = liftM

bimap :: (p -> q) -> (v -> w) -> PG p v -> PG q w
bimap f g = rewrite Epsilon fv fo fs fc
  where
    fv v   = Vertex    (g v        )
    fo l r = Overlay   (bimap f g l) (bimap f g r)
    fs l r = Sequence  (bimap f g l) (bimap f g r)
    fc l r = Condition (f l        ) (bimap f g r)

instance Applicative (PG p) where
    pure = return
    (<*>) = ap

instance MonadPlus (PG p) where
    mzero = Epsilon
    mplus = Overlay

instance Alternative (PG p) where
    empty = Epsilon
    (<|>) = Overlay

mproduct :: [PG p v] -> PG p v
mproduct = foldr (|>) Epsilon

fromList :: [v] -> PG p v
fromList = msum . map return

fromOrderedList :: [v] -> PG p v
fromOrderedList = mproduct . map return

-- Returns sorted list of all vertices that appear in a PG
support :: Ord v => PG p v -> [v]
support = rewrite [] fv fos fos fc
  where
    fv    v = [v]
    fos l r = support l `union` support r
    fc  _ r = support r

union :: Ord v => [v] -> [v] -> [v]
union ls     []     = ls
union []     rs     = rs
union (l:ls) (r:rs) = case compare l r of
    LT -> l : union ls (r:rs)
    EQ -> l : union ls rs
    GT -> r : union (l:ls) rs

-- Given a vertex and a PG return a predicate, which tells when the vertex
-- exists in the PG.
whenExists :: (Predicate p, Eq v) => v -> PG p v -> p
whenExists _ Epsilon         = false
whenExists a (Vertex b)      = if a == b then true else false
whenExists a (Overlay   l r) = whenExists a l || whenExists a r
whenExists a (Sequence  l r) = whenExists a l || whenExists a r
whenExists a (Condition x r) = x              && whenExists a r

instance (Show p, Show v) => Show (PG p v) where
    showsPrec _ Epsilon       = showString "()"
    showsPrec _ (Vertex v)    = shows v

    showsPrec d (Overlay l r) =
        showParen (d > 0) $ shows l . showChar ' ' . shows r

    showsPrec d (Sequence l r) =
        showParen (d > 1) $ showsPrec 1 l . showString " -> " . showsPrec 1 r

    showsPrec d (Condition l r) =
        showChar '[' . shows l . showChar ']' . showsPrec 2 r
