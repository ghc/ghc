{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Expression.PG (
    module Expression.Predicate,
    PG (..), (?), (??), whenExists
    ) where

import Expression.Predicate

-- A generic Parameterised Graph datatype
-- * p is the type of predicates
-- * v is the type of vertices
data PG p v = Epsilon
            | Vertex v
            | Overlay (PG p v) (PG p v)
            | Sequence (PG p v) (PG p v)
            | Condition p (PG p v)

(?) :: p -> PG p v -> PG p v
(?) = Condition

infixl 7 ?

(??) :: Predicate p => p -> (PG p v, PG p v) -> PG p v
(??) p (t, f) = Overlay (p ? t) (not p ? f)

infixl 7 ??

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
