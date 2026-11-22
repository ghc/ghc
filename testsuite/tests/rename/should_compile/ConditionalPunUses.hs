{-# LANGUAGE ScopedTypeVariables, TypeAbstractions #-}
module ConditionalPunUses where

a :: Bool
a = True

-- Its pattern signature introduces the type variable before the term binder.
fresh :: t -> t
fresh (x :: x) = (x :: x)

-- The implicit q cannot shadow the earlier term binder.
earlier :: t -> s -> s
earlier q (value :: q) = (value :: q)

-- The pattern introduces a type variable with the name of a global term.
global :: t -> t
global (value :: a) = (value :: a)

-- The outer type variable is a use in the pattern, not a fresh binder.
reused :: forall a. a -> a
reused (value :: a) = (value :: a)

-- An explicit binder introduces a new name even with a competing term.
explicit :: t -> t
explicit @r r = (r :: r)

-- The outer argument is already visible when the inner type is read.
outerPattern :: t -> s -> s
outerPattern q = \(q :: q) -> (q :: q)

-- The imported term is visible before the term pattern introduces id.
fromImport :: t -> t
fromImport (id :: id) = (id :: id)
