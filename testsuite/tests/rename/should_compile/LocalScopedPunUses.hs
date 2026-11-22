{-# LANGUAGE ScopedTypeVariables, TypeAbstractions #-}
module LocalScopedPunUses where

a :: Integer
a = 15

-- The term a shadows the outer type a in a unified namespace.
mixed :: forall a. a -> a
mixed = \a -> (a :: a)

-- The type in the argument signature is read before the term binder.
patternBeforeTerm :: t -> t
patternBeforeTerm @a = \(a :: a) -> a

-- A nearer explicit forall shadows the term binder in a unified namespace.
forallAfterTerm :: t -> Bool
forallAfterTerm a = let f :: forall a. a -> a; f x = x in f a `seq` True

-- Both binders in one complete match compete in its RHS.
sameMatch :: t -> t
sameMatch @p p = (p :: p)

-- A nearer type binder shadows an outer term binder.
termBehindType :: t -> Bool
termBehindType a = let g :: forall a. a -> Bool; g _ = a `seq` True in g a

-- A let binding enters scope before its body.
letShadowsType :: forall a. a -> a
letShadowsType x = let a = x in (a :: a)

-- An explicit local type binder also shadows a top-level term.
globalBehindType :: forall a. a -> Integer
globalBehindType _ = a

-- The implicit type introduced by the pattern cannot shadow the term.
globalWithImplicit :: t -> Integer
globalWithImplicit (_ :: a) = a
