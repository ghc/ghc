{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DatatypeContexts #-}

module ShouldCompile where

data Trivial a = Trivial
   deriving (Functor)

data Fun a = Fun (Int -> a)
  deriving (Functor)

-- lots of different things
data Strange a b c
    = T1 a b c
    | T2 [a] [b] [c]         -- lists
    | T3 [[a]] [[b]] [[c]]   -- nested lists
    | T4 (c,(b,b),(c,c))     -- tuples
    | T5 ([c],Strange a b c) -- tycons
    | T6 (Int -> c)          -- function types
    | T7 (a -> (c,a))        -- functions and tuples 
    | T8 ((c -> a) -> a)     -- continuation
  deriving (Functor)

data NotPrimitivelyRecursive a
    = S1 (NotPrimitivelyRecursive (a,a))
    | S2 a
  deriving (Functor,Eq)

data Eq a => StupidConstraint a b = Stupid a b
  deriving (Functor)

-- requires Functor constraint on f and g
data Compose f g a = Compose (f (g a))
  deriving (Functor)

-- We can't derive Functor for the following type.
-- it needs both (Functor (f Int)) and (Functor (f Bool))
-- i.e.:
--  instance (Functor (f Bool), Functor (f Int)) => Functor (ComplexConstraint f)
-- This requires FlexibleContexts and UndecidableInstances
data ComplexConstraint f a = ComplexContraint (f Int (f Bool a,a))
--  deriving (Functor)

data Universal a
    = Universal  (forall b. (b,[a]))
    | Universal2 (forall f. Functor f => (f a))
    | Universal3 (forall a. a -> Int) -- reuse a
    | NotReallyUniversal (forall b. a)
  deriving (Functor)

-- Ghc doesn't allow deriving for non-Haskell98 data constructors
data Existential b
    = forall a. ExistentialList [a]
    | forall f. Functor f => ExistentialFunctor (f b)
    | forall b. SneakyUseSameName (b -> Bool)
  -- deriving (Functor)

-- Don't get confused by synonyms
type IntFun a = Int -> a
data IntFunD a = IntFunD (IntFun a)
  deriving (Functor)

