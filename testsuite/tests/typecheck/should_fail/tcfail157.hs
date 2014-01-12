{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}
-- NB: *no* UndecidableInstances

-- This one (due to Oleg) made 6.4.1 go into a loop in the typechecker,
-- despite the lack of UndecidableInstances
--
-- The example corresponds to a type function (realized as a class E 
-- with functional dependencies) in the context of an instance. 
-- The function in question is
--
--	class E m a b | m a -> b
--	instance E m (() -> ()) (m ())
--
-- We see that the result of the function, "m ()" is smaller (in the
-- number of constructors) that the functions' arguments, "m" and
-- "() -> ()" together. Plus any type variable free in the result is also
-- free in at least one of the arguments. And yet it loops.
module ShouldFail where

class Foo m a where
    foo :: m b -> a -> Bool

instance Foo m () where
    foo _ _ = True

instance (E m a b, Foo m b) => Foo m (a->()) where
    foo m f = undefined

class E m a b | m a -> b where
    tr :: m c -> a -> b

-- There is only one instance of the class with functional dependencies
instance E m (() -> ()) (m ()) where
    tr x = undefined

-- GHC(i) loops

test = foo (\f -> (f ()) :: ()) (\f -> (f ()) :: ())
