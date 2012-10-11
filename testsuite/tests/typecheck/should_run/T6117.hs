{-# LANGUAGE FlexibleContexts #-}

{-
[Summary of the program] Ring is defined as a subclass of Semigroup,
inheriting multiplication.  Additive is a wrapper that extracts the additive
structure of Ring and reifies it as Semigroup.  For simplicity, the code omits
ring operations (+ and *) and defines only the additive and multiplicative
identities.

[The bug] If there is a cyclic class hierarchy like

    class B a => Semigroup a                  where ...
    class Semigroup (Additive a) => Ring a    where ...
    instance Ring a => Semigroup (Additive a) where ...

then uses of B's methods on (Additive a) in the method implementations of the
third declaration "instance Ring a => Semigroup (Additive a)" will:

    1. be accepted by the compiler even in cases where B (Additive a) is not
       derivable.
    2. result in <<loop>>.
 -}

class B a where
    b :: a
class B a => Semigroup a where
    unit :: a
class (Semigroup a, Semigroup (Additive a)) => Ring a where
    zero :: a
newtype Additive a = Additive a

-- The source compiles whether with or without this instance declaration in GHC
-- 7.2.1 - 7.4.1 and produces <<loop>>.
--
-- GHC 7.0.4 rejects this source without this declaration and produces
-- terminating code with the declaration.
instance B a => B (Additive a) where
   b = Additive b

instance Ring a => Semigroup (Additive a) where
    unit = b -- Use a method of type (B a => ...) with a instantiated as
             -- (Additive a).  This causes <<loop>>.


-- Now try to instantiate Ring and evaluate `unit'.
instance B Int where
    b = 1234567890
instance Semigroup Int where
    unit = 1
instance Ring Int where
    zero = 0
main = case (unit :: Additive Int) of -- Force the additive identity of Int.
         Additive x -> print x
