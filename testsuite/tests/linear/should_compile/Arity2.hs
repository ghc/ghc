{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Arity2 where
{-
inplace/bin/ghc-stage0 -O2 -dcore-lint
-}

--import GHC.Base

data Id a = Id a

(<$>) :: (a -> b) -> Id a -> Id b
(<$>) f (Id a) = Id (f a)

(<*>) :: Id (a -> b) -> Id a -> Id b
(<*>) (Id a) (Id b) = Id (a b)

data Q = Q () ()
data S = S ()

-- Q only gets eta-expand once and then trapped
foo = Q <$> Id () <*> Id ()

-- This compiles fine
foo2 = S <$> Id ()

{-
[1 of 1] Compiling Arity2           ( linear-tests/Arity2.hs, linear-tests/Arity2.o )

linear-tests/Arity2.hs:21:7: error:
    • Couldn't match type ‘() ⊸ Q’ with ‘() -> b’
      Expected type: Id (() -> b)
        Actual type: Id (() ⊸ Q)
    • In the first argument of ‘(<*>)’, namely ‘Q <$> Id ()’
      In the expression: Q <$> Id () <*> Id ()
      In an equation for ‘foo’: foo = Q <$> Id () <*> Id ()
    • Relevant bindings include
        foo :: Id b (bound at linear-tests/Arity2.hs:21:1)
   |
21 | foo = Q <$> Id () <*> Id ()
   |       ^^^^^^^^^^^
-}
