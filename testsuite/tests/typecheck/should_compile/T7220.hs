{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Test2 where

class C a b | b -> a

data A = A
data X = X
data Y = Y

type family TF b

f :: (forall b. (C a b, TF b ~ Y) => b) -> X
f _ = undefined

u :: (C A b, TF b ~ Y) => b
u = undefined

v :: X
v = (f :: (forall b. (C A b, TF b ~ Y) => b) -> X) u -- This line causes an error (see below)

{-
GHC 7.6.1-rc1 (7.6.0.20120810) rejects this code with the following error message.

Test2.hs:24:52:
    Couldn't match expected type `Y'
                with actual type `TF (forall b. (C A b, TF b ~ Y) => b)'
    In the first argument of `f ::
                                (forall b. (C A b, TF b ~ Y) => b) -> X', namely
      `u'
    In the expression: (f :: (forall b. (C A b, TF b ~ Y) => b) -> X) u
    In an equation for `v':
        v = (f :: (forall b. (C A b, TF b ~ Y) => b) -> X) u

GHC 7.4.1 rejected this code with a different error message:

Test2.hs:24:6:
    Cannot deal with a type function under a forall type:
    forall b. (C A b, TF b ~ Y) => b
    In the expression: f :: (forall b. (C A b, TF b ~ Y) => b) -> X
    In the expression: (f :: (forall b. (C A b, TF b ~ Y) => b) -> X) u
    In an equation for `v':
        v = (f :: (forall b. (C A b, TF b ~ Y) => b) -> X) u
-}