{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, TypeFamilies,
             UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module T12175 where

import GHC.Exts

class Foo a
instance Foo a => Foo (a,b)

type family TElt r :: Constraint
type instance TElt r = (Foo r, ())
-- type TElt r = (Foo r, Eq r)

data CT r = CT [r]

toCT :: Foo r => CT r -> CT r
toCT = undefined

foo :: CT (a,b) -> (CT a, CT b)
foo = undefined

unzipT :: TElt (a,b) => CT (a,b) -> (CT a, CT b)
unzipT x = foo (toCT x)

{-  Type checking for unzipT

[G] TElt (a,b)
    --> {by rewrite}                 (Foo (a,b), ())
    --> {by superclasses of tuple}   Foo (a,b), ()

toCT @(a,b)
    --> [W] Foo (a,b)
    --> {by instance decl}  Foo a    (insoluble)
 -}
