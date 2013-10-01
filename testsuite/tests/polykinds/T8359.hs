{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses #-}
module T8359 where

class DifferentTypes a b

type DifferentTypes3 a b c = (DifferentTypes a b, DifferentTypes b c, DifferentTypes a c)

class Foo a

class Bar a

-- Buggy instance requires UndecidableInstances to compile

instance (DifferentTypes3 a b c, Bar a, Bar b, Bar c) => Foo (a, b, c)

-- Equivalent instance compiles when manually expanding constraint type
-- instance (DifferentTypes a b, DifferentTypes b c, DifferentTypes a c, Bar a, Bar b, Bar c) => Foo (a, b, c)
