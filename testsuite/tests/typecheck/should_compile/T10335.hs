{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ConstraintKinds #-}

module Foo where

type X a = (Eq a, Show a)

class Eq a => C a b

-- HEAD was unable to find the (Eq a) superclass
-- for a while in March/April 2015
instance X a => C a [b]





