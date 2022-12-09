{-# LANGUAGE QuantifiedConstraints #-}
module T22570 where

import Data.Kind

class SomeClass a
class OtherClass

type SomeClassUnit = OtherClass => SomeClass () :: Constraint

instance SomeClassUnit

type SomeClassSyn a = OtherClass => SomeClass a :: Constraint

instance SomeClassSyn ()
