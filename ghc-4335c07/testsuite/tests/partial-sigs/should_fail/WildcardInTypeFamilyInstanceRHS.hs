{-# LANGUAGE PartialTypeSignatures, TypeFamilies, InstanceSigs #-}
module WildcardInTypeFamilyInstanceRHS where

class Foo k where
  type Dual k :: *

instance Foo Int where
  type Dual Int = Maybe _
