{-# LANGUAGE PartialTypeSignatures, TypeFamilies, InstanceSigs #-}
module WildcardInTypeFamilyInstanceLHS where

class Foo k where
  type Dual k :: *

instance Foo Int where
  type Dual _ = Maybe Int
