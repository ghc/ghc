{-# OPTIONS -fglasgow-exts #-}

-- This module requires quite trick desugaring,
-- because of the context in the existentials
-- It broke a pre 6.4 GHC

module Foo where

   import Data.Generics
   import Data.HashTable

   data Item = forall a. (Data a) => Leaf Bool a
             | forall a. (Data a) => Branch Bool a Int Int
	     deriving (Typeable)


   instance Data Item where
       gfoldl k z (Leaf b v) = z (Leaf b) `k` v
       gfoldl k z (Branch b v a1 a2) = z (\x -> Branch b x a1 a2) `k` v
       gunfold _ _ _ = error "urk"
       toConstr (Leaf _ _) = leafConstr
       toConstr (Branch _ _ _ _) = branchConstr
       dataTypeOf _ = itemDataType

   itemDataType = mkDataType "Subliminal.Item" [leafConstr, branchConstr]
   leafConstr = mkConstr itemDataType "Leaf" [] Prefix
   branchConstr = mkConstr itemDataType "Branch" [] Prefix



