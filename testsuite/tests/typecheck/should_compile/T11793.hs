{-# LANGUAGE ImplicitParams #-}

module T11793 where

class C a where
  op :: (?x::Int) => a -> a

-- Should be OK even without ConstrainedClassMethods
