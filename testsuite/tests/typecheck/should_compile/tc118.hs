{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances #-}

-- !!! An instance decl with a context containing a free type variable
-- The interest here is that there's a "b" in the instance decl
-- context that isn't mentioned in the instance head.  
-- Hence UndecidableInstances

module ShouldCompile where

class HasConverter a b | a -> b where
   convert :: a -> b
 
data Foo a = MkFoo a

instance (HasConverter a b,Show b) => Show (Foo a) where
   show (MkFoo value) = show (convert value)

