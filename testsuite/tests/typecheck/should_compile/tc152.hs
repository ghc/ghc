{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances #-}
-- -XUndecidableInstances now needed because the Coverage Condition fails

-- This one blew up Hugs (Apr 02)

module ShouldCompile where

--  Context reduction can introduce opportunities for context improvement,
--  so add an additional `improve' step afterwards.  The bug is demonstrated by
--  the following code:
  
  class C a b c | a b -> c where
    m :: a -> b -> c
  
  instance C Integer Integer Integer where
    m = error "urk" 
 
  newtype T a = T a
  
  instance C a b c => C (T a) (T b) (T c) where
    m = error "urk" 
  
  i :: T Integer
  i = undefined
  
  x = m (m i i) i -- This line blows up w/ unresolved top-level overloading

