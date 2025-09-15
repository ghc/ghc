{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module T15232_Unsat where

import GHC.TypeError

class C a where f :: a -> a
instance {-# OVERLAPPING #-} C Int where f _ = 42
instance {-# OVERLAPPABLE #-} Unsatisfiable ( 'Text "Only Int is supported" ) => C a where f = undefined

main :: IO ()
main = print $ f (42::Int)
