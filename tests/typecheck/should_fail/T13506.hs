{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module Bug where

class FunDep lista a | lista -> a
instance FunDep [a] a

singleton :: FunDep lista a => a -> lista
singleton _ = undefined

-- this error is expected:
--   Couldn't match type 'Char' with '()'
--   arising from a functional dependency between
--     constraint 'FunDep [Char] ()' arising from a use of 'singleton'
--     instance 'FunDep [a] a'
illTyped :: [Char]
illTyped = singleton ()
  {-  [W] FunDep [Char] () -}

-- but this one is not:
--   Couldn't match type '()' with 'Char'
--     arising from a functional dependency between constraints:
--       'FunDep [Char] Char' arising from a use of 'singleton' (in 'wellTyped')
--       'FunDep [Char] ()' arising from a use of 'singleton' (in 'illTyped')
wellTyped :: [Char]
wellTyped = singleton 'a'
  {-   [W] FunDep [Char] Char -}
