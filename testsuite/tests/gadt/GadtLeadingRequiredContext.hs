{-# LANGUAGE ExplicitNamespaces, GADTs, RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions, TypeApplications, ViewPatterns #-}
module Main where

data T where
  C :: forall a -> Eq a => forall {b} c. Show c => a -> (b, c) -> T

check :: T -> (Bool, String)
check (C (type a) @c ((\x -> x == x) -> same) ((\(_, x) -> show x) -> shown)) =
  (same, shown)

main :: IO ()
main = print (check (C (type Int) @Bool 3 ('x', True)),
              check (C (type Double) @Char (0/0) (False, 'y')))
