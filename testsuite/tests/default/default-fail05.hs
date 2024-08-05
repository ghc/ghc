-- | Fail if two constraints on the same type can be resolved to two different types

{-# LANGUAGE Haskell2010, NamedDefaults #-}

import Data.Foldable (Foldable, toList)

default Foldable (Maybe, [])
default Functor ([], Maybe)

main = do
  print (toList $ pure 21)
  traverse print (pure 42)
