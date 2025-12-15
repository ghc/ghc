{-# LANGUAGE UndecidableSuperClasses, FunctionalDependencies, GADTs #-}

module T21909b where

class C [a] => C a where
  foo :: a -> Int

bar :: C a => a -> Int
bar x = foolocal x
  where
    foolocal a = foo a
