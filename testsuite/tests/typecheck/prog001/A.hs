{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module A where

class Matrix a fa | a -> fa where
    row :: [a] -> fa
