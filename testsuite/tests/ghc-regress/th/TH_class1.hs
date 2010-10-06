{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module TH_class1 where

$( [d| class Classy a b c d | a -> b c, c -> d where
            f :: a -> b -> c -> d
     |] )
