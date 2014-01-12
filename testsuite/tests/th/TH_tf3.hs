{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}

module TH_tf3 where

type family T a

$( [d| foo :: T [a] ~ Bool => a -> a
       foo x = x |] )

$( [d| class C a 
       instance a ~ Int => C a |] )