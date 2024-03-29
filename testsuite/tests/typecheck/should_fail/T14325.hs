{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

module T14325 where

class (a~b) => C a b

foo :: C a b => a -> b
foo x = x

hm3 :: C (f b) b => b -> f b
hm3 x = foo x

{- Typechecking hm3
~~~~~~~~~~~~~~~~~~~
[G] C (f b) b
[G] f b ~# b    -- Superclass; but Irred because occurs check
[W] C b (f b)

So the wanted can't be solved and is reported
-}