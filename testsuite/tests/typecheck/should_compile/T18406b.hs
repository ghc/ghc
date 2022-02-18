{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module Bug where

class C a b | a -> b where
  op :: a -> a

foo :: forall a b. C a b => a -> b -> a
foo x y = blah x
  where
    -- GHC should infer
    -- blah :: a -> a
    -- and not
    -- blah :: forall b0. C a b0 => a -> a
    blah z = [x,z] `seq` op z
