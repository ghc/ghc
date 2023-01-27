{-# LANGUAGE LambdaCase, GADTs, ScopedTypeVariables, TypeAbstractions #-}

module T19847a where

data T a b c where
  MkT :: forall c y x b. (x~y, c~[x], Ord x) => x -> y -> T (x,y) b c

f :: forall b c. (T (Int,Int) b c -> Bool) -> (b,c)
f = error "urk"

h = f (\case { MkT @_ @_ @_ @Int p q -> True })
-- Check that the @Int argument can affect
-- the type at which `f` is instantiated
-- So h :: forall c. (Int,c)
