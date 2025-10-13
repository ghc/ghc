{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, RankNTypes,
             ConstraintKinds, QuantifiedConstraints, TypeOperators #-}

module T15359 where

class C a b

data Dict c where
  Dict :: c => Dict c

foo :: (forall a b. C a b => a ~ b) => Dict (C a b) -> a -> b
foo Dict x = x
-- This is a terrible test.  If we are trying to solve
--   [W] t1 ~ t2
-- the quantified constraint will fire, producing
--   [W] C t1 t2
-- But it would also fire the other way round producing
--   [W] C t2 t1
-- So it's a coincidence whether or not it typechecks!
