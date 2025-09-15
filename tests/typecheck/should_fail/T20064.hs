{-# LANGUAGE FlexibleContexts, FunctionalDependencies, NoMonomorphismRestriction #-}

module T20064 where

data AB a b = AB

class C a b | a -> b where
  meth :: AB a b -> b

ab :: AB Int b
ab = AB

--foo :: C Int b => b
foo = meth ab
