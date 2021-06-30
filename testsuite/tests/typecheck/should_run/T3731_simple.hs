{-# LANGUAGE
  FlexibleInstances,
  UndecidableInstances
#-}

module Bug where

class Default a
class Sat a

instance Default a => Sat a

class Sat a => Data a where
  dataTypeOf :: a -> a

defaultDefaultValue :: Data a => a
defaultDefaultValue = res
    where
      res = dataTypeOf res

-- GHC does not infer the principal type for res,
-- inferring (Default a, Data a) => a instead of Data a => a.
-- See Note [Inferring principal types] in Ghc.Tc.Solver
--
-- This used to be fine, as "Default a" was a Derived constraint
-- that could be dropped. Without Deriveds, we instead get a
-- Wanted constraint, which can't be dropped.
-- This means that this program no longer compiles.
-- (Note that a type signature on "res" allows the program to
-- compile again.)
