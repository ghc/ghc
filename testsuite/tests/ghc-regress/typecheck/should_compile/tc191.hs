{-# OPTIONS -fglasgow-exts #-}

-- This only typechecks if forall-hoisting works ok when
-- importing from an interface file.  The type of Twins.gzipWithQ
-- is this:
--   type GenericQ r = forall a. Data a => a -> r
--   gzipWithQ :: GenericQ (GenericQ r) -> GenericQ (GenericQ [r])
-- It's kept this way in the interface file for brevity and documentation,
-- but when the type synonym is expanded, the foralls need expanding

module Foo where

import Data.Generics.Basics
import Data.Generics.Aliases
import Data.Generics.Twins(gzipWithQ)

-- | Generic equality: an alternative to \deriving Eq\
geq :: Data a => a -> a -> Bool
geq x y = geq' x y
  where
    geq' :: forall a b. (Data a, Data b) => a -> b -> Bool
    geq' x y =     (toConstr x == toConstr y)
                && and (gzipWithQ geq' x y)




