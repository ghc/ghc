{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable   #-}

module NestedDatatypes () where

{-

We provide an illustrative ScrapYourBoilerplate example for a nested
datatype.  For clarity, we do not derive the Typeable and Data
instances by the deriving mechanism but we show the intended
definitions. The overall conclusion is that nested datatypes do not
pose any challenge for the ScrapYourBoilerplate scheme. Well, this is
maybe not quite true because it seems like we need to allow
undecidable instances.

-}

import Data.Dynamic
import Data.Generics

 
-- A nested datatype
data Nest a = Box a | Wrap (Nest [a]) deriving Typeable


-- The Data instance for the nested datatype
instance (Data a, Data [a]) => Data (Nest a)
  where
    gfoldl k z (Box a)  = z Box `k` a
    gfoldl k z (Wrap w) = z Wrap `k` w
    gmapT f (Box a)  = Box (f a)
    gmapT f (Wrap w) = Wrap (f w)
    toConstr (Box _)  = boxConstr
    toConstr (Wrap _) = wrapConstr
    gunfold k z c = case constrIndex c of
                      1 -> k (z Box)
                      2 -> k (z Wrap)
    dataTypeOf _ = nestDataType

boxConstr    = mkConstr nestDataType "Box"  [] Prefix
wrapConstr   = mkConstr nestDataType "Wrap" [] Prefix
nestDataType = mkDataType "Main.Nest" [boxConstr,wrapConstr]
