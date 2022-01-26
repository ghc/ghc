{-# LANGUAGE RoleAnnotations #-}
module T20999 where

type T a = Int

type role M nominal phantom
data M f a = MkM (f (T a))

type role N nominal phantom
data N f a = MkN (f Int)
