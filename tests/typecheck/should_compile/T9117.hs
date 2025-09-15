{-# LANGUAGE RoleAnnotations #-}

-- Also see Note [Order of Coercible Instances]

module T9117 where

import Data.Coerce

newtype Phant a = MkPhant Char
type role Phant representational

ex1 :: Phant Bool
ex1 = coerce (MkPhant 'x' :: Phant Int)
