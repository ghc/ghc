{-# LANGUAGE RoleAnnotations #-}

module T10285a (N, coercion) where

import Data.Type.Coercion

newtype N a = MkN Int
type role N representational

coercion :: Coercion (N a) (N b)
coercion = Coercion
