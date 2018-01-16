{-# LANGUAGE RoleAnnotations, DatatypeContexts, IncoherentInstances,
             FlexibleInstances #-}

module T8958 where

class Nominal a
instance Nominal a

class Representational a
instance Representational a
type role Representational representational

newtype (Nominal k, Representational v) => Map k v = MkMap [(k,v)]
