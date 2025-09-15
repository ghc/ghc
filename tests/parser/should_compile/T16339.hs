{-# LANGUAGE ExplicitNamespaces, TypeOperators, RoleAnnotations #-}
{-# OPTIONS -Wno-duplicate-exports #-}

module T16339
  (
    type (!),
    type (!)(Bang),
    type (!)(..),
    type (.),
    type (.)(Dot),
    type (.)(..),
  ) where

data a ! b = Bang
data f . g = Dot

type role (!) phantom phantom
type role (.) phantom phantom
