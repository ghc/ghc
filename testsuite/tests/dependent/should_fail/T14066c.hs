{-# LANGUAGE TypeFamilies, TypeInType, UndecidableInstances #-}

module T14066c where

type family H a b where
  H a b = H b a

type X = H True Nothing  -- this should fail to kind-check.
                         -- if it's accepted, then we've inferred polymorphic recursion for H
