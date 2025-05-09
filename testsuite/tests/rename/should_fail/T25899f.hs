{-# LANGUAGE ExplicitNamespaces #-}

module T25899f where

import T25899f_helper
  ( T(type X, Y, type Z)          -- rejected: X,Z not in the type namespace
  , type (#)(data F, G, data H)   -- rejected: F,H not in the data namespace
  )
