{-# LANGUAGE TypeData #-}
{-# LANGUAGE ExplicitNamespaces #-}

module T25899e2
  ( type T(data MkT)  -- rejected: MkT not in the data namespace
  ) where

type data T = MkT
