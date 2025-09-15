{-# LANGUAGE TypeData #-}
{-# LANGUAGE ExplicitNamespaces #-}

module T25899e1
  ( data MkT  -- rejected: MkT not in the data namespace
  ) where

type data T = MkT
