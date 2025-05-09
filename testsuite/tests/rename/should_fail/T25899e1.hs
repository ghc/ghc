{-# LANGUAGE TypeData #-}
{-# LANGUAGE ExplicitNamespaces #-}

module T25899e1
  ( data MkT  -- rejected: MkT not in data namespace
  ) where

type data T = MkT
