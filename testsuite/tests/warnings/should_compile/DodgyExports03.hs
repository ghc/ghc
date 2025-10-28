{-# LANGUAGE ExplicitNamespaces #-}

module DodgyExports03
  ( data MkR(..)  -- data constructors never have children ('fld' belongs to 'R')
  ) where

data R = MkR { fld :: Int }
