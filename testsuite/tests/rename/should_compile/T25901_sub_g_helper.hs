{-# LANGUAGE ExplicitNamespaces #-}

module T25901_sub_g_helper
  ( type T (..)  -- exports T and MkT
  ) where

data T = MkT