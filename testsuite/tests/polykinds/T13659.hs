{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeOperators, DataKinds, FlexibleInstances #-}

{- Defines a C-like printf function using DataKinds extensions. -}

module T13659 where

-- format string parameterized by a list of types
data Format (fmt :: [*]) where
  X :: Format '[]                   -- empty string, i.e. ""
  L :: a -> String -> Format '[]    -- string literal, e.g. "hello"
  S :: a -> Format '[String]        -- "%s"
  I :: Format a -> Format '[Int, a] -- "%d"
