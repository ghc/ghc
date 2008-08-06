
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module GHC.Types where

import GHC.Prim
-- We need Inl etc behind the scenes for the type definitions
import GHC.Generics ()

infixr 5 :

data [] a = [] | a : [a]

data Char = C# Char#

data Int = I# Int#
-- ^A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
data Float      = F# Float#

-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
data Double     = D# Double#

