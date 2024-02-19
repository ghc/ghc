{-# LANGUAGE MagicHash #-}

-- |
-- Compatibility module for pre-@ghc-bignum@ code.

module GHC.Integer.Logarithms
    (wordLog2#,
     integerLog2#,
     integerLogBase#
     ) where

import GHC.Internal.Integer.Logarithms
