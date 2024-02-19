{-# LANGUAGE Safe #-}

module GHC.Char
    (-- *  Utilities
     chr,
     -- *  Monomorphic equality operators
     -- |  See GHC.Classes#matching_overloaded_methods_in_rules
     eqChar,
     neChar
     ) where

import GHC.Internal.Char