{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

{-|
This module exports:

  - The 'TypeError' type family, which is used to provide custom type
    errors. This is a type-level analogue to the term level error function.
  - The 'ErrorMessage' kind, used to define custom error messages.
  - The 'Unsatisfiable' constraint, a more principled variant of 'TypeError'
    which gives a more predictable way of reporting custom type errors.

@since 4.17.0.0
-}

module GHC.TypeError
  ( ErrorMessage (..)
  , TypeError
  , Assert
  , Unsatisfiable, unsatisfiable
  ) where

import GHC.Internal.TypeError
