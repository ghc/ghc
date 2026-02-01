{-# LANGUAGE QualifiedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Test where

import qualified Example.Length as Length

-- Test warning for redundant pattern match
foo :: Int -> ()
foo Length."abc" = ()
foo other =
  case other of
    Length."abc" -> ()
    Length."def" -> ()
    _ -> ()
