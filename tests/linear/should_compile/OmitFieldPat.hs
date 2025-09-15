{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}

module OmitFieldPat where

import GHC.Types

-- This tests that we are indeed allowed to drop fields of multiplicity Many. At
-- time of writing this test, there is no syntax for non-linear fields in
-- record. Instead use a regular constructor and the empty record pattern.
data T where
  MkT :: Int %Many -> Bool %Many -> T

good :: T %1 -> ()
good (MkT{}) = ()
