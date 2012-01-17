-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2011
--
-- CmmLint: checking the correctness of Cmm statements and expressions
--
-----------------------------------------------------------------------------

module CmmLint (
    cmmLint
  ) where

import Cmm
import Outputable

cmmLint :: CmmGraph -> IO ()
cmmLint g = return () -- TODO!!

-- Things to check:
--     - invariant on CmmBlock in CmmExpr (see comment there)
--     - check for branches to blocks that don't exist
--     - check types
