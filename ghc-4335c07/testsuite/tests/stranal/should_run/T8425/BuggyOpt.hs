module BuggyOpt
    (
      addSequence, -- induces inliner bug, but not used anywhere
    ) where

import M
import Prelude hiding (lookup)

addSequence :: Map (Array Int) Int -> Map (Array Int) Int
addSequence seqs =
  seq (isJust (lookup seq_ seqs)) (insert seq_ 5 seqs)

seq_ = array (2,2) [ (2,3)]
