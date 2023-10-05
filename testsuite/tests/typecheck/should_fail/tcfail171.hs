module ShouldFail where

-- This one made GHC fall over on implication constraints
-- Silly, but one more test does no harm

import Text.Printf

phex :: a -> b
phex x = printf "0x%x" x
