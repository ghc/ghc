{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}

module T3943 where

-- Note that 'r' is used, in the view pattern
-- The bug was that 'r' was reported unused

test :: ([a], [a]) -> [a]
test x = let (r,(r++) -> rs) = x in rs
