{-# OPTIONS -fglasgow-exts #-}

-- !!! Scoped type variables in result signatures
-- This one is a bit crafty

module ShouldCompile where

import PrelST
import PrelArr

-- Note the *pattern* type sig on f, which forces it
-- to be monomorphic; but the separate type sig makes
-- it polymorphic; hence the error.
f:: ST t Int
f:: ST s Int = do
    v <- newSTRef 5
    let g :: ST s Int
             -- ^ should be in scope
        g = readSTRef v
    g
