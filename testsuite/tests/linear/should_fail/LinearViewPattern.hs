{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ViewPatterns #-}

module LinearViewPattern where

-- This is probably inessential. We are just protecting against potential
-- incorrect Core being emitted by the desugarer. When we understand linear view
-- pattern better, we will probably want to remove this test.

f :: Bool ->. Bool
f (not -> True) = True
