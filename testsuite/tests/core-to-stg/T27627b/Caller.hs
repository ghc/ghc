module Caller where

import Callee

-- b ignores its dictionary, so a's is absent.  Worker/wrapper must not make a
-- filler: b speculates a superclass selection out of it.
{-# NOINLINE a #-}
a :: UQ f => f Int -> Int
a x = b x + 1
