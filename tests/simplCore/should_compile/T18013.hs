{-# OPTIONS_GHC -ddump-rule-firings -ddump-simpl
                -dsuppress-coercions -dsuppress-uniques #-}
{-# LANGUAGE Arrows #-}

module T18013 where

import Control.Arrow
import T18013a

-- We want to ensure this generates good code. Uses of (.) should be
-- specialized and inlined, and the rules defined on mkRule should fire.

mapMaybeRule :: Rule IO a b -> Rule IO (Maybe a) (Maybe b)
mapMaybeRule f = proc v -> case v of
  Just x -> do
    y <- f -< x
    returnA -< Just y
  Nothing -> returnA -< Nothing
{-# NOINLINE mapMaybeRule #-}
  -- The size of mapMaybeRule is very close to the inlining threshold.
  -- The NOINLINE consistently forces a worker/wrapper split to make
  -- the test output more stable.
