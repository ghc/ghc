{-# LANGUAGE NoNumericUnderscores #-}

-- Test for NumericUnderscores extension.
-- See #14473
-- This is a testcase for floating literal
-- in NO NumericUnderscores extension.

module NoNumericUnderscores1 where

f :: Float -> ()
f 1_000.0_1 = ()
f _   = ()
