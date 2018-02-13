{-# LANGUAGE NoNumericUnderscores #-}

-- Test for NumericUnderscores extension.
-- See Trac #14473
-- This is a testcase for integer literal
-- in NO NumericUnderscores extension.

module NoNumericUnderscores0 where

f :: Int -> ()
f 1_000 = ()
f _   = ()
