-- Test that the misplaced LANGUAGE pragma results in a warning but doesn't
-- cause the program to be rejected.
module T12002 where

{-# LANGUAGE OverloadedStrings #-}
