-- Test boxing at a 'TupleRep' with more than 'mAX_TUPLE_SIZE' (64) components,
-- which requires chunking the boxed tuple.

-- An unboxed tuple with that many components can't be written in source,
-- so the plugin T27331b_Plugin builds the Core for 'roundtrip' instead.
{-# OPTIONS_GHC -fplugin T27331b_Plugin #-}

module Main where

-- Replaced by the plugin with the result of boxing and unboxing (# 0#, ..., 99# #).
roundtrip :: [Int]
roundtrip = []
{-# NOINLINE roundtrip #-}

main :: IO ()
main = print roundtrip
