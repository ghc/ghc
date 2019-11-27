{-# LANGUAGE NoBangPatterns #-}

module Proposal229c (f) where

-- should recommend to enable BangPatterns instead of parsing as an infix operator
f !x = x
