{-# LANGUAGE LinearTypes #-}

module LinearPolyDollar where

-- The goal of this test is to ensure that the special typing rule of ($) plays
-- well with multiplicity-polymorphic functions

data F = F Bool

x = F $ True
