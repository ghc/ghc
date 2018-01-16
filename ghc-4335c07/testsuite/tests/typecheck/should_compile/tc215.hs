{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Test for trac #366
-- The C2 case is impossible due to the types

module ShouldCompile where

data T a where
    C1 :: T Char
    C2 :: T Float

exhaustive :: T Char -> Char
exhaustive C1 = ' '

