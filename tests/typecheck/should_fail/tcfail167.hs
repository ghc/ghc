{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- Test inspired by trac #366
-- The C2 case is impossible due to the types

module ShouldCompile where

data T a where
    C1 :: T Char
    C2 :: T Float

inaccessible :: T Char -> Char
inaccessible C1 = ' '
inaccessible C2 = ' '

