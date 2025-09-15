{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -ddump-parsed-ast #-}
module T15279 where

foo :: Char -> Char
#include "T15279.hs-incl"
foo _ = 'a'
