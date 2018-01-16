{-# LANGUAGE NoImplicitPrelude, PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}
module Test where

x :: ()
x = ()

pattern Point2 :: () -> () -> ((), ())
pattern Point2 x y = (x, y)

pattern Point :: () -> () -> ((), ())
pattern Point{x1, y1} = (x1, y1)
