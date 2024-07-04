{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
-- It's hard to trigger #25033, because the Simplier eta-expands
-- lambdas.  So I switched off that Simplifier ability, and thereby
-- triggered the bug on this nice small example.

module T25033 where

{-# NOINLINE woo #-}
woo x = x

foo v = woo (\xs -> let
                     j ys = \ws -> xs ++ (reverse . reverse . reverse . reverse .
                                          reverse . reverse . reverse . reverse) ws
                   in
                   case v of
                     "a" -> j "wim"
                     _   -> j "wam"
           )
