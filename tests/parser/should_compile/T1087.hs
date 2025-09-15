{-# LANGUAGE BangPatterns #-}

module T1087 where

prefix_1 = let at a !b = False in at 1 2
prefix_2 = let (.!.) a !b = False in 1 .!. 2

infix_tilde_1 = let a `at` ~b = False in at 1 2
infix_tilde_2 = let a .!. ~b = False in 1 .!. 2
infix_tilde_3 = let ~a .!. b = False in 1 .!. 2

infix_bang_1 = let a .!. !b = False in 1 .!. 2
infix_bang_2 = let a `at` !b = False in at 1 2
infix_bang_3 = let !a .!. b = False in 1 .!. 2
