{-# LANGUAGE GHC_CPP #-}
-- {-# OPTIONS -ddump-ghc-cpp -dkeep-comments #-}
module GhcCpp01 where

-- Check leading whitespace on a directive
   # define FOO(A,B) A + B
#define FOO(A,B,C) A + B + C
#if FOO(1,FOO(3,4)) == 8

-- a comment
x = 1
#else
x = 5
#endif

{-# LINE 15 "GhcCpp01.hs" #-}

#if defined(BAR) || defined FOO
y = 1
#endif
