{-# LANGUAGE GHC_CPP #-}
module GhcCpp01 where

#define FOO(A,B) A + B
#define FOO(A,B,C) A + B + C
#if FOO(1,FOO(3,4)) == 8

-- a comment
x = 1
#else
x = 5
#endif

#if defined(BAR) || defined FOO
y = 1
#endif
