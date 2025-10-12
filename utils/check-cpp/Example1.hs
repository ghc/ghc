{-# LANGUAGE GHC_CPP #-}
{-# OPTIONS -ddump-ghc-cpp -dkeep-comments #-}
module Example1 where

-- A comment
y = 3

    #define FOO

x =
#ifndef FOO
  "hello"
#else
  "bye now"
#endif

foo = putStrLn x
