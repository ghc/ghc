{-# LANGUAGE GhcCPP #-}
module Example1 where

y = 3

#define FOO

x =
#ifndef FOO
  "hello"
#else
  "bye now"
#endif

foo = putStrLn x
