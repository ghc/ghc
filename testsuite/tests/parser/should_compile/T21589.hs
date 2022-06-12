{-# LANGUGE BangPatterns #-}
{-# OPTION_HUGS #-}
{-# LIE 42 "Foo.vhs" #-}
module T21589 where

x :: Int
x = 42
{-# INLNE x #-}
