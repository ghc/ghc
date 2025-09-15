{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- Hugs (2001) diverges when evaluating main, unless (Monad m) is
-- added as an extra superclass to C2, which should not be necessary.

module Main where

class Monad m => C1 m x

class (C1 m x) => C2 m x where
    c2 :: x -> m x

instance Monad m => C1 m Bool

instance C2 Maybe Bool where
    c2 = return

test :: Maybe Bool
test = c2 True

main = print test

