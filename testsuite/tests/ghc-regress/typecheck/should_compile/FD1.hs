{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- Trac #1781
-- This one should really succeed, because 'plus' can only
-- be called with a = Int->Int, but the old fundep story
-- certainly made it fail, and so that's what we expect for now
-- We may become more liberal later

module ShouldCompile where

class E a b | a -> b, b -> a
instance E a a

plus :: (E a (Int -> Int)) => Int -> a
plus x y = x + y

