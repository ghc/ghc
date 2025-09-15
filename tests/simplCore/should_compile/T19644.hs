-- {-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
-- {-# OPTIONS_GHC -O2 -fforce-recomp #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash, UnboxedTuples #-}
module T19644 where

class C a where
  m :: Show b => a -> b -> String
  dummy :: a -> () -- Force a datatype dictionary representation

instance C Int where
  m a b = show a ++ show b
  dummy _ = ()

f :: (C a, Show b) => a -> b -> String
f a b = m a b ++ "!"
{-# INLINABLE[0] f #-}

main = putStrLn (f (42::Int) (True::Bool))
