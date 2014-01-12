{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Float where

import GHC.Prim

foo vs  
  = let w = if length (reverse vs) > 10 then Just (length vs) else Nothing
        
        f :: State# RealWorld -> Int -> (# Int, State# RealWorld #)
        f s x | Just 0 <- w = case f s (x+1) of
                        (# r, s' #) -> (# r, s' #) 
              | otherwise = (# x, s #)

    in f realWorld# 1

