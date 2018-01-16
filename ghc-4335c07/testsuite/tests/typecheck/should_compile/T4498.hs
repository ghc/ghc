{-# LANGUAGE BangPatterns, NoMonoLocalBinds, NoMonoPatBinds #-}

module T4498 where

f x = let !y = (\v -> v) :: a -> a
      in (y x, y 'T')

