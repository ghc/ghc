{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module T1 where



g :: Int -> Char

g _ = 'a'



f :: Int -> Show Int => ()
f = g
