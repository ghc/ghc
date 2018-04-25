-- Made GHC 6.10.2 go into a loop in substRecBndrs
{-# OPTIONS_GHC -w #-}

module T10627 where

import Data.Word

class C a where
    splitFraction    :: a -> (b,a)

roundSimple :: (C a) => a -> b
roundSimple x = error "rik"

{-# RULES
     "rs"  roundSimple = (fromIntegral :: Int -> Word) . roundSimple;
  #-}

