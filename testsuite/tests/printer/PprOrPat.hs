{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

a = case [1] of
  [1,2,3] -> True
  ( one   {- 01-}  of
         {- 12  -}  [4, 5] ,   [6,7 {- lol -}] {-test-}
      )   ->  False

pattern A <- (one of ({-hmm-} reverse -> {-e-}( {-f-} one  of [2,1],  0:_ )),  id  ->  []) {-123-}
b = case [1,2] of A -> True

