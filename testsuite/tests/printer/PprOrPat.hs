{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

a = case [1] of
  [1,2,3] -> True
  4 ; 5
  (   {- 01-}
         {- 12  -}  [4, 5]  ;   [6,7] {-test-} ; [_,2]
      )   ->  False

pattern A <- (({-test-} reverse -> {-e-}( [2,1] ;  {-1-} 0:_ )),  id  -> [])
b = case [1,2] of A -> True
