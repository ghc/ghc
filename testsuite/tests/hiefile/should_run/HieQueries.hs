{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import TestUtils
import Data.Tree

class C a where
  f :: a -> Char

instance C Char where
  f x = x

instance C a => C [a] where
  f x = 'a'

foo :: C a => a -> Char
foo x = f [x]
--      ^ this is the point
point :: (Int,Int)
point = (17,9)

bar :: Show x => x -> String
bar x = show [(1,x,A)]
--      ^ this is the point'
point' :: (Int,Int)
point' = (23,9)

data A = A deriving Show

main = do
  (df, hf) <- readTestHie "HieQueries.hie"
  let refmap = generateReferencesMap $ getAsts $ hie_asts hf
  explainEv df hf refmap point
  explainEv df hf refmap point'
  return ()
