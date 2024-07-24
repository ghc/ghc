{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import TestUtils
import GHC.Records
import GHC.TypeLits
import Data.Tree

class C a where
  f :: a -> Char

instance C Char where
  f x = x

data Thing = Thing {field1 :: Char, field2 :: Bool}
  deriving (Show, Eq)

foo :: Thing -> String
foo t = show t.field1 ++ show t.field2
        --      ^ this is the point
point :: (Int,Int)
point = (21,17)

bar :: Show x => x -> String
bar x = show [(1,x,A)]
--      ^ this is the point'
point' :: (Int,Int)
point' = (21,33)

add :: Num a => a -> a -> a
add x y = x + y

testing (x :: Int) =
  add (add x x) x
  
testing2 (x :: Thing) = x.field1

data A = A deriving Show

data Another = Another

testing3 = (natSing :: SNat 0)

main = do
  (df, hf) <- readTestHie "HasFieldQueries.hie"
  let refmap = generateReferencesMap $ getAsts $ hie_asts hf
  explainEv df hf refmap point

  explainEv df hf refmap point'

  return ()
