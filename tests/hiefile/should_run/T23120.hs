{-# LANGUAGE GHC2021 #-}

module Main where

import TestUtils
import qualified Data.Map.Strict as Map
import Data.Either
import Data.Maybe
import GHC.Plugins (nameOccName, occNameString)

f :: Int -> Int -> Bool
f x = (x ==)
      -- ^ point

point :: (Int,Int)
point = (12,10)

main = do
  (df, hf) <- readTestHie "T23120.hie"
  let ast = fromMaybe (error "nothing") $ selectPoint hf point
      idents = sourcedNodeIdents $ sourcedNodeInfo ast
      names = rights $ Map.keys idents
  mapM_ (print . occNameString . nameOccName) names
