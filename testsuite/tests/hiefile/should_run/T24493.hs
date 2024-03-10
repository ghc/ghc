module Main where

import TestUtils
import qualified Data.Map.Strict as Map
import Data.Either
import Data.Maybe
import GHC.Plugins (moduleNameString)


point :: (Int,Int)
point = (1,9)

main = do
  (df, hf) <- readTestHie "T24493.hie"
  let ast = fromMaybe (error "nothing") $ selectPoint hf point
      idents = sourcedNodeIdents $ sourcedNodeInfo ast
      names = lefts $ Map.keys idents
  mapM_ (print . moduleNameString) names
