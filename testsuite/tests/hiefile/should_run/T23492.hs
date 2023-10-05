{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import TestUtils
import qualified Data.Map as M
import Data.Foldable

-- regression test for https://gitlab.haskell.org/ghc/ghc/-/issues/23492
data PartialFieldSelector
  = NoFields
  | PartialField { a :: Bool }
--                 ^
--                 1

f :: PartialFieldSelector -> Bool
f x = a x
--    ^
--    2

g :: PartialFieldSelector -> Bool
g x = x.a
--    ^^^
--    345

p1, p2, p3, p4, p5 :: (Int,Int)
p1 = (13,20)
p2 = (18,7)
p3 = (23,7)
p4 = (23,8)
p5 = (23,9)

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

main = do
  (df, hf) <- readTestHie "T23492.hie"
  forM_ [p1,p2,p3,p4,p5] $ \point -> do
    putStr $ "At " ++ show point ++ ", got type: "
    let types = concatMap nodeType $ getSourcedNodeInfo $ sourcedNodeInfo $ selectPoint' hf point
    forM_ types $ \typ -> do
      putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
