module Main where

import TestUtils
import qualified Data.Map as M
import Data.Foldable

foo :: Maybe Char -> Char
foo Nothing = 'a'
--  1^
foo (Just c) | c == 'a' =  c
-- 2^    3^
foo x = 'b'
-- 4^

p1,p2,p3,p4 :: (Int,Int)
p1 = (8,6)
p2 = (10,5)
p3 = (10,11)
p4 = (12,5)

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

main = do
  (df, hf) <- readTestHie "PatTypes.hie"
  forM_ [p1,p2,p3,p4] $ \point -> do
    putStr $ "At " ++ show point ++ ", got type: "
    let types = concatMap nodeType $ getSourcedNodeInfo $ sourcedNodeInfo $ selectPoint' hf point
    forM_ types $ \typ -> do
      putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
