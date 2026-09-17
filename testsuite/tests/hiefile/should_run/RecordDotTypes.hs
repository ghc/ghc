{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import TestUtils
import qualified Data.Map as M
import Data.Foldable

data MyRecord = MyRecord
  { a :: String
  , b :: Integer
  , c :: MyChild
  } deriving (Eq, Show)

newtype MyChild = MyChild
  { z :: String
  } deriving (Eq, Show)

x = MyRecord { a = "Hello", b = 12, c = MyChild { z = "there" } }
y = x.a ++ show x.b ++ x.c.z
--  ^             ^    ^  ^^
--  1             2    3  45
--  ^-^         ^-^    ^-^
--   6           7      8
--                     ^---^
--                       9


p1,p2,p3,p4 :: (Int,Int)
p1 = (22,6)
p2 = (22,20)
p3 = (22,25)
p4 = (22,28)
p5 = (22,29)

r6 = (p1, (22, 8))
r7 = ((22,17), p2)
r8 = (p3, (22, 27))
r9 = (p3, p5)

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

selectRange' :: HieFile -> ((Int,Int), (Int, Int)) -> HieAST Int
selectRange' hf (s, e) =
  maybe (error "range not found") id $ selectRange hf s e

main = do
  (df, hf) <- readTestHie "RecordDotTypes.hie"
  forM_ [p1,p2,p3,p4,p5] $ \point -> do
    putStr $ "At " ++ show point ++ ", got type: "
    let types = concatMap nodeType $ getSourcedNodeInfo $ sourcedNodeInfo $ selectPoint' hf point
    forM_ types $ \typ -> do
      putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))

  forM_ [r6, r7, r8, r9] $ \range -> do
    putStr $ "At " ++ showRange range ++ ", got type: "
    let types = concatMap nodeType $ getSourcedNodeInfo $ sourcedNodeInfo $ selectRange' hf range
    forM_ types $ \typ -> do
      putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
  where
    showRange (p1, p2) = show p1 ++ " - " ++ show p2
