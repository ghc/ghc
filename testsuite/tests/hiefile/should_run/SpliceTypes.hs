{-# LANGUAGE TemplateHaskell #-}

module Main where

import TestUtils
import qualified Data.Map as M
import Data.Foldable
import Language.Haskell.TH.Syntax


newtype T = T { getT :: Int }

instance Lift T where
  liftTyped v = [||T $$(liftTyped (getT v))||]
--              ^  ^  ^     ^           ^
--              1  2  3     4           5
--

top_level :: ()
top_level = $$([|| () ||])
--               ^  ^
--               1  2

p1,p2, p3, p4:: (Int,Int)
p1 = (14,18)
p2 = (14,21)
p3 = (14,24)
p4 = (14,29)
p5 = (14,41)

q1 = (20, 19)
q2 = (20, 21)

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

main = do
  (df, hf) <- readTestHie "SpliceTypes.hie"
  forM_ [p1,p2,p3, p4, p5, q1, q2] $ \point -> do
    let types = concatMap nodeType $ getSourcedNodeInfo $ sourcedNodeInfo $ selectPoint' hf point
    case types of
      [] -> putStrLn $ "No types at " ++ show point
      _ -> do
        putStr $ "At " ++ show point ++ ", got type: "
        forM_ types $ \typ -> do
          putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
