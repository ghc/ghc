{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main where

import GHC.Types.Name (nameSrcSpan)
import TestUtils
import qualified Data.Map as M
import Data.Foldable
import Data.Either

f :: forall a -> a -> Maybe a
f (type t) (x :: t) = Just (x :: t)
--      ^p1      ^p2             ^p3

p1,p2 :: (Int,Int)
p1 = (13,9)
p2 = (13,18)
p3 = (13,34)

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

main = do
  (df, hf) <- readTestHie "HieVdq.hie"
  forM_ [p1,p2,p3] $ \point -> do
    putStr $ "At " ++ show point ++ ", got names: "
    let names =
          concatMap (rights . M.keys . nodeIdentifiers) $
          M.elems $ getSourcedNodeInfo $
          sourcedNodeInfo $ selectPoint' hf point
    forM_ names $ \name -> do
      putStrLn (render df (nameSrcSpan name, name))
