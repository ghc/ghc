{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main where

import GHC.Types.Name (nameSrcSpan)
import TestUtils
import qualified Data.Map as M
import Data.Foldable
import Data.Either
import Data.Kind

type G :: k -> Type
data G a where
  MkG0 :: Eq a => G (a, b, c)
  MkG1 :: forall {k} (a :: k). (Eq k) => k -> G a
  MkG2 :: forall k. forall (a :: k). (Eq k) => k -> G a

points :: [(Int,Int)]
points = [(15,col)|col<-[14,22,25,28]]        -- MkG0
      ++ [(16,col)|col<-[19,23,28,36,42,49]]  -- MkG1
      ++ [(17,col)|col<-[18,29,34,42,48,55]]  -- MkG2

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

main = do
  (df, hf) <- readTestHie "HieGadtConSigs.hie"
  forM_ points $ \point -> do
    putStr $ "At " ++ show point ++ ", got names: "
    let names =
          concatMap (rights . M.keys . nodeIdentifiers) $
          M.elems $ getSourcedNodeInfo $
          sourcedNodeInfo $ selectPoint' hf point
    forM_ names $ \name -> do
      putStrLn (render df (nameSrcSpan name, name))
