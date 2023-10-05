{-# language DeriveAnyClass #-}
{-# language DefaultSignatures #-}

module Main where

import TestUtils
import qualified Data.Map as M
import Data.Tree
import Data.Foldable

class ToJSON a where
  foo :: a -> String
  default foo :: Show a => a -> String
  foo x = show x

data T = MkT { fieldName :: Bool }
  deriving (Show, ToJSON)


g :: String
g = foo (MkT True)
--   ^ this is point

h :: String
h = show (MkT True)
--   ^ this is point'

point :: (Int, Int)
point = (21,6)

point' :: (Int, Int)
point' = (25,6)

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

main = do
  (df, hf) <- readTestHie "T20341.hie"
  let asts = getAsts $ hie_asts hf
      [ast] = M.elems asts
      refmap = generateReferencesMap $ asts
      expandType = text . renderHieType df .
        flip recoverFullType (hie_types hf)
      pretty = unlines . (++["└"]) . ("┌":) . map ("│ "++) . lines
      pprint = pretty . render df
  putStr $ "At " ++ show point ++ ", got evidence: "
  let trees = getEvidenceTreesAtPoint hf refmap point
      ptrees = fmap (pprint . fmap expandType) <$> trees
  -- Print the evidence tree at point - it should include $fToJSONT
  putStr $ drawForest ptrees

  -- Get the definition location of $fToJSONT
  let loc = evidenceSpan $ head $ last $ levels $ head trees
  print loc

  -- Find the ast of the definition of $fToJSONT
  let Just fToJSONTAst= selectLargestContainedBy loc ast

  -- Print the evidence tree at point' - it should include $fShowT
  let trees' = getEvidenceTreesAtPoint hf refmap point'
      ptrees' = fmap (pprint . fmap expandType) <$> trees'
  -- Print the evidence tree at point' - it should include $ShowT
  putStr $ drawForest ptrees'

  -- Get the name of $dShow = $fShowT
  let dShowT = evidenceVar $ rootLabel $ head trees'

  -- Finally ensure that the definition of $fToJSONT contains a reference to $dShowT
  let isMember = M.member (Right dShowT) $ sourcedNodeIdents $ sourcedNodeInfo fToJSONTAst
  if isMember
  then putStrLn "$dShow was found in the definition of $fToJSONT"
  else putStrLn "ERROR: $dShow was NOT found in the definition of $fToJSONT"

