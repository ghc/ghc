module Main where

import Data.Data
import System.IO
import GHC
import GHC.Data.FastString
import GHC.Types.SrcLoc
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Data.Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )

main::IO()
main = do
  let
    loc1 = mkSrcLoc (mkFastString "filename") 1 3
    loc2 = mkSrcLoc (mkFastString "filename") 1 5
    loc3 = mkSrcLoc (mkFastString "filename") 10 1
    badLoc = mkGeneralSrcLoc (mkFastString "bad loc")

    pointSpan = mkSrcSpan loc1 loc1
    lineSpan  = mkSrcSpan loc1 loc2
    multiSpan = mkSrcSpan loc2 loc3
    badSpan  = mkGeneralSrcSpan (mkFastString "bad span")

  print $ show loc1
  print $ show loc2
  print $ show badLoc
  print $ show pointSpan
  print $ show lineSpan
  print $ show multiSpan
  print $ show badSpan
