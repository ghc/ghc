module Generator.ByteString where

import Data.List (intercalate, intersperse)
import qualified Data.ByteString.Builder as BB

replicateBB :: Int -> BB.Builder -> BB.Builder
replicateBB indent b = mconcat (replicate indent b)

unlinesBBWithIndent :: Int -> [BB.Builder] -> BB.Builder
unlinesBBWithIndent indent =
  let indent_replicated = replicateBB indent " "
  in mconcat . intersperse ("\n" <> indent_replicated)

unlinesBB :: [BB.Builder] -> BB.Builder
unlinesBB = (<> "\n") . unlinesBBWithIndent 0

unwordsBB :: [BB.Builder] -> BB.Builder
unwordsBB = mconcat . intersperse " "

