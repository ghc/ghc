module Main where

import Sort

main = do
    cs <- getContents
    putStr (mangle "quickSort" cs)

mangle :: String{-opt-} -> String{-input to sort-} -> String{-output-}
mangle opt inpt
  = (unlines . sort . lines) inpt
  where
    sort = case opt of
	     "heapSort"		-> heapSort
	     "insertSort"	-> insertSort
	     "mergeSort"	-> mergeSort
	     "quickSort"	-> quickSort
	     "quickSort2"	-> quickSort2
	     "quickerSort"	-> quickerSort
	     "treeSort"		-> treeSort
	     "treeSort2"	-> treeSort2
	     _ -> error ("unrecognized opt: "++opt++"\n")
