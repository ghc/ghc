--- !!! test for bug in handle finalization fixed in 
--- !!!  1.60      +1 -2      fptools/ghc/lib/std/PrelHandle.lhs
--- !!!  1.15      +4 -10     fptools/ghc/lib/std/PrelIO.lhs

module Main (main) where

import System.IO

doTest :: IO ()
doTest = do
  sd <- openFile "finalization001.hs" ReadMode
  result <- hGetContents sd
  slurp result
  hClose sd
  if "" `elem` lines (filter (/= '\r') result)
   then
    putStrLn "ok"
   else
    putStrLn "fail"

slurp :: String -> IO ()
slurp [] = return ()
slurp (x:xs) = x `seq` slurp xs

main :: IO ()
main = sequence_ (take 200 (repeat doTest))
