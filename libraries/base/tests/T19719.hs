module Main (main) where

import Data.Complex (Complex(..))
import Data.Functor.Classes (readPrec1, showsPrec1)
import Text.ParserCombinators.ReadPrec (readPrec_to_S)
import Text.Read (Read(..))

comp :: Complex Int
comp = 1 :+ 1

compareInstances :: Int -> IO ()
compareInstances p = do
  let precBanner = " (at precedence " ++ show p ++ ")"
  putStrLn $ "Read vs. Read1" ++ precBanner
  print (readPrec_to_S readPrec  p "1 :+ 1" :: [(Complex Int, String)])
  print (readPrec_to_S readPrec1 p "1 :+ 1" :: [(Complex Int, String)])
  putStrLn ""
  putStrLn $ "Show vs. Show1" ++ precBanner
  putStrLn $ showsPrec  p comp ""
  putStrLn $ showsPrec1 p comp ""
  putStrLn ""

main :: IO ()
main = do
  compareInstances 6
  compareInstances 7
