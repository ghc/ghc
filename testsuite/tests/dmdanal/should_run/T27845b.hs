module Main where
import T27845b_Lib (outer)
import System.Environment (getArgs)

{-# NOINLINE mkArg #-}
mkArg :: Int -> Int
mkArg n | n < 0     = n
        | otherwise = error ("boom " ++ show n)

main :: IO ()
main = do
  n <- length <$> getArgs
  let x = mkArg n
  print (length (outer (x, x) n))
