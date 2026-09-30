module Main where
import T27845_Lib (f)
import System.Environment (getArgs)

{-# NOINLINE mkArg #-}
mkArg :: Int -> Int
mkArg n | n < 0     = n
        | otherwise = error ("boom " ++ show n)

main :: IO ()
main = do
  n <- length <$> getArgs
  let x = mkArg n
  print (length (f (x, x) n))
