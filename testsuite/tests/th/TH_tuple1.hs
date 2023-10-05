module Main where

import Language.Haskell.TH
import TH_tuple1a

main :: IO ()
main = do
 let pprQ = \a -> print a >> (putStrLn $ pprint a)
 mapM_ (\q -> runQ q >>= pprQ) [tp2, tp1, tp2u, tp1u]
