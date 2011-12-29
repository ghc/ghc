
module Main (main) where

import Data.Fixed

main :: IO ()
main = do doit 38.001
          doit 38.009
          doit 38.01
          doit 38.09
          print (read "38" :: Centi)
          doit (-38.001)
          doit (-38.009)
          doit (-38.01)
          doit (-38.09)
          print (read "-38" :: Centi)

doit :: Centi -> IO ()
doit c = do let s = show c
                r = read s :: Centi
            putStrLn s
            print r
