
module Main (main) where

import Data.Fixed

data B7

instance HasResolution B7 where
  resolution _ = 128

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
          print (read "0.008" :: Fixed B7)
          print (read "-0.008" :: Fixed B7)
          print $ show (Just (-1 :: Milli))
          print $ show (Just (1 :: Milli))
          print ((read $ show (Just (-1 :: Deci))) :: Maybe Deci)
          print ((read $ show (Just (1 :: Deci))) :: Maybe Deci)

doit :: Centi -> IO ()
doit c = do let s = show c
                r = read s :: Centi
            putStrLn s
            print r
