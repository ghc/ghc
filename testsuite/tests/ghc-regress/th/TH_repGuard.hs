{-# OPTIONS -fglasgow-exts #-}
-- test the representation of unboxed literals

module Main
where

import Language.Haskell.TH

$(
  do ds <- [d|
        foo :: Int -> Int
        foo x
         | x == 5 = 6
        foo x = 7
      |]
     runIO $ putStrLn $ pprint ds
     return ds
 )

$(
  do ds <- [d|
        bar :: Maybe Int -> Int
        bar x
         | Just y <- x = y
        bar _ = 9
      |]
     runIO $ putStrLn $ pprint ds
     return ds
 )

main :: IO ()
main = do putStrLn $ show $ foo 5
          putStrLn $ show $ foo 8
          putStrLn $ show $ bar (Just 2)
          putStrLn $ show $ bar Nothing

