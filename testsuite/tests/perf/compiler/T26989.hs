module Main (main) where

import T26989a (WriterT (..), spec)

main :: IO ()
main = do
  let WriterT m = repro
  _ <- m
  pure ()

repro = do
  do {
       spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     -- 50
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec

     -- 100

     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     -- 150
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec

     -- 200

     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     -- 250
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec

     -- 300
{-
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     -- 350
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec

     -- 400

     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     -- 450
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec

     -- 500

     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     -- 550
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
     ; spec; spec; spec; spec; spec; spec; spec; spec; spec; spec
-}
     -- 600
     }
