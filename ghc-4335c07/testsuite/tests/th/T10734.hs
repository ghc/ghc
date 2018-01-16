{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

main :: IO ()
main = do
  pprint <$> runQ [| do { let { }; return (); } |]             >>= putStrLn
  pprint <$> runQ [| do { let { x = 5 }; return x; } |]        >>= putStrLn
  pprint <$> runQ [| do { let { x = 5; y = 3 }; return x; } |] >>= putStrLn
