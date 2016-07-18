{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
module Main where

import Language.Haskell.TH

data T = T (# Int, Int #)

$(return [])

main :: IO ()
main = putStrLn $(reify ''T >>= stringE . pprint)
