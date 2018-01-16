{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
module Main where

import Language.Haskell.TH

data T = T (# Int | Char #)

$(return [])

main :: IO ()
main = putStrLn $(reify ''T >>= stringE . show)
