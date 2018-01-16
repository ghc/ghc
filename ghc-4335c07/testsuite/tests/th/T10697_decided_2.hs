{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

data T = T {-# UNPACK #-} !Int !Int Int

$(return [])

main :: IO ()
main = putStrLn $(reifyConStrictness 'T >>= stringE . show)
