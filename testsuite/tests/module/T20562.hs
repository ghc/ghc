{-# LANGUAGE PatternSynonyms #-}
module Main (main) where

import GHC.Tuple

type OneTuple = Solo

only :: OneTuple a -> a
only = getSolo

pattern OneTuple :: a -> Solo a
pattern OneTuple a = Solo a

main :: IO ()
main = print (only (OneTuple 'x'))
