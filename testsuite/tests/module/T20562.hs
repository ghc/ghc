{-# LANGUAGE PatternSynonyms #-}
module Main (main) where

import GHC.Tuple (Solo (MkSolo), getSolo)

type OneTuple = Solo

only :: OneTuple a -> a
only = getSolo

pattern OneTuple :: a -> Solo a
pattern OneTuple a = MkSolo a

main :: IO ()
main = print (only (OneTuple 'x'))
