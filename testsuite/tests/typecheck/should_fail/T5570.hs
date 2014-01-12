{-# LANGUAGE MagicHash #-}
module T5570 where

import GHC.Exts

main :: IO ()
main = print $ D# $ 3.0##
