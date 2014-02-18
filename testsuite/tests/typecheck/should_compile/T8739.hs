{-# LANGUAGE MagicHash #-}
module Main where
import GHC.Exts

go :: () -> Int#
go () = 0#

main = print (I# (go $ ()))


