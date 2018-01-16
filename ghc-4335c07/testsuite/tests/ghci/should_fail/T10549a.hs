{-# OPTIONS_GHC -O #-}
module Main(main) where
import GHC.Exts
main = print 1
go (Ptr a) = a
