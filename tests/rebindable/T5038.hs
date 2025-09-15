{-# LANGUAGE RebindableSyntax #-}
module Main (main) where

import Prelude 

ifThenElse True t f = f
ifThenElse False t f = t

main = print (if True then 1 else 2 :: Int)
-- Should print 2!
