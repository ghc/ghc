{-# LANGUAGE TemplateHaskell, UnboxedTuples #-}
-- test the representation of unboxed tuples

module Main where

import GHC.Exts
import GHC.Float
import Language.Haskell.TH
import Text.PrettyPrint
import System.IO

main :: IO ()
main = case bar () of
       (# str, int #) ->
           print (str, int)

bar :: () -> (# String, Int #)
bar () = $( do e <- [| case (# 'b', False #) of
                       (# 'a', True  #) -> (# "One", 1 #)
                       (# 'b', False #) -> (# "Two", 2 #)
                       (# _,   _     #) -> (# "Three", 3 #)
                     |]
               runIO $ putStrLn $ show e
               runIO $ putStrLn $ pprint e
               runIO $ hFlush stdout
               return e )

