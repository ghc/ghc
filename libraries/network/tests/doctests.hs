module Main where

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf, isPrefixOf)
import Test.DocTest (doctest)

main :: IO ()
main = do
    traverse_ putStrLn args
    doctest args
  where
    builddir = filter ("build" `isSuffixOf`) $ filter ("-i" `isPrefixOf`) flags
    addinc path = "-I" ++ drop 2 path ++ "/include"
    args = map addinc builddir ++ flags ++ pkgs ++ module_sources
