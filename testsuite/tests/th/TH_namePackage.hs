{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

eitherName, fooName, moduleFooName :: Name
eitherName = ''Either
fooName = mkName "foo"
moduleFooName = mkName "Module.foo"

main :: IO ()
main = do
  print $ nameBase eitherName
  print $ nameBase fooName
  print $ nameBase moduleFooName

  print $ nameModule eitherName
  print $ nameModule fooName
  print $ nameModule moduleFooName

  print $ namePackage eitherName
  print $ namePackage fooName
  print $ namePackage moduleFooName
