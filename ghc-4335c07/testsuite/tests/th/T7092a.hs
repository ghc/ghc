{-# LANGUAGE TemplateHaskell #-}
module T7092a where

import Language.Haskell.TH

code :: Q Exp
code = do
  n1 <- newName "foo"
  n2 <- newName "foo"
  letE [valD (varP n1) (normalB [| (1 :: Int) |]) []] 
       $ letE [valD (varP n2) (normalB [| (2 :: Int) |]) []] 
       $ appE (appE [| ((+) :: Int -> Int -> Int)|] (varE n1)) (varE n2)
