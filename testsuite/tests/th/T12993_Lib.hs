{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module T12993_Lib (q) where
data X = X { x :: Int }
q = [|x|]
