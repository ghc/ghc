{-# LANGUAGE LinearTypes, DataKinds #-}
module NonLinearRecord where

import GHC.Exts (Multiplicity(..))

data C m = C { linC %1 :: Int, urC %Many :: Char, varC %m :: String, noC :: Bool }

data G mult where
  G :: { linG %1 :: Int, urG %Many :: Char, varG %m :: String, noG :: Bool } -> G m

testC :: Int %1 -> Char -> String -> Bool %1 -> C Many
testC w x y z = C w x y z

testCR :: Int %1 -> Char -> String -> Bool %1 -> C Many
testCR w x y z = C { linC = w, urC = x, varC = y, noC = z }

testG :: Int %1 -> Char -> String %1 -> Bool %1 -> G One
testG w x y z = G w x y z

testGR :: Int %1 -> Char -> String %1 -> Bool %1 -> G One
testGR w x y z = G { linG = w, urG = x, varG = y, noG = z }
