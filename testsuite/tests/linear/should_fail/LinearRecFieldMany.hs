{-# LANGUAGE LinearTypes, DataKinds #-}
module LinearRecFieldMany where

import GHC.Exts (Multiplicity(..))

data C = C { urC %'Many :: Int }

test :: Int %1 -> C
test x = C x

test2 :: Int %1 -> C
test2 x = C { urC = x }
