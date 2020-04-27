{-# LANGUAGE TemplateHaskell #-}
module T18097 where

import Language.Haskell.TH
import GHC.Tuple

f = case $( tupE [ [| "ok" |] ] ) of Unit x -> putStrLn x
g = case Unit "ok" of $( tupP [ [p| x |] ] ) -> putStrLn x

h :: $( tupleT 1 ) String
h = Unit "ok"

i :: Unit String
i = $( tupE [ [| "ok" |] ] )
