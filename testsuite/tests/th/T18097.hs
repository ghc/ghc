{-# LANGUAGE TemplateHaskell #-}
module T18097 where

import Language.Haskell.TH
import GHC.Tuple

f = case $( tupE [ [| "ok" |] ] ) of Solo x -> putStrLn x
g = case Solo "ok" of $( tupP [ [p| x |] ] ) -> putStrLn x

h :: $( tupleT 1 ) String
h = Solo "ok"

i :: Solo String
i = $( tupE [ [| "ok" |] ] )
