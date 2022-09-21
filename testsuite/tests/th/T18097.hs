{-# LANGUAGE TemplateHaskell #-}
module T18097 where

import Language.Haskell.TH
import GHC.Tuple

f = case $( tupE [ [| "ok" |] ] ) of MkSolo x -> putStrLn x
g = case MkSolo "ok" of $( tupP [ [p| x |] ] ) -> putStrLn x

h :: $( tupleT 1 ) String
h = MkSolo "ok"

i :: Solo String
i = $( tupE [ [| "ok" |] ] )
