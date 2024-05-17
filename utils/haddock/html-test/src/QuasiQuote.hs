{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE QuasiQuotes #-}

-- example taken from the GHC documentation
module QuasiQuote where

import QuasiExpr

val :: Integer
val = eval [expr|1 + 2|]
