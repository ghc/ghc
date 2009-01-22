{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- example taken from the GHC documentation
module QuasiQuote where

import QuasiExpr

run :: IO ()
run = do { print $ eval [$expr|1 + 2|] }
