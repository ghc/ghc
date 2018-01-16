module Outputable where

import GhcPrelude

data SDoc

showSDocUnsafe :: SDoc -> String

warnPprTrace :: Bool -> String -> Int -> SDoc -> a -> a

text :: String -> SDoc
