module Outputable where

import GhcPrelude

data SDoc' r

class HasPprConfig r

text :: String -> SDoc' r
