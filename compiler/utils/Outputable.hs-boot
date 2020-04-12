module Outputable where

import GhcPrelude
import GHC.Stack( HasCallStack )

data SDoc
data PprStyle
data SDocContext

showSDocUnsafe :: SDoc -> String

warnPprTrace :: HasCallStack => Bool -> String -> Int -> SDoc -> a -> a

text :: String -> SDoc
