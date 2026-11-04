module IfaceReadHook_A where

import Data.Maybe (fromMaybe)

f :: Maybe Int -> Int
f = fromMaybe 0
