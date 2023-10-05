module T20200 where

import qualified Data.Map as Map

cleanTempDirs :: ([String] -> a) -> Map.Map String String -> a
cleanTempDirs logger ds = logger (Map.elems ds)
