module EpsStore_A where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data T = A | B deriving (Show, Eq, Ord)

f :: M.Map T Int -> Int
f m = fromMaybe 0 (M.lookup A m) + sum (M.elems m)

g :: [Maybe Int] -> String
g = show . map (fromMaybe 0)
