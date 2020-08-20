module B where

import Data.Map

data Bar = Bar
  deriving (Show, Eq)

bar :: Map Integer [Char]
bar = fromList [(1, "Hallo"), (2, "World")]
