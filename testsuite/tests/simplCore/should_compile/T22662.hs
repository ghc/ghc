module T22662 where

import Data.Set

foo x = sequence_ [ f y | y <- x ]
  where f _ = return (fromList [0])
