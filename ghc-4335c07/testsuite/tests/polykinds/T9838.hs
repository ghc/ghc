module T9838 where

import T9838a

foo :: EqShow a => a -> String
foo x = show x ++ show (x == x)

bar :: F Int -> Bool
bar x = x