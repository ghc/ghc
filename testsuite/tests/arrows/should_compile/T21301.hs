{-# LANGUAGE Arrows, EmptyCase #-}

import Control.Arrow

main = print $ baz (Just 43)

baz :: ArrowChoice p => p (Maybe Int) String
baz = proc x ->
  (| id (case () of)
  |) x
