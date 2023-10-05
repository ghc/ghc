module B where

import A

data Y = Y Int Int

thing :: X -> a
thing (X (Y a b)) = thing (X (Y a b))
