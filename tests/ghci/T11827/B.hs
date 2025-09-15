module B where

import {-# SOURCE #-} A

data B = B A

g :: B -> Bool
g (B a) = f a
