module B where
import {-# SOURCE #-} A
data B = B A
    deriving (Show)
