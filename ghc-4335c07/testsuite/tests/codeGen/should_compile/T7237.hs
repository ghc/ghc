module T7237 where

import Data.List (foldl')

data SPair = SPair { pX, pY :: Double } deriving (Eq,Show)

data AList = AList { pairs :: [SPair] } deriving Show

rnf' :: AList -> ()
rnf' aList = foldl' (flip seq) () (pairs aList) `seq` ()
