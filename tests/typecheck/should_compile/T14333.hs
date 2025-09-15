module T14333 where

import Data.Coerce

bad :: Coercible (a b) (c d) => c d -> a b
bad = coerce

bad2 :: Coercible (c d) (a b) => c d -> a b
bad2 = coerce

bad3 :: Coercible (a b) b => b -> a b
bad3 = coerce

bad4 :: Coercible b (a b) => b -> a b
bad4 = coerce

newtype Id a = MkId a

foo :: Id Int
foo = bad3 (3 :: Int)
