-- Trac #1608

module ShouldFail where

newtype Ego a = Ego a deriving (Ord)

f :: Ord a => Ego a -> Ego a -> Bool
f e1 e2 = e1 < e2
