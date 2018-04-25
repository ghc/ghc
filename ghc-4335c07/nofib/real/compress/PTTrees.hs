module PTTrees (insert, PrefixTree(..), PrefixElem(..)) where

data PrefixTree a b = PTNil |
                      PT (PrefixElem a b) (PrefixTree a b) (PrefixTree a b)

data PrefixElem a b = PTE a b (PrefixTree a b)

{-partain-}
--insert :: Char -> Int -> PrefixTree Char Int -> PrefixTree Char Int
{-partain-}

insert k v PTNil = 
	PT (PTE k v PTNil) PTNil PTNil
insert k v (PT p@(PTE k' v' t) l r)
	| k < k'  = PT p (insert k v l) r
        | k > k'  = PT p l (insert k v r)
        | otherwise = PT p l r
