module T23630 where

data HOLType = UTypeIn !HOLType deriving Eq

tyVars :: HOLType -> [HOLType]
tyVars (UTypeIn tv) = [undefined]

union :: Eq a => [a] -> [a] -> [a]
union l1 l2 = foldr insert l2 l1

insert :: Eq a => a -> [a] -> [a]
insert x l
    | x `elem` l = l
    | otherwise = x : l

catTyVars :: [HOLType] -> [HOLType]
catTyVars = foldr (union . tyVars) []
