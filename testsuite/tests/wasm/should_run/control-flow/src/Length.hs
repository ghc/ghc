module Length
where

data List a = Nil | Cons a (List a)

length' :: List a -> Int
length' = count 0
  where count n Nil = case n of m -> m
        count n (Cons _ as) = case n + 1 of m -> case count m as of k -> k
