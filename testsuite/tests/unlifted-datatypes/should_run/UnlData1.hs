{-# LANGUAGE UnliftedDatatypes #-}

import GHC.Exts
import GHC.Types

type SList :: Type -> UnliftedType
data SList a = Cons a (SList a) | Nil

-- Sadly no IsList (SList a) or Show a => Show (SList a),
-- because type classes require lifted rep
sfromList :: [a] -> SList a
sfromList []     = Nil
sfromList (x:xs) = Cons x (sfromList xs)
stoList :: SList a -> [a]
stoList (Cons x xs) = x:stoList xs
stoList Nil         = []

sfromList2 :: [a] -> SList a
sfromList2 xs = foldl (\acc x xs -> acc (Cons x xs)) (\x -> x) xs Nil

sfromList3 :: [a] -> SList a
sfromList3 xs = foldr (\x acc xs -> Cons x (acc xs)) (\x -> x) xs Nil

sreverse :: SList a -> SList a
sreverse = go Nil
  where
    go acc Nil         = acc
    go acc (Cons x xs) = go (Cons x acc) xs

main = do
  print (stoList (sreverse (Cons 1 (Cons 2 (Cons 3 Nil)))))
  print (stoList (sreverse (sfromList [2,3,4])))
  print (stoList (sreverse (sfromList2 [3,4,5])))
  print (stoList (sreverse (sfromList3 [4,5,6])))
