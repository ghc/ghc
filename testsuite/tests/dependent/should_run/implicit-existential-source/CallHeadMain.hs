{-# LANGUAGE GADTs, LinearTypes #-}

module CallHeadMain where

import Filter (Hidden, filterVec, toList)
import Functions
import Vector

repack :: Function a -> a -> Hidden a
repack f a = f a

repackOne :: Bool -> Hidden Bool
repackOne a = one a

list :: Vec n a -> [a]
list Nil = []
list (Cons a rest) = a : list rest

immediate :: Function a -> a -> [a]
immediate f a = list (f a)

undemanded :: Function a -> a -> Bool
undemanded f a = ignore (f a)
  where
    ignore :: Vec n a -> Bool
    ignore _ = True

independent :: Function a -> Function a -> a -> ([a], [a])
independent f g a = paired (f a) (g a)
  where
    paired :: Vec n a -> Vec m a -> ([a], [a])
    paired xs ys = (list xs, list ys)

linear :: Function a %1 -> a -> Hidden a
linear f a = f a

linearNested :: Function a %1 -> a -> Hidden a
linearNested f a = move (f a)
  where
    move :: Vec n a %1 -> Vec n a
    move xs = xs

runBuilder :: Function a -> (a -> Bool) -> a -> [a]
runBuilder builder p x =
    let selected = filterVec p (builder x)
    in toList selected

main :: IO ()
main = do
    print (toList (repack one True), immediate empty (error "used the empty argument" :: Bool))
    print (independent one empty True, independent empty one False)
    print (undemanded (error "forced the package" :: Function Bool) True, undemanded bodyTrap True)
    print (toList (linear one True), toList (linearNested empty False))
    print (toList (repackOne False))
    print (runBuilder one odd (7 :: Int), runBuilder one odd 2,
           runBuilder empty odd (error "empty forced its argument" :: Int))
