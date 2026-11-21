{-# LANGUAGE ImpredicativeTypes, NoExperimentalDependentTypes #-}
module QuickLookForward where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.Bool (bool)
import Filter (Hidden, filterVec, partitionVec, toList)
import ForwardSchemas
import Vector (Vec(..), consumeVector)

apply :: (a -> Int) -> a -> Int
apply f x = f x

missing :: SlotBox Int
missing = error "unused package"

listCells :: Vec n a -> [a]
listCells = foldr (:) []

partitionHeads :: Vec n Int -> ([Int], [Int])
partitionHeads xs =
    (take 1 (listCells (fst (partitionVec odd xs))),
     take 1 (listCells (snd (partitionVec odd xs))))

refilter :: Vec n Int -> Hidden Int
refilter xs =
    let ys = consumeVector (filterVec (> 3)) (filterVec odd xs)
    in ys

main :: IO ()
main = do
    print (apply (\h -> slotLabel h) (id (slotBox (7 :: Int))),
           apply (\h -> inspectRanked h) (id ranked),
           slotLabel $ bool missing (slotBox (9 :: Int)) True)
    forced <- try (evaluate (slotLabel (bool missing (slotBox (0 :: Int)) False)))
    case forced of
        Left ex | takeWhile (/= '\n') (displayException (ex :: ErrorCall)) == "unused package" ->
            putStrLn "selected package raised the original exception"
        _ -> fail "unexpected package demand"
    print (partitionHeads (Cons 7 (Cons 2 (error "unused partition tail"))))
    let filteredCells input = toList (refilter input)
    unless (take 1 (filteredCells (Cons 3 (Cons 7 (error "unused refilter tail")))) == [7]
         && filteredCells (Cons 3 (Cons 7 (Cons 8 (Cons 9 Nil)))) == [7, 9]
         && null (filteredCells Nil)) $
        fail "the ranked callback changed filter order or demand"
