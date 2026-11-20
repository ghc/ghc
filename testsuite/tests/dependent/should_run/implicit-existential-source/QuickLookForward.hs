{-# LANGUAGE ImpredicativeTypes, NoExperimentalDependentTypes #-}
module QuickLookForward where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.Bool (bool)
import Filter (partitionVec)
import ForwardSchemas
import Vector (Vec(..))

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
