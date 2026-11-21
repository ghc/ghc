{-# LANGUAGE ImpredicativeTypes, NoExperimentalDependentTypes #-}
module Main where

import ImplicitVector (Filtered, cells, filterVec, withVector)
import PackedVecLibrary (Vec(..))
import ScopedClassMethod (Choose(..), Tagged(..), chooseTagged)

newtype Receiver = Receiver Bool
instance Choose Receiver where
    choose (Receiver b) = b

select :: Choose a => a -> Vec Int n -> [Int]
select receiver input =
    let result :: Filtered Int
        result = withVector
            (\xs -> filterVec (\_ -> case chooseTagged receiver of Tagged b -> b) xs)
            (filterVec (const True) input)
    in cells result

{-# INLINABLE countKey #-}
countKey :: Eq a => a -> [a] -> Int
countKey _ [] = 0
countKey key (x : xs) = fromEnum (key == x) + countKey key xs

main :: IO ()
main = print (take 1 (select (Receiver True) (VCons 7 (error "unused tail"))),
              select (Receiver False) (VCons 9 VNil),
              select (error "unused receiver" :: Receiver) VNil,
              countKey (2 :: Int) [4, 2, 2])
