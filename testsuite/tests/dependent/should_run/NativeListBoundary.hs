{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, ScopedTypeVariables, TypeApplications #-}
module Main where

import Control.Exception (ArithException(DivideByZero), evaluate, throw, try)
import OrdinaryVecClient (ordinaryZipExact)
import PackedVecLibrary

-- | Match an ordinary list and an indexed vector after the retained index.
matrix :: foreach (n :: Nat) -> [Int] -> Vec Bool n -> Int
matrix Z [] VNil = 0
matrix Z (_ : _) VNil = 1
matrix (S _) [] (VCons _ _) = 2
matrix (S _) (_ : _) (VCons _ _) = 3

-- | Preserve the first matching equation for each retained constructor.
ordered :: foreach (n :: Nat) -> [Int] -> Int
ordered Z [] = 11
ordered Z _ = 10
ordered (S _) (_ : _) = 20
ordered (S _) _ = 21

-- | Match a vector at the refined length in a multi-argument lambda.
vectorShape :: foreach (n :: Nat) -> Vec Int n -> String
vectorShape = \cases { Z VNil -> "zero"; (S _) (VCons _ _) -> "successor" }

-- | Try an ordinary literal before its same-constructor fallback.
literal :: foreach (n :: Nat) -> Int -> Int
literal Z 0 = 30
literal Z x = x
literal (S _) 0 = 40
literal (S _) x = x

-- | Construct the package directly in a client module.
clientPack :: [a] -> PackedVec a
clientPack xs = PackVec (sourceLength xs) (sourceVector xs)

-- | Specify the same erased element type that the first operand determines.
clientPackAt :: forall a. [a] -> PackedVec a
clientPackAt xs = PackVec (sourceLength @a xs) (sourceVector xs)

-- | Observe only the Maybe constructor.
succeeds :: Maybe a -> Bool
succeeds Nothing = False
succeeds (Just _) = True

-- | Check whether an observation demands the marked undefined spine.
demandsSpine :: Bool -> IO Bool
demandsSpine observation = do
    result <- try (evaluate observation)
    pure (result == Left DivideByZero)

main :: IO ()
main = do
    print [matrix Z [] VNil, matrix Z [1] VNil,
           matrix (S Z) [] (VCons True VNil),
           matrix (S Z) [1] (VCons True VNil)]
    print [ordered Z [], ordered Z [1], ordered (S Z) [], ordered (S Z) [1]]
    print (vectorShape Z VNil, vectorShape (S Z) (VCons 7 VNil))
    print [literal Z 0, literal Z 7, literal (S Z) 0, literal (S Z) 8]
    print (ordinaryZipExact (S (S Z)) [1, 2 :: Int] ['a', 'b'])
    print (ordinaryZipExact (S (S Z)) [1 :: Int] (throw DivideByZero :: [Char]))
    print (ordinaryZipExact (S (S Z)) [1, 2 :: Int] ['a', 'b', 'c'])
    print (ordinaryZipExact Z ([] :: [Int]) ([] :: [Bool]))
    print (succeeds (fromListExact (S Z) [throw DivideByZero :: Int]))
    print (fmap (length . toList) (fromListExact (S Z) [throw DivideByZero :: Int]))
    demandsSpine (succeeds (fromListExact (S Z) (1 : throw DivideByZero :: [Int]))) >>= print
    demandsSpine (succeeds (fromListExact (S (S Z)) (1 : throw DivideByZero :: [Int]))) >>= print
    print (succeeds (fromListExact Z (throw DivideByZero : throw DivideByZero :: [Int])))
    print (succeeds (fromListExact (S Z) (1 : 2 : throw DivideByZero :: [Int])))
    print (succeeds (fromListExact (S (S Z)) (repeat (throw DivideByZero :: Int))))
    _ <- evaluate (packList (throw DivideByZero :: [Int]))
    print (headPacked (packList (7 : throw DivideByZero :: [Int])))
    print (take 3 (toListPacked (packList ([1 ..] :: [Int]))))
    print (headPacked (clientPack (9 : throw DivideByZero :: [Int])), headPacked (clientPackAt "x"))
