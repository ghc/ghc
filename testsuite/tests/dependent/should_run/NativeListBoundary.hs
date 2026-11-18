{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, LinearTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Main where

import Control.Exception (ArithException(DivideByZero), evaluate, throw, try)
import Data.IORef (newIORef, readIORef)
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

-- | Check an infix constructor prefix against the list conversion result.
prependInfix :: foreach (n :: Nat) -> a -> [a] -> Maybe (Vec a (1 + n))
prependInfix n x xs = VCons x <$> fromListExact n xs

-- | Check the same constructor prefix in a prefix application.
prependPrefix :: foreach (n :: Nat) -> a -> [a] -> Maybe (Vec a (1 + n))
prependPrefix n x xs = fmap (VCons x) (fromListExact n xs)

-- | Give an ordinary helper the constructor's declared type.
consHelper :: a -> Vec a n -> Vec a (1 + n)
consHelper x xs = VCons x xs

-- | Check an ordinary application prefix against the conversion result.
prependHelper :: foreach (n :: Nat) -> a -> [a] -> Maybe (Vec a (1 + n))
prependHelper n x xs = fmap (consHelper x) (fromListExact n xs)

-- | Store a constructor prefix at its declared callback type.
prependStored :: forall a. foreach (n :: Nat) -> a -> [a] -> Maybe (Vec a (1 + n))
prependStored n x xs =
    let callback :: Vec a n -> Vec a (1 + n)
        callback = VCons x
    in fmap callback (fromListExact n xs)

-- | Apply a linear callback exactly once.
applyOnce :: (a %1 -> b) %1 -> a %1 -> b
applyOnce f x = f x

-- | Consume the captured constructor field and its tail exactly once.
prependLinear :: foreach (n :: Nat) -> a %1 -> Vec a n %1 -> Vec a (1 + n)
prependLinear n x xs = applyOnce (VCons x) xs

-- | Map a callback over a vector without changing its length.
mapVec :: (a -> b) -> Vec a n -> Vec b n
mapVec _ VNil = VNil
mapVec f (VCons x xs) = VCons (f x) (mapVec f xs)

-- | Check a constructor prefix against a matrix result type.
prependRows :: foreach (cols :: Nat) -> foreach (rows :: Nat) -> a -> Vec (Vec a cols) rows -> Vec (Vec a (1 + cols)) rows
prependRows cols rows x vs = mapVec (VCons x) vs

-- | Infer the matrix result before checking its first row.
prependedRowLength :: foreach (cols :: Nat) -> foreach (rows :: Nat) -> a -> Vec (Vec a cols) rows -> Int
prependedRowLength cols rows x vs = case mapVec (VCons x) vs of
    VNil -> 0
    VCons v _ -> length (toList v)

-- | Keep the captured value as the only source of the native index.
keepValue :: a -> Bool -> a
keepValue x _ = x

-- | Apply a callback whose result type is inferred from its prefix.
runCallback :: (Bool -> a) -> [a]
runCallback f = [f False]

-- | Read the native result type from the captured vector.
capturedIndex :: foreach (n :: Nat) -> Vec Bool n -> [Bool]
capturedIndex n xs = case runCallback (keepValue @(Vec Bool n) xs) of
    v : _ -> toList v
    _ -> []

-- | Fail when the callback factory is evaluated to a function.
bottomCallback :: a -> Vec a n -> Vec a (1 + n)
bottomCallback _ = throw DivideByZero

-- | Evaluate a callback without applying it.
forceCallback :: (a -> b) -> Maybe a -> Maybe b
forceCallback f _ = f `seq` Nothing

-- | Keep the callback factory's evaluation visible to the consumer.
forcePrefix :: foreach (n :: Nat) -> a -> [a] -> Maybe (Vec a (1 + n))
forcePrefix n x xs = forceCallback (bottomCallback x) (fromListExact n xs)

-- | An ordinary constructor with a strict field and a lazy payload.
data Stamped a = Stamped !Int a

-- | Discard the stamp after forcing it to construct the pair.
discardStamp :: foreach (n :: Nat) -> Int -> Vec a n -> Vec a n
discardStamp _ k xs = case Stamped k xs of Stamped _ ys -> ys

-- | Observe only the Maybe constructor.
succeeds :: Maybe a -> Bool
succeeds Nothing = False
succeeds (Just _) = True

-- | Check whether evaluating an observation forces the marked exception.
raisesDivideByZero :: Bool -> IO Bool
raisesDivideByZero observation = do
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
    print (fmap toList (prependInfix 1 (1 :: Int) [2]))
    print (fmap toList (prependPrefix 1 (1 :: Int) [2]))
    print (fmap toList (prependHelper 1 (1 :: Int) [2]))
    print (fmap toList (prependStored 1 (1 :: Int) [2]))
    print (toList (prependLinear 1 (1 :: Int) (VCons 2 VNil)))
    print (succeeds (prependInfix 1 (throw DivideByZero :: Int) [2]))
    print (fmap (length . toList) (prependPrefix 1 (throw DivideByZero :: Int) [2]))
    print (fmap toList (prependInfix 1 (throw DivideByZero :: Int) []))
    print (fmap toList (prependHelper 1 (throw DivideByZero :: Int) []))
    sizeRef <- newIORef (2 :: Nat)
    size <- readIORef sizeRef
    height <- newIORef (3 :: Nat) >>= readIORef
    let row = replicateNative size (7 :: Int)
        rows = replicateNative height row
    print (map toList (toList (prependRows size height 1 rows)))
    print (prependedRowLength size height 1 rows)
    print (prependedRowLength size 0 (1 :: Int) VNil)
    print (capturedIndex size (replicateNative size True))
    raisesDivideByZero (succeeds (forcePrefix size (1 :: Int) [])) >>= print
    print (succeeds (fromListExact (S Z) [throw DivideByZero :: Int]))
    print (fmap (length . toList) (fromListExact (S Z) [throw DivideByZero :: Int]))
    raisesDivideByZero (succeeds (fromListExact (S Z) (1 : throw DivideByZero :: [Int]))) >>= print
    raisesDivideByZero (succeeds (fromListExact (S (S Z)) (1 : throw DivideByZero :: [Int]))) >>= print
    print (succeeds (fromListExact Z (throw DivideByZero : throw DivideByZero :: [Int])))
    print (succeeds (fromListExact (S Z) (1 : 2 : throw DivideByZero :: [Int])))
    print (succeeds (fromListExact (S (S Z)) (repeat (throw DivideByZero :: Int))))
    _ <- evaluate (packList (throw DivideByZero :: [Int]))
    print (headPacked (packList (7 : throw DivideByZero :: [Int])))
    print (take 3 (toListPacked (packList ([1 ..] :: [Int]))))
    print (headPacked (clientPack (9 : throw DivideByZero :: [Int])), headPacked (clientPackAt "x"))
    raisesDivideByZero (null (toList (discardStamp size (throw DivideByZero) row))) >>= print
