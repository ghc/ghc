{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- In the test suite, so OK

module Main(main) where

import Safe
import Safe.Exact
import qualified Safe.Foldable as F

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO.Unsafe
import Test.QuickCheck.Test
import Test.QuickCheck hiding ((===))


---------------------------------------------------------------------
-- TESTS

main :: IO ()
main = do
    -- All from the docs, so check they match
    tailMay dNil === Nothing
    tailMay [1,3,4] === Just [3,4]
    tailDef [12] [] === [12]
    tailDef [12] [1,3,4] === [3,4]
    tailNote "help me" dNil `err` "Safe.tailNote [], help me"
    tailNote "help me" [1,3,4] === [3,4]
    tailSafe [] === dNil
    tailSafe [1,3,4] === [3,4]

    findJust (== 2) [d1,2,3] === 2
    findJust (== 4) [d1,2,3] `err` "Safe.findJust"
    F.findJust (== 2) [d1,2,3] === 2
    F.findJust (== 4) [d1,2,3] `err` "Safe.Foldable.findJust"
    F.findJustDef 20 (== 4) [d1,2,3] === 20
    F.findJustNote "my note" (== 4) [d1,2,3] `errs` ["Safe.Foldable.findJustNote","my note"]

    takeExact 3 [d1,2] `errs` ["Safe.Exact.takeExact","index=3","length=2"]
    takeExact (-1) [d1,2] `errs` ["Safe.Exact.takeExact","negative","index=-1"]
    takeExact 1 (takeExact 3 [d1,2]) === [1] -- test is lazy

    quickCheck_ $ \(Int10 i) (List10 (xs :: [Int])) -> do
        let (t,d) = splitAt i xs
        let good = length t == i
        let f name exact may note res =
                if good then do
                    exact i xs === res
                    note "foo" i xs === res
                    may i xs === Just res
                else do
                    exact i xs `err` ("Safe.Exact." ++ name ++ "Exact")
                    note "foo" i xs `errs` ["Safe.Exact." ++ name ++ "ExactNote","foo"]
                    may i xs === Nothing
        f "take" takeExact takeExactMay takeExactNote t
        f "drop" dropExact dropExactMay dropExactNote d
        f "splitAt" splitAtExact splitAtExactMay splitAtExactNote (t, d)
        return True

    take 2 (zipExact [1,2,3] [1,2]) === [(1,1),(2,2)]
    zipExact [d1,2,3] [d1,2] `errs` ["Safe.Exact.zipExact","first list is longer than the second"]
    zipExact [d1,2] [d1,2,3] `errs` ["Safe.Exact.zipExact","second list is longer than the first"]
    zipExact dNil dNil === []

    predMay (minBound :: Int) === Nothing
    succMay (maxBound :: Int) === Nothing
    predMay ((minBound + 1) :: Int) === Just minBound
    succMay ((maxBound - 1) :: Int) === Just maxBound

    quickCheck_ $ \(List10 (xs :: [Int])) x -> do
        let ys = maybeToList x ++ xs
        let res = zip xs ys
        let f name exact may note =
                if isNothing x then do
                    exact xs ys === res
                    note "foo" xs ys === res
                    may xs ys === Just res
                else do
                    exact xs ys `err` ("Safe.Exact." ++ name ++ "Exact")
                    note "foo" xs ys `errs` ["Safe.Exact." ++ name ++ "ExactNote","foo"]
                    may xs ys === Nothing
        f "zip" zipExact zipExactMay zipExactNote
        f "zipWith" (zipWithExact (,)) (zipWithExactMay (,)) (`zipWithExactNote` (,))
        return True

    take 2 (zip3Exact [1,2,3] [1,2,3] [1,2]) === [(1,1,1),(2,2,2)]
    zip3Exact [d1,2] [d1,2,3] [d1,2,3] `errs` ["Safe.Exact.zip3Exact","first list is shorter than the others"]
    zip3Exact [d1,2,3] [d1,2] [d1,2,3] `errs` ["Safe.Exact.zip3Exact","second list is shorter than the others"]
    zip3Exact [d1,2,3] [d1,2,3] [d1,2] `errs` ["Safe.Exact.zip3Exact","third list is shorter than the others"]
    zip3Exact dNil dNil dNil === []

    quickCheck_ $ \(List10 (xs :: [Int])) x1 x2 -> do
        let ys = maybeToList x1 ++ xs
        let zs = maybeToList x2 ++ xs
        let res = zip3 xs ys zs
        let f name exact may note =
                if isNothing x1 && isNothing x2 then do
                    exact xs ys zs === res
                    note "foo" xs ys zs === res
                    may xs ys zs === Just res
                else do
                    exact xs ys zs `err` ("Safe.Exact." ++ name ++ "Exact")
                    note "foo" xs ys zs `errs` ["Safe.Exact." ++ name ++ "ExactNote","foo"]
                    may xs ys zs === Nothing
        f "zip3" zip3Exact zip3ExactMay zip3ExactNote
        f "zipWith3" (zipWith3Exact (,,)) (zipWith3ExactMay (,,)) (flip zipWith3ExactNote (,,))
        return True


---------------------------------------------------------------------
-- UTILITIES

quickCheck_ prop = do
    r <- quickCheckResult prop
    unless (isSuccess r) $ error "Test failed"


d1 = 1 :: Double
dNil = [] :: [Double]

(===) :: (Show a, Eq a) => a -> a -> IO ()
(===) a b = when (a /= b) $ error $ "Mismatch: " ++ show a ++ " /= " ++ show b

err :: NFData a => a -> String -> IO ()
err a b = errs a [b]

errs :: NFData a => a -> [String] -> IO ()
errs a bs = do
    res <- try $ evaluate $ rnf a
    case res of
        Right v -> error $ "Expected error, but succeeded: " ++ show bs
        Left (msg :: SomeException) -> forM_ bs $ \b -> do
            let s = show msg
            unless (b `isInfixOf` s) $ error $ "Invalid error string, got " ++ show s ++ ", want " ++ show b
            let f xs = " " ++ map (\x -> if sepChar x then ' ' else x) xs ++ " "
            unless (f b `isInfixOf` f s) $ error $ "Not standalone error string, got " ++ show s ++ ", want " ++ show b

sepChar x = isSpace x || x `elem` ",;."

newtype Int10 = Int10 Int deriving Show

instance Arbitrary Int10 where
    arbitrary = fmap Int10 $ choose (-3, 10)

newtype List10 a = List10 [a] deriving Show

instance Arbitrary a => Arbitrary (List10 a) where
    arbitrary = do i <- choose (0, 10); fmap List10 $ vector i

instance Testable a => Testable (IO a) where
    property = property . unsafePerformIO
