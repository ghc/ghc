{-# LANGUAGE ViewPatterns, Modifiers, OrPatterns, UnboxedSums, NPlusKPatterns, TypeFamilies, OverloadedLists, QualifiedStrings, LambdaCase, Arrows #-}

module Main where

import Control.Exception
import Data.List
import Data.Tree
import Trace.Hpc.Mix
import Trace.Hpc.Reflect
import Trace.Hpc.Tix
import Trace.Hpc.Util
import qualified Data.String as S
import Control.Arrow

testView :: Int -> Bool
testView (id -> 1) = True
testView _ = False

testLazyForced :: Int -> Int
testLazyForced ~(id -> x) = x + 1

testLazyUnforced :: Int -> Int
testLazyUnforced ~(id -> x) = 42

testAsCovered :: Int -> Int
testAsCovered x@(id -> y) = x + y

testAsUncovered :: Int -> Int
testAsUncovered ~(_x@(id -> _y)) = 42

testBangCovered :: Int -> Int
testBangCovered !(id -> x) = x + 1

testBangUncovered :: Int -> Int
testBangUncovered ~(!(id -> x)) = 42

testSigCovered :: Int -> Int
testSigCovered ((id -> x) :: Int) = x + 1

testSigUncovered :: Int -> Int
testSigUncovered ((id -> x) :: Int) = 42

testModifiedCovered :: Int -> Int
testModifiedCovered (%() (id -> x)) = x + 1

testModifiedUncovered :: Int -> Int
testModifiedUncovered (%() (id -> x)) = 42

testList :: [Int] -> Int
testList [id -> x] = x + 1
testList [] = 42

testTuple :: (Int, Int) -> Int
testTuple (id -> x, id -> _y) = x + 1

testOr :: [Int] -> Bool
testOr (id -> []; reverse -> [_]) = True
testOr _ = False

testSum :: (# Int | Int #) -> Int
testSum (# (id -> x) | #) = x + 1
testSum (# | (id -> y) #) = y + 2

testNPat :: Int -> Int
testNPat 1 = 10
testNPat (-1) = 20
testNPat _ = 30

testNPlusKPat :: Int -> Int
testNPlusKPat (n+3) = n * 2
testNPlusKPat _ = 0

type family CoList a where
  CoList Bool = [Int]

testCoPat :: CoList Bool -> Int
testCoPat [id -> x] = x + 1
testCoPat [] = 0

testQualifiedString :: String -> Int
testQualifiedString S."hello" = 1
testQualifiedString _ = 0

testPrefix :: Maybe Int -> Int
testPrefix (Just (id -> x)) = x + 1
testPrefix _ = 0

testInfix :: [Int] -> Int
testInfix ((id -> x) : _) = x + 1
testInfix _ = 0

data Record = Record { recVal :: Int }

testRec :: Record -> Int
testRec Record{ recVal = (id -> x) } = x + 1

testLambda :: Int -> Int
testLambda = \(id -> x) -> x + 1

testLambdaCase :: Int -> Int
testLambdaCase = \case
  (id -> 5) -> 1
  _ -> 0

testLambdaCases :: Int -> Int -> Int
testLambdaCases = \cases
  (id -> 5) (id -> y) -> y + 1
  _ _ -> 0

testCase :: Int -> Int
testCase n = case n of
  (id -> 5) -> 1
  _ -> 0

testDoExpansion :: Maybe Int -> Maybe Int
testDoExpansion m = do
  (id -> 5) <- m
  pure 42

testPatBindLet :: Int -> Int
testPatBindLet n =
  let (id -> x) = n
      (id -> _) = undefined
  in x + 1

testPatBindWhere :: Int -> Int
testPatBindWhere n = x + 1
  where (id -> x) = n

testPatBindComp :: Int -> Int
testPatBindComp n =
  head [ x + 1
       | let (id -> x) = n
       , let (id -> _) = undefined ]

testPatBindGuard :: Int -> Int
testPatBindGuard n
  | let (id -> x) = n, x > 0 = x + 1
  | otherwise = 0

testPatBindDo :: Int -> Maybe Int
testPatBindDo n = do
  let (id -> x) = n
  pure (x + 1)

testPatBindCmdLet :: Int -> Int
testPatBindCmdLet = proc n ->
  let (id -> x) = n
  in returnA -< x + 1

(id -> testTopValue) = (5 :: Int)

testPatBindTopLevel :: Int
testPatBindTopLevel = testTopValue + 1

testCmdLam :: Int -> Int
testCmdLam = proc n ->
  (\(id -> x) -> returnA -< x + 1) n

testCmdCase :: Int -> Int
testCmdCase = proc n ->
  case n of
    (id -> 5) -> returnA -< 1
    _ -> returnA -< 0

testCmdLambdaCase :: Int -> Int
testCmdLambdaCase = proc n ->
  (\case
    (id -> 5) -> returnA -< 1
    _ -> returnA -< 0) n

testCmdLambdaCases :: Int -> Int
testCmdLambdaCases = proc n ->
  (\cases
    (id -> 5) (id -> y) -> returnA -< y + 1
    _ _ -> returnA -< 0) n n

main :: IO ()
main = do
  evaluate (testView 1)
  evaluate (testLazyForced 5)
  evaluate (testLazyUnforced undefined)
  evaluate (testAsCovered 5)
  evaluate (testAsUncovered undefined)
  evaluate (testBangCovered 5)
  evaluate (testBangUncovered undefined)
  evaluate (testSigCovered 5)
  evaluate (testSigUncovered undefined)
  evaluate (testModifiedCovered 5)
  evaluate (testModifiedUncovered undefined)
  evaluate (testList [5])
  evaluate (testList [])
  evaluate (testTuple (5, undefined))
  evaluate (testOr [])
  evaluate (testOr [5])
  evaluate (testOr [5, 6])
  evaluate (testSum (# 5 | #))
  evaluate (testNPat 1)
  evaluate (testNPat (-1))
  evaluate (testNPlusKPat 5)
  evaluate (testNPlusKPat 2)
  evaluate (testCoPat [5])
  evaluate (testCoPat [])
  evaluate (testQualifiedString "hello")
  evaluate (testQualifiedString "world")
  evaluate (testPrefix (Just 5))
  evaluate (testPrefix Nothing)
  evaluate (testInfix [5])
  evaluate (testInfix [])
  evaluate (testRec (Record 5))
  evaluate (testLambda 5)
  evaluate (testLambdaCase 5)
  evaluate (testLambdaCase 6)
  evaluate (testLambdaCases 5 7)
  evaluate (testLambdaCases 6 7)
  evaluate (testCase 5)
  evaluate (testCase 6)
  evaluate (maybe 0 id (testDoExpansion (Just 5)))
  evaluate (maybe 0 id (testDoExpansion (Just 6)))
  evaluate (maybe 0 id (testDoExpansion Nothing))
  evaluate (testPatBindLet 5)
  evaluate (testPatBindWhere 5)
  evaluate (testPatBindComp 5)
  evaluate (testPatBindGuard 5)
  evaluate (testPatBindGuard (-5))
  evaluate (maybe 0 id (testPatBindDo 5))
  evaluate (testPatBindCmdLet 5)
  evaluate testPatBindTopLevel
  evaluate (testCmdLam 5)
  evaluate (testCmdCase 5)
  evaluate (testCmdCase 6)
  evaluate (testCmdLambdaCase 5)
  evaluate (testCmdLambdaCase 6)
  evaluate (testCmdLambdaCases 5)
  evaluate (testCmdLambdaCases 6)
  dumpCoverage

dumpCoverage :: IO ()
dumpCoverage = do
  Mix path _ _ _ entries <- readMix [".hpc"] (Left "Main")
  Tix [TixModule "Main" _ _ counts] <- examineTix
  src <- readFileUtf8 path
  putStr . compactForest . map (fmap (describe src)) . filter interesting $
    createMixEntryDom [ (pos, (label, count)) | ((pos, label), count) <- zip entries counts ]
  where
    interesting (Node (_, boxes) _) = any (interestingBox . fst) boxes
    interestingBox (TopLevelBox [name]) = "test" `isPrefixOf` name
    interestingBox ExpBox{} = True
    interestingBox _ = False
    describe src (pos, boxes) =
      intercalate ", " [ describeBox label ++ ": " ++ show count | (label, count) <- boxes ]
        ++ if any (hasSource . fst) boxes then " " ++ show (sourceAt src pos) else ""
    hasSource (ExpBox{}; BinBox{}) = True
    hasSource _ = False
    describeBox (TopLevelBox names) = intercalate "." names
    describeBox (ExpBox False) = "exp"
    describeBox (ExpBox True) = "exp-alt"
    describeBox (BinBox kind side) = show kind ++ " " ++ show side
    compactForest = unlines . filter (not . all (`elem` " |")) . lines . drawForest

-- candidate for HPC.Utils, maybe?
sourceAt :: String -> HpcPos -> String
sourceAt src pos
  | l1 == l2 = take (c2 - c1 + 1) . drop (c1 - 1) $ ls !! (l1 - 1)
  | otherwise = intercalate "\n" $ first : (middle ++ [last])
  where
    (l1, c1, l2, c2) = fromHpcPos pos
    ls = lines src
    first = drop (c1 - 1) $ ls !! (l1 - 1)
    middle = take (l2 - l1 - 1) $ drop l1 ls
    last = take c2 $ ls !! (l2 - 1)
