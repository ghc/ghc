{-# LANGUAGE DataKinds, ImpredicativeTypes, NoExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies #-}
module QuickLookForward where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.Bool (bool)
import Data.IORef (newIORef, readIORef)
import Data.Proxy (Proxy(..))
import Filter (Hidden, familyHide, filterVec, fromList, partitionVec, toList)
import ForwardSchemas
import Nested (packPair)
import NestedSignature (PackedPair)
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

-- | Open the package type inferred for the argument to the reader.
firstForwarded :: Hidden a -> [a]
firstForwarded package = take 1 (listCells (id package))

-- | Open the vector selected by the conditional.
firstSelected :: Bool -> Hidden a -> Hidden a -> [a]
firstSelected flag left right =
    take 1 (listCells (if flag then id left else right))

-- | Read one cell from the selected list.
firstFromList :: Bool -> [a] -> [a] -> [a]
firstFromList flag left right =
    take 1 (listCells (if flag then fromList left else fromList right))

-- | Read the first element that passes the selected filter.
firstFromFilter :: Bool -> (a -> Bool) -> Vec n a -> Vec m a -> [a]
firstFromFilter flag keep left right =
    take 1 (listCells (if flag then filterVec keep left else filterVec keep right))

-- | Read one cell from a package produced by a case alternative.
firstFromCase :: Either (Either a [a]) [a] -> [a]
firstFromCase input = take 1 (listCells (case input of
    Left (Left x) -> fromList [x]
    Left (Right xs) -> fromList xs
    Right ys -> fromList ys))

newtype Env a = Env a

-- | Check a case which needs its expected type to check a callback.
rankedCase :: Bool -> (Int, Bool)
rankedCase flag = consume (case flag of
    True -> Env (\f -> (f (1 :: Int), f True))
    False -> Env (\f -> (f (2 :: Int), f False)))
  where
    consume :: Env ((forall b. b -> b) -> (Int, Bool)) -> (Int, Bool)
    consume (Env f) = f id

type family Forwarded (tag :: Bool) a where
    Forwarded 'True a = a

-- | Keep a package when the formal type reduces to that package.
forwardConditional :: Proxy (Hidden Int) -> Bool -> Hidden Int -> Hidden Int -> Bool
forwardConditional shape flag left right =
    accepts (Proxy :: Proxy 'True) shape (if flag then left else right)
  where
    accepts :: Proxy tag -> Proxy a -> Forwarded tag a -> Bool
    accepts _ _ _ = True

-- | Infer the result of a callback whose element type is fixed.
refilterMapped :: Vec n Int -> Hidden Int
refilterMapped xs =
    let kept = consumeVector mapThenKeep (filterVec odd xs)
    in kept
  where
    mapThenKeep :: Vec j Int -> Hidden Int
    mapThenKeep = filterVec (> 6) . increment

    increment :: Vec j Int -> Vec j Int
    increment Nil = Nil
    increment (Cons x rest) = Cons (x + 1) (increment rest)

-- | Keep both hidden lengths in the result of a locally declared callback.
refilterBoth :: forall a i j. Integral a => Vec i a -> Vec j a -> ([a], [a])
refilterBoth xs ys =
    let rows = withBoth (producer id) (filterVec odd xs) (filterVec even ys)
    in rowCells rows
  where
    producer :: forall n. (forall k. Vec k a -> Vec k a)
             -> forall m. Vec n a -> Vec m a -> PackedPair a
    producer transform left right =
        packPair (filterVec (> 3) (transform left)) (filterVec (< 10) (transform right))

    withBoth :: (forall n m. Vec n a -> Vec m a -> r) -> Vec l a -> Vec k a -> r
    withBoth use left right = use left right

    rowCells :: PackedPair a -> ([a], [a])
    rowCells rows = case rows of
        (left, right) -> (listCells left, listCells right)

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
    print (fmap toList (familyHide (Cons 13 Nil) :: Maybe (Hidden Int)))
    let oddCells = filterVec odd (Cons 7 (error "unused odd tail"))
        evenCells = filterVec even (Cons 2 (error "unused even tail"))
        unused = error "unused conditional branch"
    unless (firstForwarded oddCells == [7 :: Int]) $
        fail "opening a cached argument forced the vector tail"
    unless (firstSelected True oddCells unused == [7]
         && firstSelected False unused evenCells == [2]) $
        fail "opening a conditional changed branch or tail demand"
    unless (firstFromList True (7 : error "unused list tail") (error "unused right list") == [7 :: Int]
         && firstFromList False (error "unused left list") ('a' : error "unused list tail") == "a") $
        fail "inferring a conditional changed list demand"
    chooseLeft <- newIORef True >>= readIORef
    let input = Cons 1 (Cons (2 :: Int) (error "unused filter tail"))
    unless (firstFromFilter chooseLeft even input unused == [2]
         && firstFromFilter (not chooseLeft) even unused input == [2]
         && null (firstFromFilter chooseLeft (error "unused predicate") Nil input)) $
        fail "opening a conditional changed filter demand"
    selected <- try (evaluate (firstFromFilter chooseLeft (\_ -> error "selected predicate") input unused))
    case selected of
        Left ex | takeWhile (/= '\n') (displayException (ex :: ErrorCall)) == "selected predicate" -> pure ()
        _ -> fail "the selected filter did not raise its original exception"
    unless (forwardConditional Proxy (error "unused flag") unused unused) $
        fail "forwarding at a family domain forced a package"
    unless (firstFromCase (Left (Left 'x')) == "x"
         && firstFromCase (Left (Right ('y' : error "unused left tail"))) == "y"
         && firstFromCase (Right ('z' : error "unused right tail")) == "z") $
        fail "opening a case changed branch or tail demand"
    unless (rankedCase True == (1, True) && rankedCase False == (2, False)) $
        fail "case checking lost the expected callback type"
    let filteredCells input = toList (refilter input)
    unless (take 1 (filteredCells (Cons 3 (Cons 7 (error "unused refilter tail")))) == [7]
         && filteredCells (Cons 3 (Cons 7 (Cons 8 (Cons 9 Nil)))) == [7, 9]
         && null (filteredCells Nil)) $
        fail "the ranked callback changed filter order or demand"
    unless (take 1 (toList (refilterMapped (Cons 7 (error "unused mapped tail")))) == [8]
         && toList (refilterMapped (Cons 7 (Cons 2 Nil))) == [8]
         && null (toList (refilterMapped (Cons 5 (Cons 2 Nil))))) $
        fail "the concrete callback changed the filtered payload or demand"
    unless (refilterBoth (Cons 3 (Cons 7 Nil)) (Cons 2 (Cons 4 (Cons 10 Nil))) == ([7], [2, 4])
         && take 1 (fst (refilterBoth (Cons 7 (error "unused left tail")) (error "unused right vector"))) == [7]
         && take 1 (snd (refilterBoth (error "unused left vector") (Cons 2 (error "unused right tail")))) == [2]) $
        fail "the local ranked producer changed row lengths or demand"
