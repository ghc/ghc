{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import GHC.Exts (build)

newtype Producer = Producer (forall r. (Bool -> r -> r) -> r -> r)

-- | The list-producing map equations, with a call that remains opaque.
{-# NOINLINE noFusionMap #-}
noFusionMap :: (a -> b) -> [a] -> [b]
noFusionMap _ [] = []
noFusionMap f (x : xs) = f x : noFusionMap f xs

{-# NOINLINE consumeMap #-}
consumeMap :: Producer -> Bool
consumeMap (Producer produce) =
    foldr (\_ _ -> True) (error "empty seed") (map not (build produce))

{-# NOINLINE consumeNoFusionMap #-}
consumeNoFusionMap :: Producer -> Bool
consumeNoFusionMap (Producer produce) =
    foldr (\_ _ -> True) (error "empty seed") (noFusionMap not (build produce))

{-# NOINLINE mapEscapes #-}
mapEscapes :: Producer -> [Bool]
mapEscapes (Producer produce) = map not (build produce)

{-# NOINLINE consumeZip #-}
consumeZip :: Producer -> [Bool] -> Bool
consumeZip (Producer produce) ys =
    foldr (\_ _ -> True) (error "empty seed") (zipWith (&&) (map not (build produce)) ys)

-- | Inspect only the outer constructor of a filtered list.
{-# NOINLINE emptySpine #-}
emptySpine :: [a] -> Bool
emptySpine [] = True
emptySpine (_ : _) = False

-- | Make the input producer visible at the filter call.
{-# NOINLINE consumeFilter #-}
consumeFilter :: Producer -> Bool -> Bool
consumeFilter (Producer produce) accept = emptySpine (filter (const accept) (build produce))

-- | Pass the same list across a call before applying the filter.
{-# NOINLINE consumeFilterList #-}
consumeFilterList :: [Bool] -> Bool -> Bool
consumeFilterList xs accept = emptySpine (filter (const accept) xs)

-- | Force the result of the supplied list step before producing one cell.
{-# NOINLINE checkCons #-}
checkCons :: (Bool -> r -> r) -> r -> r
checkCons c n = c False (error "filter tail") `seq` c True n

-- | Observe the filtered cons and also return the filtered tail.
{-# NOINLINE sharedFilter #-}
sharedFilter :: (Bool -> Bool) -> Bool -> (Bool, [Bool])
sharedFilter p x =
    let xs = build checkCons
        rest = filter p xs
    in (foldr (\y _ -> y) (error "empty seed") (filter p (x : xs)), rest)

-- | Apply the cons equation with the same producer, fold, and shared tail.
{-# NOINLINE sharedFilterStep #-}
sharedFilterStep :: (Bool -> Bool) -> Bool -> (Bool, [Bool])
sharedFilterStep p x =
    let xs = build checkCons
        rest = filter p xs
    in (foldr (\y _ -> y) (error "empty seed") (if p x then x : rest else rest), rest)

main :: IO ()
main = do
    let withSeq = Producer (\c n -> n `seq` c True n)
        withoutSeq = Producer (\c n -> c True n)
    observe (consumeMap withSeq)
    observe (consumeNoFusionMap withSeq)
    observe (consumeMap withoutSeq)
    observe (not (null (mapEscapes withSeq)))
    observe (consumeZip withSeq [False])
    observe (consumeFilter (Producer checkCons) False)
    observe (consumeFilterList (build checkCons) False)
    observe (consumeFilter (Producer checkCons) True)
    observe (fst (sharedFilter id False))
    observe (fst (sharedFilterStep id False))
  where
    observe result = do
        value <- try (evaluate result) :: IO (Either ErrorCall Bool)
        putStrLn (either (takeWhile (/= '\n') . displayException) show value)
