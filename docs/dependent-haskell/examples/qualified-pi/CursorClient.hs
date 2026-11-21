{-# LANGUAGE BangPatterns, LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes #-}

-- | Open a checked list selection in an ordinary module.
module CursorClient (observeCursors, observeIndexedCallback, observeAligned, observeBoth, observeZip, zipTwoPairs) where

import AlignedList (checkedZipWith, zipWithExact)
import PackedVecLibrary (toList)
import Data.IORef (newIORef, readIORef)
import ListCursor (Selection, cursor, lookupAligned, positions, prepare, readBoth, withIndex)
import ParameterizedIndexLibrary (indexGeneric, mkFinGeneric)

-- | Pass the stored position once to its reader.
readSelection :: Selection a %1 -> a
readSelection s = let !(position, reader) = s in reader position

-- | Check the same selection through ordinary and linear consumers.
observeCursors :: IO ([Maybe Char], [Maybe Char])
observeCursors = do
    xs <- newIORef "abcd" >>= readIORef
    n <- newIORef 2 >>= readIORef
    let ordinary ys i = fmap (indexGeneric ys) (mkFinGeneric ys i)
        packed ys i = fmap (\c -> readSelection (prepare c)) (cursor ys i)
        positions f = [f xs n, f xs 4, f (error "forced list") (-1),
                       f (cycle "ab") 3, f ('x' : error "forced suffix") 0,
                       f (error "forced skipped element" : 'y' : error "forced suffix") 1]
    pure (positions ordinary, positions packed)

-- | Check a runtime position before calling a linear continuation.
observeIndexedCallback :: IO (Maybe String, Maybe String, Bool)
observeIndexedCallback = do
    xs <- newIORef "abcd" >>= readIORef
    n <- newIORef 2 >>= readIORef
    bottom <- newIORef (error "forced partially applied lookup" :: [Char]) >>= readIORef
    let checked ys i = case mkFinGeneric ys i of
            Nothing -> Nothing
            Just pos -> Just (withIndex ys pos (\c -> [c, c]))
    pure (checked xs n, checked bottom (-1), withIndex bottom (error "forced position") `seq` True)

-- | Reuse a checked position after comparing the lengths of two runtime lists.
observeAligned :: IO [Maybe (Char, Int)]
observeAligned = do
    xs <- newIORef "abc" >>= readIORef
    ys <- newIORef [10, 20, 30] >>= readIORef
    k <- newIORef 1 >>= readIORef
    pure [lookupAligned xs ys k, lookupAligned xs (drop 1 ys) k,
          lookupAligned (error "forced first list") (error "forced second list") (-1)]

-- | Use independent offsets on finite and cyclic lists from ordinary code.
observeBoth :: IO [Maybe (Char, Int)]
observeBoth = do
    word <- newIORef "abc" >>= readIORef
    k <- newIORef 1 >>= readIORef
    let readAt xs i ys j = case positions xs i ys j of
            Nothing -> Nothing
            Just pair -> Just (readBoth pair)
    pure [readAt word k [10, 20, 30, 40] 0,
          readAt (cycle "ab") 3 (cycle [10, 20]) 3,
          readAt word (-1) (error "forced right list") 0]

-- | Check two input lengths and use the indexed result from ordinary code.
observeZip :: IO (Maybe [(Char, Int)], Maybe [Int], Maybe Int, [(Char, Char)])
observeZip = do
    word <- newIORef "ab" >>= readIORef
    numbers <- newIORef [10, 20] >>= readIORef
    lazyWord <- newIORef [error "forced head", 'x'] >>= readIORef
    prefix <- newIORef ('x' : error "forced suffix") >>= readIORef
    let exact = fmap toList (checkedZipWith word numbers (,))
        mismatch = fmap toList (checkedZipWith word (take 1 numbers) (error "forced callback"))
        selected = case checkedZipWith lazyWord numbers (flip const) of
            Just vector -> Just (sum (toList vector))
            Nothing -> Nothing
        sameInput = take 1 (toList (zipWithExact prefix prefix (,)))
    pure (exact, mismatch, selected, sameInput)

-- | Both written lists have length two.
zipTwoPairs :: a -> a -> b -> b -> [(a, b)]
zipTwoPairs x y u v = toList (zipWithExact [x, y] [u, v] (,))
