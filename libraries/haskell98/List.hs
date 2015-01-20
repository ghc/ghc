{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module List (
        elemIndex, elemIndices,
        find, findIndex, findIndices,
        nub, nubBy, delete, deleteBy, (\\), deleteFirstsBy,
        union, unionBy, intersect, intersectBy,
        intersperse, transpose, partition, group, groupBy,
        inits, tails, isPrefixOf, isSuffixOf,
        mapAccumL, mapAccumR,
        sort, sortBy, insert, insertBy, maximumBy, minimumBy,
        genericLength, genericTake, genericDrop,
        genericSplitAt, genericIndex, genericReplicate,
        zip4, zip5, zip6, zip7,
        zipWith4, zipWith5, zipWith6, zipWith7,
        unzip4, unzip5, unzip6, unzip7, unfoldr,

        -- ...and what the Prelude exports
        -- []((:), []), -- This is built-in syntax
        map, (++), concat, filter,
        head, last, tail, init, null, length, (!!),
        foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
        iterate, repeat, replicate, cycle,
        take, drop, splitAt, takeWhile, dropWhile, span, break,
        lines, words, unlines, unwords, reverse, and, or,
        any, all, elem, notElem, lookup,
        sum, product, maximum, minimum, concatMap,
        zip, zip3, zipWith, zipWith3, unzip, unzip3
    ) where

import Data.OldList hiding (foldl', splitAt)
