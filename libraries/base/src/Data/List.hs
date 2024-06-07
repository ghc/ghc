{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.List
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Operations on lists.
--

module Data.List
    (List,
     -- *  Basic functions
     (++),
     head,
     last,
     tail,
     init,
     uncons,
     unsnoc,
     singleton,
     null,
     length,
     compareLength,
     -- *  List transformations
     map,
     reverse,
     intersperse,
     intercalate,
     transpose,
     subsequences,
     permutations,
     -- *  Reducing lists (folds)
     foldl,
     foldl',
     foldl1,
     foldl1',
     foldr,
     foldr1,
     -- **  Special folds
     concat,
     concatMap,
     and,
     or,
     any,
     all,
     sum,
     product,
     maximum,
     minimum,
     -- *  Building lists
     -- **  Scans
     scanl,
     scanl',
     scanl1,
     scanr,
     scanr1,
     -- **  Accumulating maps
     mapAccumL,
     mapAccumR,
     -- **  Infinite lists
     iterate,
     iterate',
     repeat,
     replicate,
     cycle,
     -- **  Unfolding
     unfoldr,
     -- *  Sublists
     -- **  Extracting sublists
     take,
     drop,
     splitAt,
     takeWhile,
     dropWhile,
     dropWhileEnd,
     span,
     break,
     stripPrefix,
     group,
     inits,
     inits1,
     tails,
     tails1,
     -- **  Predicates
     isPrefixOf,
     isSuffixOf,
     isInfixOf,
     isSubsequenceOf,
     -- *  Searching lists
     -- **  Searching by equality
     elem,
     notElem,
     lookup,
     -- **  Searching with a predicate
     find,
     filter,
     partition,
     -- *  Indexing lists
     -- |  These functions treat a list @xs@ as an indexed collection,
     -- with indices ranging from 0 to @'length' xs - 1@.
     (!?),
     (!!),
     elemIndex,
     elemIndices,
     findIndex,
     findIndices,
     -- *  Zipping and unzipping lists
     zip,
     zip3,
     zip4,
     zip5,
     zip6,
     zip7,
     zipWith,
     zipWith3,
     zipWith4,
     zipWith5,
     zipWith6,
     zipWith7,
     unzip,
     unzip3,
     unzip4,
     unzip5,
     unzip6,
     unzip7,
     -- *  Special lists
     -- **  Functions on strings
     lines,
     words,
     unlines,
     unwords,
     -- **  \"Set\" operations
     nub,
     delete,
     (\\),
     union,
     intersect,
     -- **  Ordered lists
     sort,
     sortOn,
     insert,
     -- *  Generalized functions
     -- **  The \"@By@\" operations
     -- |  By convention, overloaded functions have a non-overloaded
     -- counterpart whose name is suffixed with \`@By@\'.
     --
     -- It is often convenient to use these functions together with
     -- 'Data.Function.on', for instance @'sortBy' ('Prelude.compare'
     -- ``Data.Function.on`` 'Prelude.fst')@.

     -- ***  User-supplied equality (replacing an @Eq@ context)
     -- |  The predicate is assumed to define an equivalence.
     nubBy,
     deleteBy,
     deleteFirstsBy,
     unionBy,
     intersectBy,
     groupBy,
     -- ***  User-supplied comparison (replacing an @Ord@ context)
     -- |  The function is assumed to define a total ordering.
     sortBy,
     insertBy,
     maximumBy,
     minimumBy,
     -- **  The \"@generic@\" operations
     -- |  The prefix \`@generic@\' indicates an overloaded function that
     -- is a generalized version of a "Prelude" function.
     genericLength,
     genericTake,
     genericDrop,
     genericSplitAt,
     genericIndex,
     genericReplicate
     ) where

import GHC.Internal.Data.Bool (otherwise)
import GHC.Internal.Data.List
import GHC.Internal.Data.List.NonEmpty (NonEmpty(..))
import GHC.Internal.Data.Ord (Ordering(..), (<), (>))
import GHC.Internal.Int (Int)
import GHC.Internal.Num ((-))
import GHC.List (build)

inits1, tails1 :: [a] -> [NonEmpty a]

-- | The 'inits1' function returns all non-empty initial segments of the
-- argument, shortest first.
--
-- @since 4.21.0.0
--
-- ==== __Laziness__
--
-- Note that 'inits1' has the following strictness property:
-- @inits1 (xs ++ _|_) = inits1 xs ++ _|_@
--
-- In particular,
-- @inits1 _|_ = _|_@
--
-- ==== __Examples__
--
-- >>> inits1 "abc"
-- ['a' :| "",'a' :| "b",'a' :| "bc"]
--
-- >>> inits1 []
-- []
--
-- inits1 is productive on infinite lists:
--
-- >>> take 3 $ inits1 [1..]
-- [1 :| [],1 :| [2],1 :| [2,3]]
inits1 [] = []
inits1 (x : xs) = map (x :|) (inits xs)

-- | \(\mathcal{O}(n)\). The 'tails1' function returns all non-empty final
-- segments of the argument, longest first.
--
-- @since 4.21.0.0
--
-- ==== __Laziness__
--
-- Note that 'tails1' has the following strictness property:
-- @tails1 _|_ = _|_@
--
-- >>> tails1 undefined
-- *** Exception: Prelude.undefined
--
-- >>> drop 1 (tails1 [undefined, 1, 2])
-- [1 :| [2],2 :| []]
--
-- ==== __Examples__
--
-- >>> tails1 "abc"
-- ['a' :| "bc",'b' :| "c",'c' :| ""]
--
-- >>> tails1 [1, 2, 3]
-- [1 :| [2,3],2 :| [3],3 :| []]
--
-- >>> tails1 []
-- []
{-# INLINABLE tails1 #-}
tails1 lst = build (\c n ->
  let tails1Go [] = n
      tails1Go (x : xs) = (x :| xs) `c` tails1Go xs
  in tails1Go lst)

-- | Use 'compareLength' @xs@ @n@ as a safer and faster alternative
-- to 'compare' ('length' @xs@) @n@. Similarly, it's better
-- to write @compareLength xs 10 == LT@ instead of @length xs < 10@.
--
-- While 'length' would force and traverse
-- the entire spine of @xs@ (which could even diverge if @xs@ is infinite),
-- 'compareLength' traverses at most @n@ elements to determine its result.
--
-- >>> compareLength [] 0
-- EQ
-- >>> compareLength [] 1
-- LT
-- >>> compareLength ['a'] 1
-- EQ
-- >>> compareLength ['a', 'b'] 1
-- GT
-- >>> compareLength [0..] 100
-- GT
-- >>> compareLength undefined (-1)
-- GT
-- >>> compareLength ('a' : undefined) 0
-- GT
--
-- @since 4.21.0.0
--
compareLength :: [a] -> Int -> Ordering
compareLength xs n
  | n < 0 = GT
  | otherwise = foldr
    (\_ f m -> if m > 0 then f (m - 1) else GT)
    (\m -> if m > 0 then LT else EQ)
    xs
    n
