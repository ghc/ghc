{-# LANGUAGE Safe #-}


-- |
--
-- Module      :  GHC.List
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The List data type and its operations
--

module GHC.List (

   -- * The list data type
   List,

   -- * List-monomorphic Foldable methods and misc functions
   foldr, foldr', foldr1,
   foldl, foldl', foldl1,
   null, length, elem, notElem,
   maximum, minimum, sum, product, and, or, any, all,

   -- * Other functions
   foldl1', concat, concatMap,
   map, (++), filter, lookup,
   head, last, tail, init, uncons, unsnoc, (!?), (!!),
   scanl, scanl1, scanl', scanr, scanr1,
   iterate, iterate', repeat, replicate, cycle,
   take, drop, splitAt, takeWhile, dropWhile, span, break, reverse,
   zip, zip3, zipWith, zipWith3, unzip, unzip3,
   errorEmptyList,

   -- * GHC List fusion
   augment, build,

 ) where

import GHC.Internal.List
