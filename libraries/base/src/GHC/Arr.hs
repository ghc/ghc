{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Arr
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/393>)
-- Portability :  non-portable (GHC extensions)
--
-- GHC\'s array implementation.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.Arr should be removed in GHC 10.02."
#endif

module GHC.Arr
  {-# DEPRECATED "GHC.Arr is deprecated and will be removed in GHC 10.02. Please use the array package." #-}
    (Ix(..),
     Array(..),
     STArray(..),
     arrEleBottom,
     array,
     listArray,
     (!),
     safeRangeSize,
     negRange,
     safeIndex,
     badSafeIndex,
     bounds,
     numElements,
     numElementsSTArray,
     indices,
     elems,
     assocs,
     accumArray,
     adjust,
     (//),
     accum,
     amap,
     ixmap,
     eqArray,
     cmpArray,
     cmpIntArray,
     newSTArray,
     boundsSTArray,
     readSTArray,
     writeSTArray,
     freezeSTArray,
     thawSTArray,
     foldlElems,
     foldlElems',
     foldl1Elems,
     foldrElems,
     foldrElems',
     foldr1Elems,
     -- *  Unsafe operations
     fill,
     done,
     unsafeArray,
     unsafeArray',
     lessSafeIndex,
     unsafeAt,
     unsafeReplace,
     unsafeAccumArray,
     unsafeAccumArray',
     unsafeAccum,
     unsafeReadSTArray,
     unsafeWriteSTArray,
     unsafeFreezeSTArray,
     unsafeThawSTArray
     ) where

import GHC.Internal.Arr
