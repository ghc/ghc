{-# LANGUAGE CPP #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Word64Map
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- An efficient implementation of maps from integer keys to values
-- (dictionaries).
--
-- This module re-exports the value lazy "Data.Word64Map.Lazy" API, plus
-- several deprecated value strict functions. Please note that these functions
-- have different strictness properties than those in "Data.Word64Map.Strict":
-- they only evaluate the result of the combining function. For example, the
-- default value to 'insertWith'' is only evaluated if the combining function
-- is called and uses it.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.Word64Map (Word64Map)
-- >  import qualified Data.Word64Map as Word64Map
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced map implementation (see "Data.Map").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve Information Coded In Alphanumeric/\",
--      Journal of the ACM, 15(4), October 1968, pages 514-534.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-- Many operations have a worst-case complexity of \(O(\min(n,64))\).
-- This means that the operation can become linear in the number of
-- elements with a maximum of \(64\)
-----------------------------------------------------------------------------

module GHC.Data.Word64Map
    ( module GHC.Data.Word64Map.Lazy
    ) where

import GHC.Data.Word64Map.Lazy