{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Converting values to readable strings:
-- the 'Show' class and associated functions.
--
-----------------------------------------------------------------------------

module Text.Show (
   ShowS,
   Show(showsPrec, show, showList),
   shows,
   showChar,
   showString,
   showParen,
   showListWith,
 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Show
#endif

-- | Show a list (using square brackets and commas), given a function
-- for showing elements.
showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith = showList__

#ifndef __GLASGOW_HASKELL__
showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)
#endif

