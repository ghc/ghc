{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

import GHC.Show

-- | Show a list (using square brackets and commas), given a function
-- for showing elements.
showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith = showList__
