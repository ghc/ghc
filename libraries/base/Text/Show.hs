{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Text.Show
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Show.hs,v 1.1 2001/06/28 14:15:04 simonmar Exp $
--
-- Exiting the program.
--
-----------------------------------------------------------------------------

module Text.Show (
   ShowS,	 	-- String -> String
   Show(
      showsPrec,	-- :: Int -> a -> ShowS
      show,		-- :: a   -> String
      showList		-- :: [a] -> ShowS 
    ),
   shows,		-- :: (Show a) => a -> ShowS
   showChar,		-- :: Char -> ShowS
   showString,		-- :: String -> ShowS
   showParen,		-- :: Bool -> ShowS -> ShowS
 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Show
#endif   

