{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Read
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exiting the program.
--
-----------------------------------------------------------------------------

module Text.Read (
   ReadS, 		-- String -> Maybe (a,String)
   Read(
      readsPrec,	-- :: Int -> ReadS a
      readList		-- :: ReadS [a]
    ),
   reads,		-- :: (Read a) => ReadS a
   read,		-- :: (Read a) => String -> a
   readParen, 		-- :: Bool -> ReadS a -> ReadS a
   lex,			-- :: ReadS String
 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Read
#endif   
