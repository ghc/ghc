{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (only on platforms that provide a regex lib)
--
-- Regular expression matching.  Uses the POSIX regular expression
-- interface in "Text.Regex.Posix".
--
-----------------------------------------------------------------------------
#include "config.h"
module Text.Regex (
    -- * Regular expressions
    Regex,
#if !defined(__HUGS__) || defined(HAVE_REGEX_H)
    mkRegex,
    mkRegexWithOpts,
    matchRegex,
    matchRegexAll
#endif
  ) where

import Prelude
import qualified Text.Regex.Posix as RE
import Text.Regex.Posix ( Regex )
import System.IO.Unsafe

#if !defined(__HUGS__) || defined(HAVE_REGEX_H)
-- | Makes a regular expression with the default options (multi-line,
-- case-sensitive).  The syntax of regular expressions is
-- otherwise that of @egrep@ (i.e. POSIX \"extended\" regular
-- expressions).
mkRegex :: String -> Regex
mkRegex s = unsafePerformIO (RE.regcomp s RE.regExtended)

-- | Makes a regular expression, where the multi-line and
-- case-sensitve options can be changed from the default settings.
mkRegexWithOpts
   :: String  -- ^ The regular expression to compile
   -> Bool    -- ^ 'True' @\<=>@ @\'^\'@ and @\'$\'@ match the beginning and 
	      -- end of individual lines respectively, and @\'.\'@ does /not/
	      -- match the newline character.
   -> Bool    -- ^ 'True' @\<=>@ matching is case-sensitive
   -> Regex   -- ^ Returns: the compiled regular expression

mkRegexWithOpts s single_line case_sensitive
   = unsafePerformIO (RE.regcomp s (RE.regExtended + newline + igcase))
   where
	newline | single_line = RE.regNewline
		| otherwise   = 0

	igcase  | case_sensitive = 0 
		| otherwise 	 = RE.regIgnoreCase

-- | Match a regular expression against a string
matchRegex
   :: Regex	-- ^ The regular expression
   -> String	-- ^ The string to match against
   -> Maybe [String]	-- ^ Returns: @'Just' strs@ if the match succeeded
			-- (and @strs@ is the list of subexpression matches),
			-- or 'Nothing' otherwise.
matchRegex p str = 
  case (unsafePerformIO (RE.regexec p str)) of
	Nothing -> Nothing
	Just (before, match, after, sub_strs) -> Just sub_strs

-- | Match a regular expression against a string, returning more information
-- about the match.
matchRegexAll
   :: Regex	-- ^ The regular expression
   -> String	-- ^ The string to match against
   -> Maybe ( String, String, String, [String] )
		-- ^ Returns: 'Nothing' if the match failed, or:
		-- 
		-- >  Just ( everything before match,
		-- >         portion matched,
		-- >         everything after the match,
		-- >         subexpression matches )

matchRegexAll p str = unsafePerformIO (RE.regexec p str)

#endif
