-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (only on platforms that provide a regex lib)
--
-- Regular expression matching.
-- Uses the POSIX regular expression interface in Text.Regex.Posix for now.
--
-----------------------------------------------------------------------------

module Text.Regex (
    Regex,
    mkRegex,
    mkRegexWithOpts,
    matchRegex,
    matchRegexAll
  ) where

import Prelude
import qualified Text.Regex.Posix as RE
import System.IO.Unsafe

type Regex = RE.Regex

mkRegex :: String -> Regex
mkRegex s = unsafePerformIO (RE.regcomp s RE.regExtended)

mkRegexWithOpts :: String -> Bool -> Bool -> Regex
mkRegexWithOpts s single_line case_sensitive
   = unsafePerformIO (RE.regcomp s (RE.regExtended + newline + igcase))
   where
	newline | single_line = 0
		| otherwise   = RE.regNewline

	igcase  | case_sensitive = 0 
		| otherwise 	 = RE.regIgnoreCase

matchRegex :: Regex -> String -> Maybe [String]
matchRegex p str = 
  case (unsafePerformIO (RE.regexec p str)) of
	Nothing -> Nothing
	Just (before, match, after, sub_strs) -> Just sub_strs

matchRegexAll :: Regex -> String ->
        Maybe ( String,  -- \$`
                String,  -- \$&
                String,  -- \$'
                [String] -- \$1..
              )
matchRegexAll p str = unsafePerformIO (RE.regexec p str)

