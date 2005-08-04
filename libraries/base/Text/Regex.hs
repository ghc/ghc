-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Regular expression matching.  Uses the POSIX regular expression
-- interface in "Text.Regex.Posix".
--
-----------------------------------------------------------------------------
module Text.Regex (
    -- * Regular expressions
    Regex,
    mkRegex,
    mkRegexWithOpts,
    matchRegex,
    matchRegexAll,
    subRegex,
    splitRegex
  ) where

import Prelude
import qualified Text.Regex.Posix as RE
import Text.Regex.Posix ( Regex )
import System.IO.Unsafe

-- | Makes a regular expression with the default options (multi-line,
-- case-sensitive).  The syntax of regular expressions is
-- otherwise that of @egrep@ (i.e. POSIX \"extended\" regular
-- expressions).
mkRegex :: String -> Regex
mkRegex s = unsafePerformIO (RE.regcomp s RE.regExtended)

-- | Makes a regular expression, where the multi-line and
-- case-sensitive options can be changed from the default settings.
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

{- | Replaces every occurance of the given regexp with the replacement string.

In the replacement string, @\"\\1\"@ refers to the first substring;
@\"\\2\"@ to the second, etc; and @\"\\0\"@ to the entire match.
@\"\\\\\\\\\"@ will insert a literal backslash.

-}
subRegex :: Regex                          -- ^ Search pattern
      -> String                         -- ^ Input string
      -> String                         -- ^ Replacement text
      -> String                         -- ^ Output string
subRegex _ "" _ = ""
subRegex regexp inp repl =
    let bre = mkRegex "\\\\(\\\\|[0-9]+)"
        lookup _ [] _ = []
        lookup [] _ _ = []
        lookup match repl groups =
            case matchRegexAll bre repl of
                Nothing -> repl
                Just (lead, _, trail, bgroups) ->
                    let newval = if (head bgroups) == "\\"
                                 then "\\"
                                 else let index = (read (head bgroups)) - 1
                                          in
                                          if index == -1
                                             then match
                                             else groups !! index
                        in
                        lead ++ newval ++ lookup match trail groups
        in
        case matchRegexAll regexp inp of
            Nothing -> inp
            Just (lead, match, trail, groups) ->
              lead ++ lookup match repl groups ++ (subRegex regexp trail repl)

{- | Splits a string based on a regular expression.  The regular expression
should identify one delimiter.
-}

splitRegex :: Regex -> String -> [String]
splitRegex _ [] = []
splitRegex delim str =
    case matchRegexAll delim str of
       Nothing -> [str]
       Just (firstline, _, remainder, _) ->
           if remainder == ""
              then firstline : [] : []
              else firstline : splitRegex delim remainder
