\section[match]{PackedString functions for matching}

This module provides regular expression matching and substitution
at the PackedString level. It is built on top of the GNU Regex
library modified to handle perl regular expression syntax.
For a complete description of the perl syntax, do `man perlre`
or have a gander in (Programming|Learning) Perl. Here's
a short summary:

^     matches the beginning of line
$     matches end of line
\b    matches word boundary
\B    matches non-word boundary
\w    matches a word(alpha-numeric) character
\W    matches a non-word character
\d    matches a digit
\D    matches a non-digit
\s    matches whitespace
\S    matches non-whitespace
\A    matches beginning of buffer
\Z    matches end-of-buffer
.     matches any (bar newline in single-line mode)
+     matches 1 or more times
*     matches 0 or more times
?     matches 0 or 1
{n,m} matches >=n and <=m atoms
{n,}  matches at least n times
{n}   matches n times
[..]  matches any character member of char class.
(..)  if pattern inside parens match, then the ith group is bound
      to the matched string
\digit matches whatever the ith group matched. 

Backslashed letters
\n	newline
\r	carriage return
\t	tab
\f	formfeed
\v	vertical tab
\a      alarm bell
\e      escape


\begin{code}
module MatchPS

      (
        matchPS,
	searchPS,
	substPS,
	replacePS,
	
	match2PS,
	search2PS,
	
	getMatchesNo,
	getMatchedGroup,
	getWholeMatch,
	getLastMatch,
	getAfterMatch,
	
	findPS,
	rfindPS,
	chopPS,
	
	matchPrefixPS,

	REmatch(..)
      ) where

import GlaExts
import PackedString

import Array    ((!), bounds)
import Char     ( isDigit, ord )
import PrelBase ( Char(..) )

import Regex

\end{code}

\subsection[ps-matching]{PackedString matching}

Posix matching, returning an array of the the intervals that
the individual groups matched within the string.

\begin{code}

matchPS :: PackedString		-- reg. exp
	-> PackedString 		-- string to match
	-> [Char]			-- flags
	-> Maybe REmatch
matchPS reg str flags
 = let
    insensitive = 'i' `elem` flags
    mode = 's' `elem` flags
   in
    unsafePerformIO (do
      pat <- re_compile_pattern reg mode insensitive
      re_match pat str 0 True)


match2PS :: PackedString		-- reg. exp
	 -> PackedString 		-- string1 to match
	 -> PackedString 		-- string2 to match
	 -> [Char]			-- flags
	 -> Maybe REmatch
match2PS reg str1 str2 flags
 = let
    insensitive = 'i' `elem` flags
    mode = 's' `elem` flags
    len1 = lengthPS str1
    len2 = lengthPS str2
   in
    unsafePerformIO (do
      pat <- re_compile_pattern reg mode insensitive
      re_match2 pat str1 str2 0 (len1+len2) True)

\end{code}

PackedString front-end to searching with GNU Regex

\begin{code}

searchPS :: PackedString		-- reg. exp
	 -> PackedString 		-- string to match
	 -> [Char]			-- flags
	 -> Maybe REmatch
searchPS reg str flags
 = let
    insensitive = 'i' `elem` flags
    mode = 's' `elem` flags
   in
    unsafePerformIO (do
      pat <- re_compile_pattern reg mode insensitive
      re_search pat str 
		    0 
		    (lengthPS str)
		    True)


      
search2PS :: PackedString		-- reg. exp
	  -> PackedString 		-- string to match
	  -> PackedString 		-- string to match
	  -> [Char]			-- flags
	  -> Maybe REmatch
search2PS reg str1 str2 flags
 = let
    insensitive = 'i' `elem` flags
    mode = 's' `elem` flags
    len1 = lengthPS str1
    len2 = lengthPS str2
    len  = len1+len2
   in
    unsafePerformIO (do
      pat <- re_compile_pattern reg mode insensitive
      re_search2 pat 
                 str1
                 str2
		 0 
		 len
		 len
		 True)


      
\end{code}

@substrPS s st end@ cuts out the chunk in \tr{s} between \tr{st} and \tr{end}, inclusive.
The \tr{Regex} registers represent substrings by storing the start and the end point plus
one( st==end => empty string) , so we use @chunkPS@ instead.


\begin{code}

chunkPS :: PackedString
	 -> (Int,Int)
	 -> PackedString
chunkPS str (st,end)
 = if st==end then
      nilPS
   else
      substrPS str st (max 0 (end-1))

\end{code}

Perl-like match and substitute

\begin{code}

substPS :: PackedString   -- reg. exp
	-> PackedString   -- replacement
	-> [Char]	   -- flags
	-> PackedString   -- string
	-> PackedString
substPS rexp
	repl
	flags
	str
 = search str 
   where
    global = 'g' `elem` flags
    case_insensitive = 'i' `elem` flags
    mode = 's' `elem` flags	-- single-line mode
    pat  = unsafePerformIO (
              re_compile_pattern rexp mode case_insensitive)

    search str 
     = let
	search_res
         = unsafePerformIO (re_search pat str 0 (lengthPS str) True)
       in
        case search_res of
          Nothing  -> str
          Just matcher@(REmatch arr before match after lst) ->
	    let
	     (st,en) = match
             prefix = chunkPS str before
             suffix 
              = if global && (st /= en) then
	           search (dropPS en str)
	        else
	           chunkPS str after
	    in	
	     concatPS [prefix,
	                replace matcher repl str,
		        suffix]


replace :: REmatch
	-> PackedString
        -> PackedString
        -> PackedString
replace (REmatch arr before@(_,b_end) match after lst)
	replacement
        str
 = concatPS (reverse acc) -- ToDo: write a `reversed' version of concatPS
   where
    (_,b) = bounds arr

    acc = replace' [] replacement False

    single :: Char -> PackedString
    single x = consPS x nilPS

    replace' :: [PackedString] 
             -> PackedString 
	     -> Bool 
	     -> [PackedString]
    replace' acc repl escaped
     = if (nullPS repl) then
         acc
       else
         let
          x  = headPS repl
	  x# = case x of { C# c -> c }
          xs = tailPS repl
         in
          case x# of
            '\\'# ->  
               if escaped then
                  replace' acc xs True
               else
                  replace' ((single x):acc) xs (not escaped)
            '$'#  ->
              if (not escaped) then
	       let
	        x'           = headPS xs
	        xs'          = tailPS xs
	        ith_ival     = arr!num
                (num,xs_num) = getNumber ((ord x') - ord '0') xs'
	       in
	        if (isDigit x') && (num<=b) then
		  replace' ((chunkPS str ith_ival):acc) xs_num escaped
	        else if x' == '&' then
		  replace' ((chunkPS str match):acc) xs' escaped
	        else if x' == '+' then
		  replace' ((chunkPS str lst):acc) xs' escaped
	        else if x' == '`' then
		  replace' ((chunkPS str (0,b_end)):acc) xs' escaped
	        else if x' == '\'' then
		  replace' ((chunkPS str after):acc) xs' escaped
	        else -- ignore
		  replace' acc xs escaped
              else
	        replace' ((single x):acc) xs False

	    _ -> if escaped then
		   (case x# of
		     'n'# ->   -- newline
                         replace' ((single '\n'):acc)
		     'f'# ->   -- formfeed
                         replace' ((single '\f'):acc)
		     'r'# ->   -- carriage return
                         replace' ((single '\r'):acc)
		     't'# ->   -- (horiz) tab
                         replace' ((single '\t'):acc)
		     'v'# ->   -- vertical tab
                         replace' ((single '\v'):acc)
		     'a'# ->   -- alarm bell
                         replace' ((single '\a'):acc)
		     'e'# ->   -- escape
                         replace' ((single '\033'):acc)
		     _    ->
                         replace' ((single x):acc))    xs False
		 else
		   replace' ((single x):acc) xs False


getNumber :: Int -> PackedString -> (Int,PackedString)
getNumber acc ps
 = if nullPS ps then
      (acc,ps)
   else
     let
      x = headPS  ps
      xs = tailPS ps
     in
      if (isDigit x) then
	 getNumber (acc*10+(ord x - ord '0')) xs
      else
         (acc,ps)

\end{code}

Just like substPS, but no prefix and suffix.

\begin{code}

replacePS :: PackedString   -- reg. exp
	  -> PackedString   -- replacement
	  -> [Char]	   -- flags
	  -> PackedString   -- string
	  -> PackedString
replacePS rexp
	  repl
	  flags
	  str
 = search str 
   where
    global = 'g' `elem` flags
    case_insensitive = 'i' `elem` flags
    mode = 's' `elem` flags	-- single-line mode
    pat  = unsafePerformIO (
              re_compile_pattern rexp mode case_insensitive)

    search str 
     = let
	search_res
         = unsafePerformIO (re_search pat str 0 (lengthPS str) True)
       in
        case search_res of
          Nothing  -> str
          Just matcher@(REmatch arr before match after lst) ->
	     replace matcher repl str

\end{code}

Picking matched groups out of string

\begin{code}

getMatchesNo :: REmatch
	     -> Int
getMatchesNo (REmatch arr _ _ _ _)
 = snd (bounds arr)

getMatchedGroup :: REmatch 
	        -> Int 
	        -> PackedString 
	        -> PackedString
getMatchedGroup (REmatch arr bef mtch after lst) nth str
 = let
    (1,grps) = bounds arr
   in
    if (nth >= 1) && (nth <= grps) then
       chunkPS str (arr!nth)
    else
       error "getMatchedGroup: group out of range"

getWholeMatch :: REmatch 
	      -> PackedString 
	      -> PackedString
getWholeMatch (REmatch _ _  mtch _ _) str
 = chunkPS str mtch

getLastMatch :: REmatch 
	      -> PackedString 
	      -> PackedString
getLastMatch (REmatch _ _ _ _ lst) str
 = chunkPS str lst

getAfterMatch :: REmatch 
	      -> PackedString 
	      -> PackedString
getAfterMatch (REmatch _ _ _ aft _) str
 = chunkPS str aft

\end{code}


More or less straight translation of a brute-force string matching
function written in C. (Sedgewick ch. 18)

This is intended to provide much the same facilities as index/rindex in perl.

\begin{code}


findPS :: PackedString
       -> PackedString
       -> Maybe Int
findPS str substr
 = let
    m = lengthPS substr
    n = lengthPS str

    loop i j
     | j>=m || i>=n = if j==m then (Just (i-m)) else Nothing
     | otherwise  
	= inner_loop i j

    inner_loop i j
     = if j<m && i<n && (indexPS str i /= indexPS substr j) then
	  inner_loop (i-j+1) 0
       else
          loop (i+1) (j+1)
   in
    loop 0 0
      
rfindPS :: PackedString
        -> PackedString
        -> Maybe Int
rfindPS str substr
 = let
    m = lengthPS substr - 1
    n = lengthPS str - 1

    loop i j
     | j<0 || i<0 = if j<0 then (Just (i+1)) else Nothing
     | otherwise  
	= inner_loop i j

    inner_loop i j
     = if j>=0 && i>=0 && (indexPS str i /= indexPS substr j) then
	  inner_loop (i+(m-j)-1) m
       else
          loop (i-1) (j-1)
   in
    loop n m
      
	
\end{code}

\begin{code}

chopPS :: PackedString -> PackedString
chopPS str = if nullPS str then
		nilPS
	     else
		chunkPS  str (0,lengthPS str-1)

\end{code}

Tries to match as much as possible of strA starting from the beginning of strB
(handy when matching fancy literals in parsers)

\begin{code}
matchPrefixPS :: PackedString
	      -> PackedString
	      -> Int
matchPrefixPS pref str
 = matchPrefixPS' pref str 0
   where
    matchPrefixPS' pref str n
     = if (nullPS pref) || (nullPS str) then
	  n
       else if (headPS pref) == (headPS str) then
	  matchPrefixPS' (tailPS pref) (tailPS str) (n+1)
       else
	  n 

\end{code}
