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

import PreludeGlaST

import Regex

import Core	-- alas ...

\end{code}

_tailPS and _dropPS in PS.lhs are not to my liking, use
these instead. 

\begin{code}

_dropPS' x str = _substrPS str x (_lengthPS str)

_tailPS' x
 = if _nullPS x then
     error "_tailPS []"
   else
     _substrPS x 1 (_lengthPS x)


\end{code}

\subsection[ps-matching]{PackedString matching}

Posix matching, returning an array of the the intervals that
the individual groups matched within the string.

\begin{code}

matchPS :: _PackedString		-- reg. exp
	-> _PackedString 		-- string to match
	-> [Char]			-- flags
	-> Maybe REmatch
matchPS reg str flags
 = let
    insensitive = 'i' `elem` flags
    mode = 's' `elem` flags
   in
    unsafePerformPrimIO (
      re_compile_pattern reg mode insensitive 	`thenPrimIO` \ pat ->
      re_match pat str 0 True)


match2PS :: _PackedString		-- reg. exp
	 -> _PackedString 		-- string1 to match
	 -> _PackedString 		-- string2 to match
	 -> [Char]			-- flags
	 -> Maybe REmatch
match2PS reg str1 str2 flags
 = let
    insensitive = 'i' `elem` flags
    mode = 's' `elem` flags
    len1 = _lengthPS str1
    len2 = _lengthPS str2
   in
    unsafePerformPrimIO (
      re_compile_pattern reg mode insensitive 	`thenPrimIO` \ pat ->
      re_match2 pat str1 str2 0 (len1+len2) True)

\end{code}

PackedString front-end to searching with GNU Regex

\begin{code}

searchPS :: _PackedString		-- reg. exp
	 -> _PackedString 		-- string to match
	 -> [Char]			-- flags
	 -> Maybe REmatch
searchPS reg str flags
 = let
    insensitive = 'i' `elem` flags
    mode = 's' `elem` flags
   in
    unsafePerformPrimIO (
      re_compile_pattern reg mode insensitive	`thenPrimIO` \ pat ->
      re_search pat str 
		    0 
		    (_lengthPS str)
		    True)


      
search2PS :: _PackedString		-- reg. exp
	  -> _PackedString 		-- string to match
	  -> _PackedString 		-- string to match
	  -> [Char]			-- flags
	  -> Maybe REmatch
search2PS reg str1 str2 flags
 = let
    insensitive = 'i' `elem` flags
    mode = 's' `elem` flags
    len1 = _lengthPS str1
    len2 = _lengthPS str2
    len  = len1+len2
   in
    unsafePerformPrimIO (
      re_compile_pattern reg mode insensitive	`thenPrimIO` \ pat ->
      re_search2 pat 
                 str1
                 str2
		 0 
		 len
		 len
		 True)


      
\end{code}

@_substrPS s st end@ cuts out the chunk in \tr{s} between \tr{st} and \tr{end}, inclusive.
The \tr{Regex} registers represent substrings by storing the start and the end point plus
one( st==end => empty string) , so we use @chunkPS@ instead.


\begin{code}

_chunkPS :: _PackedString
	 -> (Int,Int)
	 -> _PackedString
_chunkPS str (st,end)
 = if st==end then
      _nilPS
   else
      _substrPS str st (max 0 (end-1))

\end{code}

Perl-like match and substitute

\begin{code}

substPS :: _PackedString   -- reg. exp
	-> _PackedString   -- replacement
	-> [Char]	   -- flags
	-> _PackedString   -- string
	-> _PackedString
substPS rexp
	repl
	flags
	str
 = search str 
   where
    global = 'g' `elem` flags
    case_insensitive = 'i' `elem` flags
    mode = 's' `elem` flags	-- single-line mode
    pat  = unsafePerformPrimIO (
              re_compile_pattern rexp mode case_insensitive)

    search str 
     = let
	search_res
         = unsafePerformPrimIO (re_search pat str 0 (_lengthPS str) True)
       in
        case search_res of
          Nothing  -> str
          Just matcher@(REmatch arr before match after lst) ->
	    let
	     (st,en) = match
             prefix = _chunkPS str before
             suffix 
              = if global && (st /= en) then
	           search (_dropPS' en str)
	        else
	           _chunkPS str after
	    in	
	     _concatPS [prefix,
	                replace matcher repl str,
		        suffix]


replace :: REmatch
	-> _PackedString
        -> _PackedString
        -> _PackedString
replace (REmatch arr before@(_,b_end) match after lst)
	replacement
        str
 = _concatPS (reverse acc) -- ToDo: write a `reversed' version of concatPS
   where
    (_,b) = bounds arr

    acc = replace' [] replacement False

    single :: Char -> _PackedString
    single x = _consPS x _nilPS

    replace' :: [_PackedString] 
             -> _PackedString 
	     -> Bool 
	     -> [_PackedString]
    replace' acc repl escaped
     = if (_nullPS repl) then
         acc
       else
         let
          x  = _headPS repl
	  x# = case x of { C# c -> c }
          xs = _tailPS' repl
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
	        x'           = _headPS xs
	        xs'          = _tailPS' xs
	        ith_ival     = arr!num
                (num,xs_num) = getNumber ((ord x') - ord '0') xs'
	       in
	        if (isDigit x') && (num<=b) then
		  replace' ((_chunkPS str ith_ival):acc) xs_num escaped
	        else if x' == '&' then
		  replace' ((_chunkPS str match):acc) xs' escaped
	        else if x' == '+' then
		  replace' ((_chunkPS str lst):acc) xs' escaped
	        else if x' == '`' then
		  replace' ((_chunkPS str (0,b_end)):acc) xs' escaped
	        else if x' == '\'' then
		  replace' ((_chunkPS str after):acc) xs' escaped
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


getNumber :: Int -> _PackedString -> (Int,_PackedString)
getNumber acc ps
 = if _nullPS ps then
      (acc,ps)
   else
     let
      x = _headPS ps
      xs = _tailPS ps
     in
      if (isDigit x) then
	 getNumber (acc*10+(ord x - ord '0')) xs
      else
         (acc,ps)

\end{code}

Just like substPS, but no prefix and suffix.

\begin{code}

replacePS :: _PackedString   -- reg. exp
	  -> _PackedString   -- replacement
	  -> [Char]	   -- flags
	  -> _PackedString   -- string
	  -> _PackedString
replacePS rexp
	  repl
	  flags
	  str
 = search str 
   where
    global = 'g' `elem` flags
    case_insensitive = 'i' `elem` flags
    mode = 's' `elem` flags	-- single-line mode
    pat  = unsafePerformPrimIO (
              re_compile_pattern rexp mode case_insensitive)

    search str 
     = let
	search_res
         = unsafePerformPrimIO (re_search pat str 0 (_lengthPS str) True)
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
	        -> _PackedString 
	        -> _PackedString
getMatchedGroup (REmatch arr bef mtch after lst) nth str
 = let
    (1,grps) = bounds arr
   in
    if (nth >= 1) && (nth <= grps) then
       _chunkPS str (arr!nth)
    else
       error "getMatchedGroup: group out of range"

getWholeMatch :: REmatch 
	      -> _PackedString 
	      -> _PackedString
getWholeMatch (REmatch _ _  mtch _ _) str
 = _chunkPS str mtch

getLastMatch :: REmatch 
	      -> _PackedString 
	      -> _PackedString
getLastMatch (REmatch _ _ _ _ lst) str
 = _chunkPS str lst

getAfterMatch :: REmatch 
	      -> _PackedString 
	      -> _PackedString
getAfterMatch (REmatch _ _ _ aft _) str
 = _chunkPS str aft

\end{code}


More or less straight translation of a brute-force string matching
function written in C. (Sedgewick ch. 18)

This is intended to provide much the same facilities as index/rindex in perl.

\begin{code}


findPS :: _PackedString
       -> _PackedString
       -> Maybe Int
findPS str substr
 = let
    m = _lengthPS substr
    n = _lengthPS str

    loop i j
     | j>=m || i>=n = if j==m then (Just (i-m)) else Nothing
     | otherwise  
	= inner_loop i j

    inner_loop i j
     = if j<m && i<n && (_indexPS str i /= _indexPS substr j) then
	  inner_loop (i-j+1) 0
       else
          loop (i+1) (j+1)
   in
    loop 0 0
      
rfindPS :: _PackedString
        -> _PackedString
        -> Maybe Int
rfindPS str substr
 = let
    m = _lengthPS substr - 1
    n = _lengthPS str - 1

    loop i j
     | j<0 || i<0 = if j<0 then (Just (i+1)) else Nothing
     | otherwise  
	= inner_loop i j

    inner_loop i j
     = if j>=0 && i>=0 && (_indexPS str i /= _indexPS substr j) then
	  inner_loop (i+(m-j)-1) m
       else
          loop (i-1) (j-1)
   in
    loop n m
      
	
\end{code}

\begin{code}

chopPS :: _PackedString -> _PackedString
chopPS str = if _nullPS str then
		_nilPS
	     else
		_chunkPS  str (0,_lengthPS str-1)

\end{code}

Tries to match as much as possible of strA starting from the beginning of strB
(handy when matching fancy literals in parsers)

\begin{code}
matchPrefixPS :: _PackedString
	      -> _PackedString
	      -> Int
matchPrefixPS pref str
 = matchPrefixPS' pref str 0
   where
    matchPrefixPS' pref str n
     = if (_nullPS pref) || (_nullPS str) then
	  n
       else if (_headPS pref) == (_headPS str) then
	  matchPrefixPS' (_tailPS pref) (_tailPS str) (n+1)
       else
	  n 

\end{code}
