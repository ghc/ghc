%
% (c) The GRAP/AQUA Project, Glasgow University, 1992-1996
%
\section{Module @PrelShow@}


\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelShow
	(
	Show(..), ShowS,

	-- Instances for Show: (), [], Bool, Ordering, Int, Char

	-- Show support code
	shows, showChar, showString, showParen, showList__, showSpace,
	showLitChar, protectEsc, 
	intToDigit, showSignedInt,

	-- Character operations
	isAscii, isLatin1, isControl, isPrint, isSpace, isUpper,
	isLower, isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
	toUpper, toLower,
	asciiTab,

	-- String operations
	lines, unlines, words, unwords
  ) 
	where

import {-# SOURCE #-} PrelErr ( error )
import PrelBase
import PrelMaybe
import PrelList	( (!!), break, dropWhile
#ifdef USE_REPORT_PRELUDE
                , concatMap, foldr1
#endif
                )
\end{code}



%*********************************************************
%*							*
\subsection{The @Show@ class}
%*							*
%*********************************************************

\begin{code}
type ShowS = String -> String

class  Show a  where
    showsPrec :: Int -> a -> ShowS
    show      :: a   -> String
    showList  :: [a] -> ShowS

    showsPrec _ x s = show x ++ s
    show x          = shows x ""
    showList ls     = showList__ shows ls 

showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)
\end{code}

%*********************************************************
%*							*
\subsection{Simple Instances}
%*							*
%*********************************************************

\begin{code}
 
instance  Show ()  where
    showsPrec _ () = showString "()"

instance Show a => Show [a]  where
    showsPrec _         = showList

instance Show Bool where
  showsPrec _ True  = showString "True"
  showsPrec _ False = showString "False"

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

instance  Show Char  where
    showsPrec _ '\'' = showString "'\\''"
    showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
		 where showl ""       = showChar '"'
		       showl ('"':xs) = showString "\\\"" . showl xs
		       showl (x:xs)   = showLitChar x . showl xs

instance  Show Int  where
    showsPrec p n = showSignedInt p n

instance Show a => Show (Maybe a) where
    showsPrec _p Nothing  = showString "Nothing"
    showsPrec _p (Just x) = showString "Just " . shows x
	-- Not sure I have the priorities right here

instance (Show a, Show b) => Show (Either a b) where
    showsPrec _p (Left a)  = showString "Left "  . shows a
    showsPrec _p (Right b) = showString "Right " . shows b
	-- Not sure I have the priorities right here
\end{code}


%*********************************************************
%*							*
\subsection{Show instances for the first few tuples
%*							*
%*********************************************************

\begin{code}
instance  (Show a, Show b) => Show (a,b)  where
    showsPrec _ (x,y) = showChar '(' . shows x . showChar ',' .
                                       shows y . showChar ')'

instance (Show a, Show b, Show c) => Show (a, b, c) where
    showsPrec _ (x,y,z) = showChar '(' . shows x . showChar ',' .
					 shows y . showChar ',' .
					 shows z . showChar ')'

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showsPrec _ (w,x,y,z) = showChar '(' . shows w . showChar ',' .
					   shows x . showChar ',' .
					   shows y . showChar ',' .
					   shows z . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showsPrec _ (v,w,x,y,z) = showChar '(' . shows v . showChar ',' .
					     shows w . showChar ',' .
					     shows x . showChar ',' .
					     shows y . showChar ',' .
					     shows z . showChar ')'
\end{code}


%*********************************************************
%*							*
\subsection{Support code for @Show@}
%*							*
%*********************************************************

\begin{code}
shows           :: (Show a) => a -> ShowS
shows           =  showsPrec zeroInt

showChar        :: Char -> ShowS
showChar        =  (:)

showString      :: String -> ShowS
showString      =  (++)

showParen       :: Bool -> ShowS -> ShowS
showParen b p   =  if b then showChar '(' . p . showChar ')' else p

showSpace :: ShowS
showSpace = {-showChar ' '-} \ xs -> ' ' : xs
\end{code}

Code specific for characters

\begin{code}
showLitChar 		   :: Char -> ShowS
showLitChar c | c > '\DEL' =  showChar '\\' . protectEsc isDigit (shows (ord c))
showLitChar '\DEL'	   =  showString "\\DEL"
showLitChar '\\'	   =  showString "\\\\"
showLitChar c | c >= ' '   =  showChar c
showLitChar '\a'	   =  showString "\\a"
showLitChar '\b'	   =  showString "\\b"
showLitChar '\f'	   =  showString "\\f"
showLitChar '\n'	   =  showString "\\n"
showLitChar '\r'	   =  showString "\\r"
showLitChar '\t'	   =  showString "\\t"
showLitChar '\v'	   =  showString "\\v"
showLitChar '\SO'	   =  protectEsc (== 'H') (showString "\\SO")
showLitChar c		   =  showString ('\\' : asciiTab!!ord c)

protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s

intToDigit :: Int -> Char
intToDigit (I# i)
 | i >=# 0#  && i <=#  9# =  unsafeChr (ord '0' `plusInt` I# i)
 | i >=# 10# && i <=# 15# =  unsafeChr (ord 'a' `plusInt` I# i `minusInt` I# 10#)
 | otherwise		  =  error ("Char.intToDigit: not a digit " ++ show (I# i))

\end{code}

Code specific for Ints.

\begin{code}
showSignedInt :: Int -> Int -> ShowS
showSignedInt (I# p) (I# n) r
  | n <# 0# && p ># 6# = '(':itos n (')':r)
  | otherwise	       = itos n r

itos :: Int# -> String -> String
itos n r
  | n >=# 0#		= itos' n r
  | negateInt# n <# 0#  = -- n is minInt, a difficult number
	    itos (n `quotInt#` 10#) (itos' (negateInt# (n `remInt#` 10#)) r)
  | otherwise = '-':itos' (negateInt# n) r
 where
   itos' :: Int# -> String -> String
	-- x >= 0
   itos' x cs 
     | x <# 10#  = C# (chr# (x +# ord# '0'#)) : cs
     | otherwise = itos' (x `quotInt#` 10#) 
		         (C# (chr# (x `remInt#` 10# +# ord# '0'#)) : cs)
\end{code}



%*********************************************************
%*							*
\subsection{Character stuff}
%*							*
%*********************************************************

\begin{code}
isAscii, isLatin1, isControl, isPrint, isSpace, isUpper,
 isLower, isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum :: Char -> Bool
isAscii c	 	=  c <  '\x80'
isLatin1 c              =  c <= '\xff'
isControl c		=  c < ' ' || c >= '\DEL' && c <= '\x9f'
isPrint c		=  not (isControl c)

-- isSpace includes non-breaking space
-- Done with explicit equalities both for efficiency, and to avoid a tiresome
-- recursion with PrelList elem
isSpace c		=  c == ' '	||
			   c == '\t'	||
			   c == '\n'	||
			   c == '\r'	||
			   c == '\f'	||
			   c == '\v'	||
			   c == '\xa0'

-- The upper case ISO characters have the multiplication sign dumped
-- randomly in the middle of the range.  Go figure.
isUpper c		=  c >= 'A' && c <= 'Z' || 
                           c >= '\xC0' && c <= '\xD6' ||
                           c >= '\xD8' && c <= '\xDE'
-- The lower case ISO characters have the division sign dumped
-- randomly in the middle of the range.  Go figure.
isLower c		=  c >= 'a' && c <= 'z' ||
                           c >= '\xDF' && c <= '\xF6' ||
                           c >= '\xF8' && c <= '\xFF'
isAsciiLower c          =  c >= 'a' && c <= 'z'
isAsciiUpper c          =  c >= 'A' && c <= 'Z'

isAlpha c		=  isLower c || isUpper c
isDigit c		=  c >= '0' && c <= '9'
isOctDigit c		=  c >= '0' && c <= '7'
isHexDigit c		=  isDigit c || c >= 'A' && c <= 'F' ||
                                        c >= 'a' && c <= 'f'
isAlphaNum c		=  isAlpha c || isDigit c

-- Case-changing operations

toUpper, toLower	:: Char -> Char
toUpper c@(C# c#)
  | isAsciiLower c    = C# (chr# (ord# c# -# 32#))
  | isAscii c         = c
    -- fall-through to the slower stuff.
  | isLower c	&& c /= '\xDF' && c /= '\xFF'
  = unsafeChr (ord c `minusInt` ord 'a' `plusInt` ord 'A')
  | otherwise
  = c



toLower c@(C# c#)
  | isAsciiUpper c = C# (chr# (ord# c# +# 32#))
  | isAscii c      = c
  | isUpper c	   = unsafeChr (ord c `minusInt` ord 'A' `plusInt` ord 'a')
  | otherwise	   =  c

asciiTab :: [String]
asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI", 
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", 
	    "SP"] 
\end{code}

%*********************************************************
%*							*
\subsection{Functions on strings}
%*							*
%*********************************************************

lines breaks a string up into a list of strings at newline characters.
The resulting strings do not contain newlines.  Similary, words
breaks a string up into a list of words, which were delimited by
white space.  unlines and unwords are the inverse operations.
unlines joins lines with terminating newlines, and unwords joins
words with separating spaces.

\begin{code}
lines			:: String -> [String]
lines ""		=  []
lines s			=  let (l, s') = break (== '\n') s
			   in  l : case s' of
					[]     	-> []
					(_:s'') -> lines s''

words			:: String -> [String]
words s			=  case dropWhile {-partain:Char.-}isSpace s of
				"" -> []
				s' -> w : words s''
				      where (w, s'') = 
                                             break {-partain:Char.-}isSpace s'

unlines			:: [String] -> String
#ifdef USE_REPORT_PRELUDE
unlines			=  concatMap (++ "\n")
#else
-- HBC version (stolen)
-- here's a more efficient version
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls
#endif

unwords			:: [String] -> String
#ifdef USE_REPORT_PRELUDE
unwords []		=  ""
unwords ws		=  foldr1 (\w s -> w ++ ' ':s) ws
#else
-- HBC version (stolen)
-- here's a more efficient version
unwords []		=  ""
unwords [w]		= w
unwords (w:ws)		= w ++ ' ' : unwords ws
#endif

\end{code}
