% ------------------------------------------------------------------------------
% $Id: Show.lhs,v 1.1 2001/06/28 14:15:03 simonmar Exp $
%
% (c) The University of Glasgow, 1992-2000
%

\section{Module @GHC.Show@}


\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module GHC.Show
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

import {-# SOURCE #-} GHC.Err ( error )
import GHC.Base
import GHC.Tup
import GHC.Maybe
import GHC.List	( (!!), break, dropWhile
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
    showList ls   s = showList__ shows ls s

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
		 where showl ""       s = showChar '"' s
		       showl ('"':xs) s = showString "\\\"" (showl xs s)
		       showl (x:xs)   s = showLitChar x (showl xs s)
		-- Making 's' an explicit parameter makes it clear to GHC
		-- that showl has arity 2, which avoids it allocating an extra lambda
		-- The sticking point is the recursive call to (showl xs), which
		-- it can't figure out would be ok with arity 2.

instance Show Int where
    showsPrec = showSignedInt

instance Show a => Show (Maybe a) where
    showsPrec _p Nothing s = showString "Nothing" s
    showsPrec (I# p#) (Just x) s
                          = (showParen (p# >=# 10#) $ 
    			     showString "Just " . 
			     showsPrec (I# 10#) x) s

instance (Show a, Show b) => Show (Either a b) where
    showsPrec (I# p#) e s =
       (showParen (p# >=# 10#) $
        case e of
         Left  a -> showString "Left "  . showsPrec (I# 10#) a
	 Right b -> showString "Right " . showsPrec (I# 10#) b)
       s

\end{code}


%*********************************************************
%*							*
\subsection{Show instances for the first few tuples
%*							*
%*********************************************************

\begin{code}
-- The explicit 's' parameters are important
-- Otherwise GHC thinks that "shows x" might take a lot of work to compute
-- and generates defns like
--	showsPrec _ (x,y) = let sx = shows x; sy = shows y in
--			    \s -> showChar '(' (sx (showChar ',' (sy (showChar ')' s))))

instance  (Show a, Show b) => Show (a,b)  where
    showsPrec _ (x,y) s = (showChar '(' . shows x . showChar ',' .
                                          shows y . showChar ')') 
			  s

instance (Show a, Show b, Show c) => Show (a, b, c) where
    showsPrec _ (x,y,z) s = (showChar '(' . shows x . showChar ',' .
					    shows y . showChar ',' .
					    shows z . showChar ')')
			    s

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showsPrec _ (w,x,y,z) s = (showChar '(' . shows w . showChar ',' .
					      shows x . showChar ',' .
					      shows y . showChar ',' .
					      shows z . showChar ')')
			      s

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showsPrec _ (v,w,x,y,z) s = (showChar '(' . shows v . showChar ',' .
					     	shows w . showChar ',' .
					     	shows x . showChar ',' .
					     	shows y . showChar ',' .
					     	shows z . showChar ')') 
				s
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
showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDigit (shows (ord c)) s)
showLitChar '\DEL'	   s =  showString "\\DEL" s
showLitChar '\\'	   s =  showString "\\\\" s
showLitChar c s | c >= ' '   =  showChar c s
showLitChar '\a'	   s =  showString "\\a" s
showLitChar '\b'	   s =  showString "\\b" s
showLitChar '\f'	   s =  showString "\\f" s
showLitChar '\n'	   s =  showString "\\n" s
showLitChar '\r'	   s =  showString "\\r" s
showLitChar '\t'	   s =  showString "\\t" s
showLitChar '\v'	   s =  showString "\\v" s
showLitChar '\SO'	   s =  protectEsc (== 'H') (showString "\\SO") s
showLitChar c		   s =  showString ('\\' : asciiTab!!ord c) s
	-- I've done manual eta-expansion here, becuase otherwise it's
	-- impossible to stop (asciiTab!!ord) getting floated out as an MFE

protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s

intToDigit :: Int -> Char
intToDigit (I# i)
    | i >=# 0#  && i <=#  9# =  unsafeChr (ord '0' `plusInt` I# i)
    | i >=# 10# && i <=# 15# =  unsafeChr (ord 'a' `minusInt` I# 10# `plusInt` I# i)
    | otherwise		  =  error ("Char.intToDigit: not a digit " ++ show (I# i))

\end{code}

Code specific for Ints.

\begin{code}
showSignedInt :: Int -> Int -> ShowS
showSignedInt (I# p) (I# n) r
    | n <# 0# && p ># 6# = '(' : itos n (')' : r)
    | otherwise          = itos n r

itos :: Int# -> String -> String
itos n# cs
    | n# <# 0# = let
        n'# = negateInt# n#
        in if n'# <# 0# -- minInt?
            then '-' : itos' (negateInt# (n'# `quotInt#` 10#))
                             (itos' (negateInt# (n'# `remInt#` 10#)) cs)
            else '-' : itos' n'# cs
    | otherwise = itos' n# cs
    where
    itos' :: Int# -> String -> String
    itos' n# cs
        | n# <# 10#  = C# (chr# (ord# '0'# +# n#)) : cs
        | otherwise = itos' (n# `quotInt#` 10#)
                            (C# (chr# (ord# '0'# +# (n# `remInt#` 10#))) : cs)
\end{code}

%*********************************************************
%*							*
\subsection{Character stuff}
%*							*
%*********************************************************

\begin{code}
isAscii, isLatin1, isControl, isPrint, isSpace, isUpper,
 isLower, isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
 isAsciiUpper, isAsciiLower :: Char -> Bool
isAscii c	 	=  c <  '\x80'
isLatin1 c              =  c <= '\xff'
isControl c		=  c < ' ' || c >= '\DEL' && c <= '\x9f'
isPrint c		=  not (isControl c)

-- isSpace includes non-breaking space
-- Done with explicit equalities both for efficiency, and to avoid a tiresome
-- recursion with GHC.List elem
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
