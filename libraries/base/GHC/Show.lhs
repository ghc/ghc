\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Show
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Show' class, and related operations.
--
-----------------------------------------------------------------------------

module GHC.Show
	(
	Show(..), ShowS,

	-- Instances for Show: (), [], Bool, Ordering, Int, Char

	-- Show support code
	shows, showChar, showString, showParen, showList__, showSpace,
	showLitChar, protectEsc, 
	intToDigit, showSignedInt,
	appPrec, appPrec1,

	-- Character operations
	asciiTab,
  ) 
	where

import {-# SOURCE #-} GHC.Err ( error )
import GHC.Base
import GHC.Enum
import Data.Maybe
import Data.Either
import GHC.List	( (!!),
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

appPrec, appPrec1 :: Int
	-- Use unboxed stuff because we don't have overloaded numerics yet
appPrec = I# 10#	-- Precedence of application:
			--   one more than the maximum operator precedence of 9
appPrec1 = I# 11#	-- appPrec + 1
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
    showsPrec p (Just x) s
                          = (showParen (p > appPrec) $ 
    			     showString "Just " . 
			     showsPrec appPrec1 x) s

instance (Show a, Show b) => Show (Either a b) where
    showsPrec p e s =
       (showParen (p > appPrec) $
        case e of
         Left  a -> showString "Left "  . showsPrec appPrec1 a
	 Right b -> showString "Right " . showsPrec appPrec1 b)
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
-- | Convert a character to a string using only printable characters,
-- using Haskell source-language escape conventions.  For example:
--
-- > showLitChar '\n' s  =  "\\n" ++ s
--
showLitChar 		   :: Char -> ShowS
showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDec (shows (ord c)) s)
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

isDec c = c >= '0' && c <= '9'

protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s


asciiTab :: [String]
asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI", 
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", 
	    "SP"] 
\end{code}

Code specific for Ints.

\begin{code}
-- | Convert an 'Int' in the range @0@..@15@ to the corresponding single
-- digit 'Char'.  This function fails on other inputs, and generates
-- lower-case hexadecimal digits.
intToDigit :: Int -> Char
intToDigit (I# i)
    | i >=# 0#  && i <=#  9# =  unsafeChr (ord '0' `plusInt` I# i)
    | i >=# 10# && i <=# 15# =  unsafeChr (ord 'a' `minusInt` ten `plusInt` I# i)
    | otherwise		  =  error ("Char.intToDigit: not a digit " ++ show (I# i))

ten = I# 10#

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
        | otherwise = case chr# (ord# '0'# +# (n# `remInt#` 10#)) of { c# ->
		      itos' (n# `quotInt#` 10#) (C# c# : cs) }
\end{code}
