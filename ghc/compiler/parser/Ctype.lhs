Character classification

\begin{code}
{-# OPTIONS -#include "hs_ctype.h" #-}

module Ctype
	( is_ident	-- Char# -> Bool
	, is_symbol	-- Char# -> Bool
	, is_any	-- Char# -> Bool
	, is_space	-- Char# -> Bool
	, is_lower	-- Char# -> Bool
	, is_upper	-- Char# -> Bool
	, is_digit	-- Char# -> Bool
	, is_string	-- Char# -> Bool
	) where
\end{code}

\begin{code}
import Bits	( Bits((.&.)) )
import Int	( Int32 )
import Addr
import Char	( ord )
import GlaExts  ( Char#, Char(..) )
\end{code}

#define NO_CDECLS
#include <hs_ctype.h>
\end{code}

The predicates below look costly, but aren't, GHC+GCC do a great job
at the big case below.

\begin{code}
{-# INLINE is_ctype #-}
is_ctype :: Int -> Char# -> Bool
is_ctype mask c = (fromIntegral (charType (C# c)) .&. fromIntegral mask) /= (0::Int32)

cIdent, cSymbol, cAny, cSpace, cLower, cUpper, cDigit, cString :: Int
cIdent  =  C_Ident  :: Int
cSymbol =  C_Symbol :: Int
cAny    =  C_Any    :: Int
cSpace  =  C_Space  :: Int
cLower  =  C_Lower  :: Int
cUpper  =  C_Upper  :: Int
cDigit  =  C_Digit  :: Int
cString =  C_String :: Int

is_ident, is_symbol, is_any, is_space, is_lower, is_upper, is_digit, is_string :: Char# -> Bool
is_ident  = is_ctype cIdent
is_symbol = is_ctype cSymbol
is_any    = is_ctype cAny
is_space  = is_ctype cSpace
is_lower  = is_ctype cLower
is_upper  = is_ctype cUpper
is_digit  = is_ctype cDigit
is_string = is_ctype cString

foreign label "hs_char_types" hs_char_types :: Addr

charType :: Char -> Int
charType c = ord (indexCharOffAddr hs_char_types (ord c))
\end{code}
