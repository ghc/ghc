Character classification

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module Ctype
	( is_ident	-- Char# -> Bool
	, is_symbol	-- Char# -> Bool
	, is_any	-- Char# -> Bool
	, is_space	-- Char# -> Bool
	, is_lower	-- Char# -> Bool
	, is_upper	-- Char# -> Bool
	, is_digit	-- Char# -> Bool
	, is_alphanum   -- Char# -> Bool

	, is_decdigit, is_hexdigit, is_octdigit
	, hexDigit, octDecDigit
	) where

#include "HsVersions.h"

import Data.Int		( Int32 )
import Data.Bits	( Bits((.&.)) )
import Data.Char	( ord, chr )
import Panic
\end{code}

Bit masks

\begin{code}
cIdent, cSymbol, cAny, cSpace, cLower, cUpper, cDigit :: Int
cIdent  =  1
cSymbol =  2
cAny    =  4
cSpace  =  8
cLower  = 16
cUpper  = 32
cDigit  = 64
\end{code}

The predicates below look costly, but aren't, GHC+GCC do a great job
at the big case below.

\begin{code}
{-# INLINE is_ctype #-}
is_ctype :: Int -> Char -> Bool
is_ctype mask c = (fromIntegral (charType c) .&. fromIntegral mask) /= (0::Int32)

is_ident, is_symbol, is_any, is_space, is_lower, is_upper, is_digit,
    is_alphanum :: Char -> Bool
is_ident  = is_ctype cIdent
is_symbol = is_ctype cSymbol
is_any    = is_ctype cAny
is_space  = is_ctype cSpace
is_lower  = is_ctype cLower
is_upper  = is_ctype cUpper
is_digit  = is_ctype cDigit
is_alphanum = is_ctype (cLower+cUpper+cDigit)
\end{code}

Utils

\begin{code}
hexDigit :: Char -> Int
hexDigit c | is_decdigit c = ord c - ord '0'
           | otherwise     = ord (to_lower c) - ord 'a' + 10

octDecDigit :: Char -> Int
octDecDigit c = ord c - ord '0'

is_decdigit :: Char -> Bool
is_decdigit c
	=  c >= '0' && c <= '9'

is_hexdigit :: Char -> Bool
is_hexdigit c
	=  is_decdigit c 
	|| (c >= 'a' && c <= 'f')
	|| (c >= 'A' && c <= 'F')

is_octdigit :: Char -> Bool
is_octdigit c = c >= '0' && c <= '7'

to_lower :: Char -> Char
to_lower c
  | c >=  'A' && c <= 'Z' = chr (ord c - (ord 'A' - ord 'a'))
  | otherwise = c
\end{code}

We really mean .|. instead of + below, but GHC currently doesn't do
any constant folding with bitops. *sigh*

\begin{code}
charType :: Char -> Int
charType c = case c of
   '\0'   -> 0                         -- \000
   '\1'   -> 0                         -- \001
   '\2'   -> 0                         -- \002
   '\3'   -> 0                         -- \003
   '\4'   -> 0                         -- \004
   '\5'   -> 0                         -- \005
   '\6'   -> 0                         -- \006
   '\7'   -> 0                         -- \007
   '\8'   -> 0                         -- \010
   '\9'   -> cSpace                    -- \t  (not allowed in strings, so !cAny)
   '\10'  -> cSpace	               -- \n  (ditto)
   '\11'  -> cSpace                    -- \v  (ditto)
   '\12'  -> cSpace                    -- \f  (ditto)
   '\13'  -> cSpace                    --  ^M (ditto)
   '\14'  -> 0                         -- \016
   '\15'  -> 0                         -- \017
   '\16'  -> 0                         -- \020
   '\17'  -> 0                         -- \021
   '\18'  -> 0                         -- \022
   '\19'  -> 0                         -- \023
   '\20'  -> 0                         -- \024
   '\21'  -> 0                         -- \025
   '\22'  -> 0                         -- \026
   '\23'  -> 0                         -- \027
   '\24'  -> 0                         -- \030
   '\25'  -> 0                         -- \031
   '\26'  -> 0                         -- \032
   '\27'  -> 0                         -- \033
   '\28'  -> 0                         -- \034
   '\29'  -> 0                         -- \035
   '\30'  -> 0                         -- \036
   '\31'  -> 0                         -- \037
   '\32'  -> cAny + cSpace             --
   '\33'  -> cAny + cSymbol            -- !
   '\34'  -> cAny                      -- "
   '\35'  -> cAny + cSymbol            --  #
   '\36'  -> cAny + cSymbol            --  $
   '\37'  -> cAny + cSymbol            -- %
   '\38'  -> cAny + cSymbol            -- &
   '\39'  -> cAny + cIdent             -- '
   '\40'  -> cAny                      -- (
   '\41'  -> cAny                      -- )
   '\42'  -> cAny + cSymbol            --  *
   '\43'  -> cAny + cSymbol            -- +
   '\44'  -> cAny                      -- ,
   '\45'  -> cAny + cSymbol            -- -
   '\46'  -> cAny + cSymbol            -- .
   '\47'  -> cAny + cSymbol            --  /
   '\48'  -> cAny + cIdent  + cDigit   -- 0
   '\49'  -> cAny + cIdent  + cDigit   -- 1
   '\50'  -> cAny + cIdent  + cDigit   -- 2
   '\51'  -> cAny + cIdent  + cDigit   -- 3
   '\52'  -> cAny + cIdent  + cDigit   -- 4
   '\53'  -> cAny + cIdent  + cDigit   -- 5
   '\54'  -> cAny + cIdent  + cDigit   -- 6
   '\55'  -> cAny + cIdent  + cDigit   -- 7
   '\56'  -> cAny + cIdent  + cDigit   -- 8
   '\57'  -> cAny + cIdent  + cDigit   -- 9
   '\58'  -> cAny + cSymbol            -- :
   '\59'  -> cAny                      -- ;
   '\60'  -> cAny + cSymbol            -- <
   '\61'  -> cAny + cSymbol            -- =
   '\62'  -> cAny + cSymbol            -- >
   '\63'  -> cAny + cSymbol            -- ?
   '\64'  -> cAny + cSymbol            -- @
   '\65'  -> cAny + cIdent  + cUpper   -- A
   '\66'  -> cAny + cIdent  + cUpper   -- B
   '\67'  -> cAny + cIdent  + cUpper   -- C
   '\68'  -> cAny + cIdent  + cUpper   -- D
   '\69'  -> cAny + cIdent  + cUpper   -- E
   '\70'  -> cAny + cIdent  + cUpper   -- F
   '\71'  -> cAny + cIdent  + cUpper   -- G
   '\72'  -> cAny + cIdent  + cUpper   -- H
   '\73'  -> cAny + cIdent  + cUpper   -- I
   '\74'  -> cAny + cIdent  + cUpper   -- J
   '\75'  -> cAny + cIdent  + cUpper   -- K
   '\76'  -> cAny + cIdent  + cUpper   -- L
   '\77'  -> cAny + cIdent  + cUpper   -- M
   '\78'  -> cAny + cIdent  + cUpper   -- N
   '\79'  -> cAny + cIdent  + cUpper   -- O
   '\80'  -> cAny + cIdent  + cUpper   -- P
   '\81'  -> cAny + cIdent  + cUpper   -- Q
   '\82'  -> cAny + cIdent  + cUpper   -- R
   '\83'  -> cAny + cIdent  + cUpper   -- S
   '\84'  -> cAny + cIdent  + cUpper   -- T
   '\85'  -> cAny + cIdent  + cUpper   -- U
   '\86'  -> cAny + cIdent  + cUpper   -- V
   '\87'  -> cAny + cIdent  + cUpper   -- W
   '\88'  -> cAny + cIdent  + cUpper   -- X
   '\89'  -> cAny + cIdent  + cUpper   -- Y
   '\90'  -> cAny + cIdent  + cUpper   -- Z
   '\91'  -> cAny                      -- [
   '\92'  -> cAny + cSymbol            -- backslash
   '\93'  -> cAny                      -- ]
   '\94'  -> cAny + cSymbol            --  ^
   '\95'  -> cAny + cIdent  + cLower   -- _
   '\96'  -> cAny                      -- `
   '\97'  -> cAny + cIdent  + cLower   -- a
   '\98'  -> cAny + cIdent  + cLower   -- b
   '\99'  -> cAny + cIdent  + cLower   -- c
   '\100' -> cAny + cIdent  + cLower   -- d
   '\101' -> cAny + cIdent  + cLower   -- e
   '\102' -> cAny + cIdent  + cLower   -- f
   '\103' -> cAny + cIdent  + cLower   -- g
   '\104' -> cAny + cIdent  + cLower   -- h
   '\105' -> cAny + cIdent  + cLower   -- i
   '\106' -> cAny + cIdent  + cLower   -- j
   '\107' -> cAny + cIdent  + cLower   -- k
   '\108' -> cAny + cIdent  + cLower   -- l
   '\109' -> cAny + cIdent  + cLower   -- m
   '\110' -> cAny + cIdent  + cLower   -- n
   '\111' -> cAny + cIdent  + cLower   -- o
   '\112' -> cAny + cIdent  + cLower   -- p
   '\113' -> cAny + cIdent  + cLower   -- q
   '\114' -> cAny + cIdent  + cLower   -- r
   '\115' -> cAny + cIdent  + cLower   -- s
   '\116' -> cAny + cIdent  + cLower   -- t
   '\117' -> cAny + cIdent  + cLower   -- u
   '\118' -> cAny + cIdent  + cLower   -- v
   '\119' -> cAny + cIdent  + cLower   -- w
   '\120' -> cAny + cIdent  + cLower   -- x
   '\121' -> cAny + cIdent  + cLower   -- y
   '\122' -> cAny + cIdent  + cLower   -- z
   '\123' -> cAny                      -- {
   '\124' -> cAny + cSymbol            --  |
   '\125' -> cAny                      -- }
   '\126' -> cAny + cSymbol            -- ~
   '\127' -> 0                         -- \177
   _ -> panic ("charType: " ++ show c)
\end{code}
