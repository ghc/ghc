Character classification

\begin{code}
module Ctype
	( is_ident	-- Char# -> Bool
	, is_symbol	-- Char# -> Bool
	, is_any	-- Char# -> Bool
	, is_space	-- Char# -> Bool
	, is_lower	-- Char# -> Bool
	, is_upper	-- Char# -> Bool
	, is_digit	-- Char# -> Bool
	, is_alphanum   -- Char# -> Bool

	, is_hexdigit, is_octdigit
	, hexDigit, octDecDigit
	) where

#include "HsVersions.h"

import Data.Int		( Int32 )
import Data.Bits	( Bits((.&.)) )
import Data.Char	( ord, chr )
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

is_ident, is_symbol, is_any, is_space, is_lower, is_upper, is_digit :: Char -> Bool
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
hexDigit c | is_digit c = ord c - ord '0'
           | otherwise  = ord (to_lower c) - ord 'a' + 10

octDecDigit :: Char -> Int
octDecDigit c = ord c - ord '0'

is_hexdigit c
	=  is_digit c 
	|| (c >= 'a' && c <= 'f')
	|| (c >= 'A' && c <= 'F')

is_octdigit c = c >= '0' && c <= '7'

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
   '\9'   -> cAny + cSpace             -- \t
   '\10'  -> cSpace	               -- \n (not allowed in strings, so !cAny)
   '\11'  -> cAny + cSpace             -- \v
   '\12'  -> cAny + cSpace             -- \f
   '\13'  -> cAny + cSpace             --  ^M
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
   '\128' -> 0                         -- \200
   '\129' -> 0                         -- \201
   '\130' -> 0                         -- \202
   '\131' -> 0                         -- \203
   '\132' -> 0                         -- \204
   '\133' -> 0                         -- \205
   '\134' -> 0                         -- \206
   '\135' -> 0                         -- \207
   '\136' -> 0                         -- \210
   '\137' -> 0                         -- \211
   '\138' -> 0                         -- \212
   '\139' -> 0                         -- \213
   '\140' -> 0                         -- \214
   '\141' -> 0                         -- \215
   '\142' -> 0                         -- \216
   '\143' -> 0                         -- \217
   '\144' -> 0                         -- \220
   '\145' -> 0                         -- \221
   '\146' -> 0                         -- \222
   '\147' -> 0                         -- \223
   '\148' -> 0                         -- \224
   '\149' -> 0                         -- \225
   '\150' -> 0                         -- \226
   '\151' -> 0                         -- \227
   '\152' -> 0                         -- \230
   '\153' -> 0                         -- \231
   '\154' -> 0                         -- \232
   '\155' -> 0                         -- \233
   '\156' -> 0                         -- \234
   '\157' -> 0                         -- \235
   '\158' -> 0                         -- \236
   '\159' -> 0                         -- \237
   '\160' -> cSpace                    --
   '\161' -> cAny + cSymbol            -- ¡
   '\162' -> cAny + cSymbol            -- ¢
   '\163' -> cAny + cSymbol            -- £
   '\164' -> cAny + cSymbol            -- ¤
   '\165' -> cAny + cSymbol            -- ¥
   '\166' -> cAny + cSymbol            -- ¦
   '\167' -> cAny + cSymbol            -- §
   '\168' -> cAny + cSymbol            -- ¨
   '\169' -> cAny + cSymbol            -- ©
   '\170' -> cAny + cSymbol            -- ª
   '\171' -> cAny + cSymbol            -- «
   '\172' -> cAny + cSymbol            -- ¬
   '\173' -> cAny + cSymbol            -- ­
   '\174' -> cAny + cSymbol            -- ®
   '\175' -> cAny + cSymbol            -- ¯
   '\176' -> cAny + cSymbol            -- °
   '\177' -> cAny + cSymbol            -- ±
   '\178' -> cAny + cSymbol            -- ²
   '\179' -> cAny + cSymbol            -- ³
   '\180' -> cAny + cSymbol            -- ´
   '\181' -> cAny + cSymbol            -- µ
   '\182' -> cAny + cSymbol            -- ¶
   '\183' -> cAny + cSymbol            -- ·
   '\184' -> cAny + cSymbol            -- ¸
   '\185' -> cAny + cSymbol            -- ¹
   '\186' -> cAny + cSymbol            -- º
   '\187' -> cAny + cSymbol            -- »
   '\188' -> cAny + cSymbol            -- ¼
   '\189' -> cAny + cSymbol            -- ½
   '\190' -> cAny + cSymbol            -- ¾
   '\191' -> cAny + cSymbol            -- ¿
   '\192' -> cAny + cIdent  + cUpper   -- À
   '\193' -> cAny + cIdent  + cUpper   -- Á
   '\194' -> cAny + cIdent  + cUpper   -- Â
   '\195' -> cAny + cIdent  + cUpper   -- Ã
   '\196' -> cAny + cIdent  + cUpper   -- Ä
   '\197' -> cAny + cIdent  + cUpper   -- Å
   '\198' -> cAny + cIdent  + cUpper   -- Æ
   '\199' -> cAny + cIdent  + cUpper   -- Ç
   '\200' -> cAny + cIdent  + cUpper   -- È
   '\201' -> cAny + cIdent  + cUpper   -- É
   '\202' -> cAny + cIdent  + cUpper   -- Ê
   '\203' -> cAny + cIdent  + cUpper   -- Ë
   '\204' -> cAny + cIdent  + cUpper   -- Ì
   '\205' -> cAny + cIdent  + cUpper   -- Í
   '\206' -> cAny + cIdent  + cUpper   -- Î
   '\207' -> cAny + cIdent  + cUpper   -- Ï
   '\208' -> cAny + cIdent  + cUpper   -- Ð
   '\209' -> cAny + cIdent  + cUpper   -- Ñ
   '\210' -> cAny + cIdent  + cUpper   -- Ò
   '\211' -> cAny + cIdent  + cUpper   -- Ó
   '\212' -> cAny + cIdent  + cUpper   -- Ô
   '\213' -> cAny + cIdent  + cUpper   -- Õ
   '\214' -> cAny + cIdent  + cUpper   -- Ö
   '\215' -> cAny + cSymbol + cLower   -- ×
   '\216' -> cAny + cIdent  + cUpper   -- Ø
   '\217' -> cAny + cIdent  + cUpper   -- Ù
   '\218' -> cAny + cIdent  + cUpper   -- Ú
   '\219' -> cAny + cIdent  + cUpper   -- Û
   '\220' -> cAny + cIdent  + cUpper   -- Ü
   '\221' -> cAny + cIdent  + cUpper   -- Ý
   '\222' -> cAny + cIdent  + cUpper   -- Þ
   '\223' -> cAny + cIdent             -- ß
   '\224' -> cAny + cIdent  + cLower   -- à
   '\225' -> cAny + cIdent  + cLower   -- á
   '\226' -> cAny + cIdent  + cLower   -- â
   '\227' -> cAny + cIdent  + cLower   -- ã
   '\228' -> cAny + cIdent  + cLower   -- ä
   '\229' -> cAny + cIdent  + cLower   -- å
   '\230' -> cAny + cIdent  + cLower   -- æ
   '\231' -> cAny + cIdent  + cLower   -- ç
   '\232' -> cAny + cIdent  + cLower   -- è
   '\233' -> cAny + cIdent  + cLower   -- é
   '\234' -> cAny + cIdent  + cLower   -- ê
   '\235' -> cAny + cIdent  + cLower   -- ë
   '\236' -> cAny + cIdent  + cLower   -- ì
   '\237' -> cAny + cIdent  + cLower   -- í
   '\238' -> cAny + cIdent  + cLower   -- î
   '\239' -> cAny + cIdent  + cLower   -- ï
   '\240' -> cAny + cIdent  + cLower   -- ð
   '\241' -> cAny + cIdent  + cLower   -- ñ
   '\242' -> cAny + cIdent  + cLower   -- ò
   '\243' -> cAny + cIdent  + cLower   -- ó
   '\244' -> cAny + cIdent  + cLower   -- ô
   '\245' -> cAny + cIdent  + cLower   -- õ
   '\246' -> cAny + cIdent  + cLower   -- ö
   '\247' -> cAny + cSymbol            -- ÷
   '\248' -> cAny + cIdent             -- ø
   '\249' -> cAny + cIdent  + cLower   -- ù
   '\250' -> cAny + cIdent  + cLower   -- ú
   '\251' -> cAny + cIdent  + cLower   -- û
   '\252' -> cAny + cIdent  + cLower   -- ü
   '\253' -> cAny + cIdent  + cLower   -- ý
   '\254' -> cAny + cIdent  + cLower   -- þ
   '\255' -> cAny + cIdent  + cLower   -- ÿ
\end{code}
