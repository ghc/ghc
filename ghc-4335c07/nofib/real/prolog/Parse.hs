--
-- General parsing library, based on Richard Bird's parselib.orw for Orwell
-- (with a number of extensions)
-- Mark P. Jones November 1990
--
-- uses Haskell B. version 0.99.3
--
module Parse(Parser, faiL, okay, tok, sat, orelse, seQ, doo,
             sptok, just, listOf, many, sp, many1) where

infixr 6 `seQ`
infixl 5 `doo`
infixr 4 `orelse`

--- Type definition:

type Parser a = [Char] -> [(a,[Char])]

-- A parser is a function which maps an input stream of characters into
-- a list of pairs each containing a parsed value and the remainder of the
-- unused input stream.  This approach allows us to use the list of
-- successes technique to detect errors (i.e. empty list ==> syntax error).
-- it also permits the use of ambiguous grammars in which there may be more
-- than one valid parse of an input string.

--- Primitive parsers:

-- faiL     is a parser which always fails.
-- okay v   is a parser which always succeeds without consuming any characters
--          from the input string, with parsed value v.
-- tok w    is a parser which succeeds if the input stream begins with the
--          string (token) w, returning the matching string and the following
--          input.  If the input does not begin with w then the parser fails.
-- sat p    is a parser which succeeds with value c if c is the first input
--          character and c satisfies the predicate p.

faiL        :: Parser a 
faiL inn      = []

okay        :: a -> Parser a  
okay v inn    = [(v,inn)]

tok         :: [Char] -> Parser [Char]
tok w inn     = [(w, drop n inn) | w == take n inn]
               where n = length w

sat         :: (Char -> Bool) -> Parser Char 
sat p []     = []
sat p (c:inn) = [ (c,inn) | p c ]

--- Parser combinators:

-- p1 `orelse` p2 is a parser which returns all possible parses of the input
--                string, first using the parser p1, then using parser p2.
-- p1 `seQ` p2    is a parser which returns pairs of values (v1,v2) where
--                v1 is the result of parsing the input string using p1 and
--                v2 is the result of parsing the remaining input using p2.
-- p `doo` f       is a parser which behaves like the parser p, but returns
--                the value f v wherever p would have returned the value v.
--
-- just p         is a parser which behaves like the parser p, but rejects any
--                parses in which the remaining input string is not blank.
-- sp p           behaves like the parser p, but ignores leading spaces.
-- sptok w        behaves like the parser tok w, but ignores leading spaces.
--
-- many p         returns a list of values, each parsed using the parser p.
-- many1 p        parses a non-empty list of values, each parsed using p.
-- listOf p s     parses a list of input values using the parser p, with
--                separators parsed using the parser s.

orelse             :: Parser a -> Parser a -> Parser a 
orelse p1 p2 inn = p1 inn ++ p2 inn
 
seQ                :: Parser a -> Parser b -> Parser (a,b)
seQ p1 p2 inn    = [((v1,v2),inn2) | (v1,inn1) <- p1 inn, (v2,inn2) <- p2 inn1]

doo                 :: Parser a -> (a -> b) -> Parser b 
doo p f inn       = [(f v, inn1) | (v,inn1) <- p inn]

just               :: Parser a -> Parser a
just p inn           = [ (v,"") | (v,inn')<- p inn, dropWhile (' '==) inn' == "" ]

sp                 :: Parser a -> Parser a
sp p                = p . dropWhile (' '==)

sptok              :: [Char] -> Parser [Char]
sptok               =  sp . tok

many               :: Parser a  -> Parser [a]
many p              = q
                      where q = ((p `seQ` q) `doo` makeList) `orelse` (okay [])

many1              :: Parser a -> Parser [a]
many1 p             = p `seQ` many p `doo` makeList

listOf             :: Parser a -> Parser b -> Parser [a]
listOf p s          = p `seQ` many (s `seQ` p) `doo` nonempty
                      `orelse` okay []
                      where nonempty (x,xs) = x:(map snd xs)

--- Internals:

makeList       :: (a,[a]) -> [a]
makeList (x,xs) = x:xs

{-
-- an attempt to optimise the performance of the standard prelude function
-- `take' in Haskell B 0.99.3 gives the wrong semantics.  The original
-- definition, given below works correctly and is used in the above.

safetake              :: (Integral a) => a -> [b] -> [b]
safetake  _     []     =  []
safetake  0     _      =  []
safetake (n+1) (x:xs)  =  x : safetake n xs
-}
--- End of Parse.hs
