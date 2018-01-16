module ParseLib where

import Data.Char

type Parser a = String -> [(a, String)]

-- Trivial Parser:

empty :: Parser [a]
empty = (\x -> [([], x)])

-- Parser Generators:

character' :: Char -> Parser Char
character' c x		| null x		= []
			| c == head x		= [(head x, tail x)]
			| otherwise		= []

parse_if' :: (Char -> Bool) -> Parser Char
parse_if' p x		| null x		= []
			| p (head x)		= [(head x, tail x)]
			| otherwise		= []

string' :: String -> Parser String
string' s x		| s == fst split	= [split]
			| otherwise		= []
  where
	split					= splitAt (length s) x

parse_while' :: (Char -> Bool) -> Parser String
parse_while' p x	| null x		= []
			| p (head x)		= [span p x]
			| otherwise		= []

-- Elementary Parser Combinators:

choice :: [Parser r] -> Parser r
choice ps x = [r | p <- ps, r <- p x]

sequence2 :: Parser r1 -> Parser r2 -> Parser (r1, r2)
sequence2 p1 p2 x0 = [((r1, r2), x2) | (r1, x1) <- p1 x0, (r2, x2) <- p2 x1]

sequence3 :: Parser r1 -> Parser r2 -> Parser r3 -> Parser (r1, r2, r3)
sequence3 p1 p2 p3 x0 = [((r1, r2, r3), x3) | (r1, x1) <- p1 x0, (r2, x2) <- p2 x1, (r3, x3) <- p3 x2]

-- Parser Transformers:

transform :: (a -> b) -> Parser a -> Parser b
transform f p s = [(f r, x) | (r, x) <- p s]

allow_null :: Parser [a] -> Parser [a]
allow_null p x	| null result	= [([], x)]
		| otherwise	= result
  where
	result			= p x

forbid_null :: Parser [a] -> Parser [a]
forbid_null p = filter (\(r, x) -> (not . null) r) . p

-- More Parser Combinators:

cons :: Parser a -> Parser [a] -> Parser [a]
cons p ps = transform (\(a, as) -> a : as) (sequence2 p ps)

repetition0 :: Parser a -> Parser [a]
repetition0 p = allow_null (cons p (repetition0 p))

repetition1 :: Parser a -> Parser [a]
repetition1 p = (cons p (repetition0 p))

option :: Parser a -> Parser [a]
option p = choice [transform (\a -> [a]) p, empty]

-- Parser Combinators:

remove_left :: Parser a -> Parser b -> Parser b
remove_left p1 p2 = transform snd (sequence2 p1 p2)

remove_right :: Parser a -> Parser b -> Parser a
remove_right p1 p2 = transform fst (sequence2 p1 p2)

enclose :: Parser a -> Parser b -> Parser c -> Parser b
enclose p1 p2 p3 		= transform take_mid (sequence3 p1 p2 p3)
  where
	take_mid (a, b, c)	= b

glue :: Parser a -> Parser b -> Parser c -> Parser (a, c)
glue p1 p2 p3			= transform drop_mid (sequence3 p1 p2 p3)
  where
	drop_mid (a, b, c)	= (a, c)

-- Elementary Parser:

whitespace :: Parser String
whitespace = allow_null (parse_while' isSpace)

-- More Parser Generators:

character :: Char -> Parser Char
character c = remove_left whitespace (character' c)

parse_if :: (Char -> Bool) -> Parser Char
parse_if p = remove_left whitespace (parse_if' p)

string :: String -> Parser String
string s = remove_left whitespace (string' s)

parse_while :: (Char -> Bool) -> Parser String
parse_while p = remove_left whitespace (parse_while' p)

-- Final routine:

parse :: Parser a -> String -> [a]
parse parser = map fst . filter (null . snd) . parser
