--------------------------------------------------
-- $Log: Parsers.hs,v $
-- Revision 1.1  1996/01/08 20:02:55  partain
-- Initial revision
--
-- Revision 1.3  1994/03/15  15:34:53  thiemann
-- minor revisions
--
--Revision 1.2  1993/08/31  12:31:32  thiemann
--reflect changes in type FONT
--
--Revision 1.1  1993/08/17  12:34:29  thiemann
--Initial revision
--
-- $Locker:  $
--------------------------------------------------
module Parsers where

infixl 6 `using`, `using2`
infixr 7 `alt`
infixr 8 `thn`, `xthn`, `thnx` 

type Parser a b = [a] -> [(b, [a])]

succeed :: beta -> Parser alpha beta
succeed value tokens = [(value, tokens)]

-- the parser
--	satisfy p
-- accepts the language { token | p(token) }

satisfy :: (alpha -> Bool) -> Parser alpha alpha
satisfy p [] = []
satisfy p (token:tokens) | p token = succeed token tokens
			 | otherwise = []

-- the parser
--	literal word
-- accepts { word }

literal :: Eq alpha => alpha -> Parser alpha alpha
literal token = satisfy (== token)

-- if p1 and p2 are parsers accepting L1 and L2 then
--	then p1 p2
-- accepts L1.L2

thn :: Parser alpha beta -> Parser alpha gamma -> Parser alpha (beta, gamma)
thn p1 p2 =
	concat
	. map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> ((v1,v2), tokens2)) (p2 tokens1))
	. p1

thnx :: Parser alpha beta -> Parser alpha gamma -> Parser alpha beta
thnx p1 p2 = 
	concat
	. map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> (v1, tokens2)) (p2 tokens1))
	. p1

xthn :: Parser alpha beta -> Parser alpha gamma -> Parser alpha gamma
xthn p1 p2 =
	concat
	. map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> (v2, tokens2)) (p2 tokens1))
	. p1


-- if p1 and p2 are parsers accepting L1 and L2 then
--	alt p1 p2
-- accepts L1 \cup L2

alt :: Parser alpha beta -> Parser alpha beta -> Parser alpha beta
alt p1 p2 tokens = p1 tokens ++ p2 tokens

-- if p1 is a parser then
--	using p1 f
-- is a parser that accepts the same language as p1
-- but mangles the semantic value with f

using :: Parser alpha beta -> (beta -> gamma) -> Parser alpha gamma
using p1 f = map (\ (v, tokens) -> (f v, tokens)) . p1

using2 :: Parser a (b,c) -> (b -> c -> d) -> Parser a d
using2 p f = map ( \((v,w), tokens) -> (f v w, tokens)) . p

-- if p accepts L then plus p accepts L+

plus :: Parser alpha beta -> Parser alpha [beta]
plus p = (p `thn` rpt p) `using2` (:)

-- if p accepts L then rpt p accepts L*

rpt :: Parser alpha beta -> Parser alpha [beta]
rpt p = plus p `alt` succeed []

-- if p accepts L then opt p accepts L?

opt :: Parser alpha beta -> Parser alpha [beta]
opt p = (p `using` \x -> [x]) `alt` succeed []

-- followedBy p1 p2 recognizes L(p1) if followed by a word in L (p2)

followedBy :: Parser a b -> Parser a c -> Parser a b
followedBy p q tks = [(v, rest) | (v, rest) <- p tks, x <- q rest]
