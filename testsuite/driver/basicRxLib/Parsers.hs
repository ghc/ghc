{-
This module contains combinator parser functions based on ones by
Peter Thiemann.
They have been tweaked with extra info to allow them to work for the 
ParseRegexp module. Its a recursive descent DETERMINISTIC parser.
Particularly that means that, it does not do full backtracking. 
For instance, (rpt (lit 'a')) will return only the longest match it
can.  ((rpt (lit 'a')) `thn` (lit 'a')) will not match "a".
-}

module Parsers 
 (Parser,
  initParser,
  lookupEnv,
  updateEnv,
  lookupFn,
  lookupAs,
  satisfy,
  lit,
  isWord,
  oneofC,
  anyC,
  butC,
  anyInt,
  anyPos,
  unitL,
  thenxPx,
  thenxP_,
  then_Px,
  (+.+),
  (..+),
  (+..),
  (|||),
  (<<<),
  (<<*),
  plus,
  star,
  opt,
  followedBy,
  notFollowedBy
 )

where

import FiniteMap
import Matchers(MatcherFlag)

infixr 8 +.+ , +.. , ..+
infixl 7 <<< , <<* 
infixr 6 |||


type Env = FiniteMap String MatcherFlag

type Parser a b c d =[c]	-- some kind of function list, labeled by 
				-- posn in list
                  -> [d]	-- an assertion list, labeled by posn in list
                  -> (Env,	-- Environment info about state of regexp
                      [a])	-- The input list
                  -> [(b,(Env,[a]))]-- Updated info
				-- The empty list here,
				-- represents a failed parse

-- initParser - Run a parser 
initParser :: Parser a b c d	-- the parser to run 
              -> [a]		-- the list to parse
              -> [c]		-- lookup environment, label by posn in list
              -> [d]		-- another lookup environment for assertions
              -> [(String,MatcherFlag)]	-- environment info about state we're in
              -> [(b,		-- The result of parser
                   [a])]	-- rest of list left over


initParser p1 inp fs as mfs =
    let   env =listToFM mfs
    in 
           case p1 fs as (env,inp) of
             ((res,(env,after)):xs) -> [(res,after)]
             [] -> []


-- lookupFn - look for the nth item, from the function  list
lookupFn :: Int -> Parser a c c d
lookupFn key fs _ tkns
  =  if key > length fs then
       []
     else -- finds eth function in given list, first is 1.
       [(fs !! (key-1),tkns)] 
      
-- lookupAs - look for the nth item, from the assertion list
lookupAs :: Int -> Parser a d e d
lookupAs key fs as tkns
  =  if key > length as then
       []
     else -- finds eth function in given list, first is 1.
       [(as !! (key-1),tkns)] 

-- lookupEnv - look for the nth item, from the environment list
lookupEnv :: String -> Parser a MatcherFlag c d
lookupEnv key fs as (env,tkns)
  = case lookupFM env key of 
       Just x -> [(x,(env,tkns))] 

updateEnv :: String -> MatcherFlag -> Parser a MatcherFlag c d
updateEnv key elt fs as (env,tkns)
   = [(elt,(addToFM env key elt,tkns))]

-- Sequence two parser actions, second uses result of first
-- don't care about result returned by second         
thenxP_ :: Parser a b c e -> (b -> Parser a d c e) -> Parser a b c e
thenxP_ p1 p2 fs as tkns =
        (concat
	. map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> (v1, tokens2)) (p2 v1 fs as tokens1)))
	(p1 fs as tkns)

-- Sequence two parser actions, second uses result of first
-- don't care about result returned by first         
then_Px :: Parser a b c e -> (b -> Parser a d c e) -> Parser a d c e
then_Px p1 p2 fs as tkns =
        (concat
	. map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> (v2, tokens2)) (p2 v1 fs as tokens1)))
	(p1 fs as tkns)

-- Sequence two parser actions, second uses result of first,
-- pair off result of first and second action
thenxPx :: Parser a b c e -> (b -> Parser a d c e) -> Parser a (b,d) c e
thenxPx p1 p2 fs as tkns =
        (concat
	. map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> ((v1,v2), tokens2)) (p2 v1 fs as tokens1)))
	(p1 fs as tkns)


-- have successfully matched something so return it

succeed :: b -> Parser a b c e
succeed value fs as rest = [(value, rest)]

-- the parser
--	satisfy p
-- accepts the language { token | p(token) }

satisfy :: (a -> Bool) -> Parser a a b e
satisfy p fs as (_,[]) = []
satisfy p fs as (env,(token:tokens)) | p token = succeed token fs as (env,tokens)
		        	  | otherwise = []

isnull :: Parser a [b] c e
isnull _ _ (env,[]) = [([],(env,[]))]
isnull _ _ _ = []

cut :: [a] -> [a] 
cut [] = []
cut (x:xs) = [x]

unitL :: a -> [a]
unitL = \x -> [x]

-- match any element
anyC :: Parser a a c e
anyC  = satisfy (const True)

-- match any element not a member of the list
butC :: Eq a => [a] -> Parser a a c e
butC cs = satisfy (not.(`elem` cs))

oneofC :: Eq a => [a] -> Parser a a c e
oneofC cs = satisfy (`elem` cs)

isWord :: Eq a => [a] -> (Parser a [a] b e)
isWord [x] =  (lit x <<< unitL)
isWord (x:xs) = (lit x <<< unitL) +.+ (isWord xs) <<* (++)

-- anyInt recognises any non-negative integer, from longest possible
-- list of digit chars
anyInt :: Parser Char Int a e
anyInt = ((plus (satisfy (\c -> (c >= '0') && (c <= '9'))))) <<< read

-- anyPos recognises any positive integer, from longest possible list
-- of digit chars
anyPos :: Parser Char Int a e
anyPos = ((satisfy (\c -> (c > '0') && (c <= '9'))) <<< unitL)
         +.+
         ((star (satisfy (\c -> (c >= '0') && (c <= '9')))))
         <<* (++)
         <<< read 

-- the parser
--	lit word
-- accepts { word }

lit :: Eq a => a -> Parser a a b e
lit token = satisfy (== token)


-- if p1 and p2 are parsers accepting L1 and L2 then
--	then p1 p2
-- accepts L1.L2

-- (+.+) - pair off result of first and second parser
--(+.+) :: Parser a b c e -> Parser a d c e -> Parser a (b,d) c e
p1 +.+ p2 = 
   \fs as tkns ->
        (concat
	 . map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> ((v1,v2), tokens2)) (p2 fs as tokens1)))
	 (p1 fs as tkns)

-- (+..) - don't care about result of second parser
(+..) :: Parser a b c e -> Parser a d c e -> Parser a b c e
p1 +.. p2 =
   \fs as tkns ->
        (concat
	. map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> (v1, tokens2)) (p2 fs as tokens1)))
	(p1 fs as tkns)

-- (..+) - don't care about result of first parser
(..+) :: Parser a b c e -> Parser a d c e -> Parser a d c e
p1 ..+ p2 =
   \fs as tkns ->
	(concat
	. map (\ (v1, tokens1) -> map (\ (v2, tokens2) -> (v2,tokens2)) (p2 fs as tokens1)))
	(p1 fs as tkns)


-- if p1 and p2 are parsers accepting L1 and L2 then
--	alt p1 p2
-- accepts L1 | L2

(|||) :: Parser a b c e -> Parser a b c e -> Parser a b c e
p1 ||| p2 = \fs as tokens -> cut (p1 fs as tokens ++ p2 fs as tokens)

-- if p1 is a parser then
--	p1 <<< f
-- is a parser that accepts the same language as p1
-- but mangles the semantic value with f

(<<<) :: Parser a b c e -> (b -> d) -> Parser a d c e
p1 <<< f = \fs as tkns -> map (\ (v, tokens) -> (f v, tokens)) (p1 fs as tkns)

(<<*) :: Parser a (b,c) d f -> (b -> c -> e) -> Parser a e d f
p <<* f = \fs as tkns -> map ( \((v,w), tokens) -> (f v w, tokens)) (p fs as tkns)

-- if p accepts L then plus p accepts L+

plus :: Parser a b c e -> Parser a [b] c e
plus p fs as tkns = cut  (((p +.+ star p) <<* (:)) fs as tkns)

-- if p accepts L then star p accepts L*

star :: Parser a b c e -> Parser a [b] c e
star p fs as tkns = cut ((plus p ||| succeed []) fs as tkns)

-- if p accepts L then opt p accepts L?

opt :: Parser a b c e -> Parser a [b] c e
opt p fs as tkns = cut (((p <<< \x -> [x]) ||| succeed []) fs as tkns)

-- followedBy p1 p2 recognizes L(p1) if followed by a word in L (p2)

followedBy :: Parser a b d e -> Parser a c d e -> Parser a b d e
followedBy p q fs as tks = [(v, rest) | (v, rest) <- p fs as tks, x <- q fs as rest]


notFollowedBy :: Parser a b d e -> Parser a c d e -> Parser a b d e
notFollowedBy p q fs as tkns = --((p +.. q) ||| (p +.. isnull)) fs as tkns

                   case p fs as tkns of
                      res@([(a,(_,[]))]) -> res 
                      res@([(a,res1)]) -> case q fs as res1 of
                                                [] -> res
                                                (x:xs) -> []
    
                      [] -> []
