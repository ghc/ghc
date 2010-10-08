{-# LANGUAGE Arrows, MultiParamTypeClasses, FlexibleInstances #-}

-- Simple expression parser
-- (uses do-notation and operators)

module Main(main) where

import Control.Arrow
import Control.Category
import Data.Char
import Prelude hiding (id, (.))

-- Parsers

class (Eq s, Show s, ArrowPlus a) => ArrowParser s a where
	symbol :: s -> a b String

data Sym s = Sym { token :: s, value :: String }

-- Simple backtracking instance

newtype BTParser s a b = BTParser (a -> [Sym s] -> [(b, [Sym s])])

instance Category (BTParser s) where
	id = BTParser $ \a ss -> [(a, ss)]
	BTParser f . BTParser g = BTParser $ \b ss ->
		[(d, ss'') | (c, ss') <- g b ss, (d, ss'') <- f c ss']

instance Arrow (BTParser s) where
	arr f = BTParser $ \a ss -> [(f a, ss)]
	first (BTParser f) = BTParser $ \(b,d) ss ->
		[((c,d), ss') | (c,ss') <- f b ss]

instance ArrowZero (BTParser s) where
	zeroArrow = BTParser $ \b ss -> []

instance ArrowPlus (BTParser s) where
	BTParser f <+> BTParser g = BTParser $ \b ss -> f b ss ++ g b ss

instance (Eq s, Show s) => ArrowParser s (BTParser s) where
	symbol s = BTParser $ \b ss ->
		case ss of
		Sym s' v:ss' | s' == s -> [(v, ss')]
		_ -> []

runBTParser :: BTParser s () c -> [Sym s] -> c
runBTParser (BTParser parser) syms =
	head [c | (c, []) <- parser () syms]

-- Expressions

data ESym = LPar | RPar | Plus | Minus | Mult | Div | Number | Unknown
	deriving (Show, Eq, Ord)

type ExprParser = BTParser ESym
type ExprSym = Sym ESym

-- The grammar

expr :: ExprParser () Int
expr = proc () -> do
		x <- term -< ()
		expr' -< x

expr' :: ExprParser Int Int
expr' = proc x -> do
		returnA -< x
	<+> do
		(|(symbol Plus)|)
		y <- term -< ()
		expr' -< x + y
	<+> do
		(|(symbol Minus)|)
		y <- term -< ()
		expr' -< x - y

term :: ExprParser () Int
term = proc () -> do
		x <- factor -< ()
		term' -< x

term' :: ExprParser Int Int
term' = proc x -> do
		returnA -< x
	<+> do
		(|(symbol Mult)|)
		y <- factor -< ()
		term' -< x * y
	<+> do
		(|(symbol Div)|)
		y <- factor -< ()
		term' -< x `div` y

factor :: ExprParser () Int
factor = proc () -> do
		v <- (|(symbol Number)|)
		returnA -< read v::Int
	<+> do
		(|(symbol Minus)|)
		v <- factor -< ()
		returnA -< -v
	<+> do
		(|(symbol LPar)|)
		v <- expr -< ()
		(|(symbol RPar)|)
		returnA -< v

-- Lexical analysis

lexer :: String -> [ExprSym]
lexer [] = []
lexer ('(':cs) = Sym LPar "(":lexer cs
lexer (')':cs) = Sym RPar ")":lexer cs
lexer ('+':cs) = Sym Plus "+":lexer cs
lexer ('-':cs) = Sym Minus "-":lexer cs
lexer ('*':cs) = Sym Mult "*":lexer cs
lexer ('/':cs) = Sym Div "/":lexer cs
lexer (c:cs)
	| isSpace c = lexer cs
	| isDigit c = Sym Number (c:w):lexer cs'
	| otherwise = Sym Unknown [c]:lexer cs
		where (w,cs') = span isDigit cs

parse = runBTParser expr . lexer

main = do
	print (parse "1+2*(3+4)")
	print (parse "3*5-17/3+4")
