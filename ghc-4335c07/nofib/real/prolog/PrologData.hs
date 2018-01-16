--
-- Representation of Prolog Terms, Clauses and Databases
-- Mark P. Jones November 1990
--
-- uses Haskell B. version 0.99.3
--
-- partain: some changes taken from Hugs 1.0 demo version
--
module PrologData(Id, Atom, Term(..), term, termlist, varsIn,
                  Clause((:==)), clause,
                  Database, emptyDb, renClauses, addClause, Parser ) where

import Parse
import Data.List(nub)--1.3
import Data.Char(isAlpha,isDigit,isUpper)

infix 6 :==

--- Prolog Terms:

type Id       = (Int,String)
type Atom     = String
data Term     = Var Id | Struct Atom [Term]
data Clause   = Term :== [Term]
data Database = Db [(Atom,[Clause])]

instance Eq Term where
    Var v       == Var w       =  v==w
    Struct a ts == Struct b ss =  a==b && ts==ss
    _           == _           =  False

--- Determine the list of variables in a term:

varsIn              :: Term -> [Id]
varsIn (Var i)       = [i]
varsIn (Struct i ts) = (nub . concat . map varsIn) ts

renameVars                  :: Int -> Term -> Term
renameVars lev (Var (n,s))   = Var (lev,s)
renameVars lev (Struct s ts) = Struct s (map (renameVars lev) ts)

--- Functions for manipulating databases (as an abstract datatype)

emptyDb      :: Database
emptyDb       = Db []

renClauses                  :: Database -> Int -> Term -> [Clause]
renClauses db n (Var _)      = []
renClauses db n (Struct a _) = [ r tm:==map r tp | (tm:==tp)<-clausesFor a db ]
                               where r = renameVars n

clausesFor           :: Atom -> Database -> [Clause]
clausesFor a (Db rss) = case dropWhile (\(n,rs) -> n<a) rss of
                         []         -> []
                         ((n,rs):_) -> if a==n then rs else []

addClause (Db rss) r@(Struct a _ :== _)
           = Db (update rss)
             where update []            = [(a,[r])]
                   update (h@(n,rs):rss')
                          | n==a        = (n,rs++[r]) : rss'
		          | n<a         = h : update rss'
                          | otherwise   = (a,[r]) : h : rss'

--- Output functions (defined as instances of Text):

instance Show Term where
  showsPrec p (Var (n,s))
              | n==0        = showString s
              | otherwise   = showString s . showChar '_' . shows n
  showsPrec p (Struct a []) = showString a
  showsPrec p (Struct a ts) = showString a . showChar '('
                                           . showWithSep "," ts
                                           . showChar ')'

instance Show Clause where
   showsPrec p (t:==[]) = shows t . showChar '.'
   showsPrec p (t:==gs) = shows t . showString ":=="
                                 . showWithSep "," gs
                                 . showChar '.'

instance Show Database where
    showsPrec p (Db [])  = showString "-- Empty Database --\n"
    showsPrec p (Db rss) = foldr1 (\u v-> u . showChar '\n' . v)
                                  [ showWithTerm "\n" rs | (i,rs)<-rss ]

--- Local functions for use in defining instances of Text:

showWithSep          :: Show a => String -> [a] -> ShowS
showWithSep s [x]     = shows x
showWithSep s (x:xs)  = shows x . showString s . showWithSep s xs

showWithTerm         :: Show a => String -> [a] -> ShowS
showWithTerm s xs     = foldr1 (.) [shows x . showString s | x<-xs]

--- String parsing functions for Terms and Clauses:
--- Local definitions:

letter       :: Parser Char
letter        = sat (\c -> isAlpha c || isDigit c || c `elem` ":;+=-*&%$#@?/.~!")

variable     :: Parser Term
variable      = sat isUpper `seQ` many letter `doo` makeVar
                where makeVar (initial,rest) = Var (0,(initial:rest))

struct       :: Parser Term
struct        = many letter `seQ` (sptok "(" `seQ` termlist `seQ` sptok ")"
                                       `doo` (\(o,(ts,c))->ts)
                                  `orelse`
                                   okay [])
                `doo` (\(name,terms)->Struct name terms)

--- Exports:

term         :: Parser Term
term          = sp (variable `orelse` struct)

termlist     :: Parser [Term]
termlist      = listOf term (sptok ",")

clause       :: Parser Clause
clause        = sp struct `seQ` (sptok ":==" `seQ` listOf term (sptok ",")
                                 `doo` (\(from,body)->body)
                                `orelse` okay [])
                          `seQ` sptok "."
                     `doo` (\(head,(goals,dot))->head:==goals)

--- End of PrologData.hs
