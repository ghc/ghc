\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Read
-- Copyright   :  (c) The FFI Task Force, 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Read' class and instances for basic data types.
--
-----------------------------------------------------------------------------

module GHC.Read 
  ( Read(..)   -- class
  
  -- ReadS type
  , ReadS      -- :: *; = String -> [(a,String)]
  
  -- utility functions
  , reads      -- :: Read a => ReadS a
  , readp      -- :: Read a => ReadP a
  , readEither -- :: Read a => String -> Either String a
  , read       -- :: Read a => String -> a

  -- H98 compatibility
  , lex	        -- :: ReadS String
  , lexLitChar	-- :: ReadS String
  , readLitChar	-- :: ReadS Char
  , lexDigits	-- :: ReadS String
  
  -- defining readers
  , lexP       -- :: ReadPrec Lexeme
  , paren      -- :: ReadPrec a -> ReadPrec a
  , parens     -- :: ReadPrec a -> ReadPrec a
  , list       -- :: ReadPrec a -> ReadPrec [a]
  , choose     -- :: [(String, ReadPrec a)] -> ReadPrec a

  -- Temporary
  , readList__
  , readParen
  )
 where

import qualified Text.ParserCombinators.ReadP as P

import Text.ParserCombinators.ReadP
  ( ReadP
  , readP_to_S
  , readS_to_P
  )

import qualified Text.Read.Lex as L

import Text.Read.Lex
  ( Lexeme(..)
  , Number(..)
  , numberToInt
  , numberToInteger
  , numberToFloat
  , numberToDouble
  )

import Text.ParserCombinators.ReadPrec

import Data.Maybe
import Data.Either

import {-# SOURCE #-} GHC.Err		( error )
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.List
import GHC.Show		-- isAlpha etc
import GHC.Base

ratioPrec = 7 	-- Precedence of ':%' constructor
appPrec   = 10	-- Precedence of applictaion
\end{code}
-------------------------------------------------------
	TEMPORARY UNTIL I DO DERIVED READ

\begin{code}
readParen       :: Bool -> ReadS a -> ReadS a
readParen b g   =  if b then mandatory else optional
                   where optional r  = g r ++ mandatory r
                         mandatory r = do
				("(",s) <- lex r
				(x,t)   <- optional s
				(")",u) <- lex t
				return (x,u)


readList__ :: ReadS a -> ReadS [a]

readList__ readx
  = readParen False (\r -> do
		       ("[",s) <- lex r
		       readl s)
  where readl  s = 
           (do { ("]",t) <- lex s ; return ([],t) }) ++
	   (do { (x,t) <- readx s ; (xs,u) <- readl2 t ; return (x:xs,u) })

	readl2 s = 
	   (do { ("]",t) <- lex s ; return ([],t) }) ++
	   (do { (",",t) <- lex s ; (x,u) <- readx t ; (xs,v) <- readl2 u ; return (x:xs,v) })
\end{code}


%*********************************************************
%*							*
\subsection{The @Read@ class and @ReadS@ type}
%*							*
%*********************************************************

\begin{code}
------------------------------------------------------------------------
-- ReadS

-- | A parser for a type @a@, represented as a function that takes a
-- 'String' and returns a list of possible parses @(a,'String')@ pairs.
type ReadS a = String -> [(a,String)]

------------------------------------------------------------------------
-- class Read

class Read a where
  readsPrec    :: Int -> ReadS a
  readList     :: ReadS [a]
  readPrec     :: ReadPrec a
  readListPrec :: ReadPrec [a]
  
  -- default definitions
  readsPrec    = readPrec_to_S readPrec
  readList     = readPrec_to_S (list readPrec) 0
  readPrec     = readS_to_Prec readsPrec
  readListPrec = readS_to_Prec (\_ -> readList)

readListDefault :: Read a => ReadS [a]
readListDefault = readPrec_to_S readListPrec 0

readListPrecDefault :: Read a => ReadPrec [a]
readListPrecDefault = list readPrec

------------------------------------------------------------------------
-- utility functions

reads :: Read a => ReadS a
reads = readsPrec minPrec

readp :: Read a => ReadP a
readp = readPrec_to_P readPrec minPrec

readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []	-> Left "Prelude.read: no parse"
    _	-> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

read :: Read a => String -> a
read s = either error id (readEither s)

------------------------------------------------------------------------
-- H98 compatibility

lex :: ReadS String		-- As defined by H98
lex "" = [("","")] -- ugly hack
lex s  = readP_to_S (do { lexeme <- L.lex ;
		          return (show lexeme) }) s

lexLitChar :: ReadS String	-- As defined by H98
lexLitChar = readP_to_S (do { lexeme <- L.lexLitChar ;
			      return (show lexeme) })

readLitChar :: ReadS Char	-- As defined by H98
readLitChar = readP_to_S (do { Char c <- L.lexLitChar ;
			       return c })

lexDigits :: ReadS String
lexDigits = readP_to_S (P.munch1 isDigit)

------------------------------------------------------------------------
-- utility parsers

lexP :: ReadPrec Lexeme
lexP = lift L.lex

paren :: ReadPrec a -> ReadPrec a
-- (paren p) parses (P0) 
--	where p parses P0 in precedence context zero
paren p =
  do Single '(' <- lexP
     x          <- reset p
     Single ')' <- lexP
     return x

parens :: ReadPrec a -> ReadPrec a
-- (parens p) parses P, (P0), ((P0)), etc, 
--	where p parses P  in the current precedence context
--	        parses P0 in precedence context zero
parens p = optional
 where
  optional  = p +++ mandatory
  mandatory = paren optional

list :: ReadPrec a -> ReadPrec [a]
list readx =
  parens
  ( do Single '[' <- lexP
       (listRest False +++ listNext)
  )
 where
  listRest started =
    do Single c <- lexP
       case c of
         ']'           -> return []
         ',' | started -> listNext
         _             -> pfail
  
  listNext =
    do x  <- reset readx
       xs <- listRest True
       return (x:xs)

choose :: [(String, ReadPrec a)] -> ReadPrec a
-- Parse the specified lexeme and continue as specified
-- Esp useful for nullary constructors
choose sps = foldr ((+++) . try_one) pfail sps
	   where
	     try_one (s,p) = do { Ident s' <- lexP ;
				  if s == s' then p else pfail }
\end{code}


%*********************************************************
%*							*
\subsection{Simple instances of Read}
%*							*
%*********************************************************

\begin{code}
instance Read Char where
  readPrec =
    parens
    ( do Char c <- lexP
         return c
    )

  readListPrec =
    parens
    ( do String s <- lexP	-- Looks for "foo"
         return s
     +++
      readListPrecDefault	-- Looks for ['f','o','o']
    )				-- (more generous than H98 spec)

  readList = readListDefault

instance Read Bool where
  readPrec =
    parens
    ( do Ident s <- lexP
         case s of
           "True"  -> return True
           "False" -> return False
           _       -> pfail
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Ordering where
  readPrec =
    parens
    ( do Ident s <- lexP
         case s of
           "LT" -> return LT
           "EQ" -> return EQ
           "GT" -> return GT
           _    -> pfail
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault
\end{code}


%*********************************************************
%*							*
\subsection{Structure instances of Read: Maybe, List etc}
%*							*
%*********************************************************

For structured instances of Read we start using the precedences.  The
idea is then that 'parens (prec k p)' will fail immediately when trying
to parse it in a context with a higher precedence level than k. But if
there is one parenthesis parsed, then the required precedence level
drops to 0 again, and parsing inside p may succeed.

'appPrec' is just the precedence level of function application (maybe
it should be called 'appPrec' instead).  So, if we are parsing
function application, we'd better require the precedence level to be
at least 'appPrec'. Otherwise, we have to put parentheses around it.

'step' is used to increase the precedence levels inside a
parser, and can be used to express left- or right- associativity. For
example, % is defined to be left associative, so we only increase
precedence on the right hand side.

Note how step is used in for example the Maybe parser to increase the
precedence beyond appPrec, so that basically only literals and
parenthesis-like objects such as (...) and [...] can be an argument to
'Just'.

\begin{code}
instance Read a => Read (Maybe a) where
  readPrec =
    parens
    ( prec appPrec
      ( do Ident "Nothing" <- lexP
           return Nothing
       +++
        do Ident "Just" <- lexP
           x            <- step readPrec
           return (Just x)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b) => Read (Either a b) where
  readPrec =
    parens
    ( prec appPrec
      ( do Ident "Left" <- lexP
           x            <- step readPrec
           return (Left x)
       +++
        do Ident "Right" <- lexP
           y             <- step readPrec
           return (Right y)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read a => Read [a] where
  readPrec     = readListPrec
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Lexeme where
  readPrec     = lexP
  readListPrec = readListPrecDefault
  readList     = readListDefault
\end{code}


%*********************************************************
%*							*
\subsection{Numeric instances of Read}
%*							*
%*********************************************************

\begin{code}
readNumber :: Num a => (Number -> Maybe a) -> ReadPrec a
-- Read a signed number
readNumber convert =
  parens
  ( do x <- lexP
       case x of
         Symbol "-" -> do n <- readNumber convert
                          return (negate n)
       
         Number y   -> case convert y of
                         Just n  -> return n
                         Nothing -> pfail
         
         _          -> pfail
  )

readIEEENumber :: (RealFloat a) => (Number -> Maybe a) -> ReadPrec a
-- Read a Float/Double.
readIEEENumber convert =
  parens
  ( do x <- lexP
       case x of
         Ident "NaN"      -> return (0/0)
	 Ident "Infinity" -> return (1/0)
         Symbol "-" -> do n <- readIEEENumber convert
                          return (negate n)
       
         Number y   -> case convert y of
                         Just n  -> return n
                         Nothing -> pfail
         
         _          -> pfail
  )

instance Read Int where
  readPrec     = readNumber numberToInt
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Integer where
  readPrec     = readNumber numberToInteger
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Float where
  readPrec     = readIEEENumber numberToFloat
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Double where
  readPrec     = readIEEENumber numberToDouble
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Integral a, Read a) => Read (Ratio a) where
  readPrec =
    parens
    ( prec ratioPrec
      ( do x          <- step readPrec
           Symbol "%" <- lexP
           y          <- step readPrec
           return (x % y)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault
\end{code}


%*********************************************************
%*							*
\subsection{Tuple instances of Read}
%*							*
%*********************************************************

\begin{code}
instance Read () where
  readPrec =
    parens
    ( paren
      ( return ()
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b) => Read (a,b) where
  readPrec =
    parens
    ( paren
      ( do x <- readPrec
           Single ',' <- lexP
           y <- readPrec
           return (x,y)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault


instance (Read a, Read b, Read c) => Read (a, b, c) where
  readPrec =
    parens
    ( paren
      ( do x <- readPrec
           Single ',' <- lexP
           y <- readPrec
           Single ',' <- lexP
           z <- readPrec
           return (x,y,z)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
  readPrec =
    parens
    ( paren
      ( do w <- readPrec
           Single ',' <- lexP
           x <- readPrec
           Single ',' <- lexP
           y <- readPrec
           Single ',' <- lexP
           z <- readPrec
           return (w,x,y,z)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
  readPrec =
    parens
    ( paren
      ( do v <- readPrec
           Single ',' <- lexP
           w <- readPrec
           Single ',' <- lexP
           x <- readPrec
           Single ',' <- lexP
           y <- readPrec
           Single ',' <- lexP
           z <- readPrec
           return (v,w,x,y,z)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault
\end{code}
