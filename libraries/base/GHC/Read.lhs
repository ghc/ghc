\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Read
-- Copyright   :  (c) The University of Glasgow, 1994-2002
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
  , readListDefault, readListPrecDefault

  -- Temporary
  , readParen
  )
 where

import qualified Text.ParserCombinators.ReadP as P

import Text.ParserCombinators.ReadP
  ( ReadP
  , ReadS
  , readP_to_S
  )

import qualified Text.Read.Lex as L
-- Lex exports 'lex', which is also defined here,
-- hence the qualified import.
-- We can't import *anything* unqualified, because that
-- confuses Haddock.

import Text.ParserCombinators.ReadPrec

import Data.Maybe
import Data.Either

import {-# SOURCE #-} GHC.Err		( error )
#ifndef __HADDOCK__
import {-# SOURCE #-} GHC.Unicode	( isDigit )
#endif
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.List
import GHC.Show
import GHC.Base
import GHC.Arr
\end{code}


\begin{code}
readParen       :: Bool -> ReadS a -> ReadS a
-- A Haskell 98 function
readParen b g   =  if b then mandatory else optional
                   where optional r  = g r ++ mandatory r
                         mandatory r = do
				("(",s) <- lex r
				(x,t)   <- optional s
				(")",u) <- lex t
				return (x,u)
\end{code}


%*********************************************************
%*							*
\subsection{The @Read@ class}
%*							*
%*********************************************************

\begin{code}
------------------------------------------------------------------------
-- class Read

class Read a where
  readsPrec    :: Int -> ReadS a
  readList     :: ReadS [a]
  -- | Proposed replacement for 'readsPrec' using new-style parsers (GHC only).
  readPrec     :: ReadPrec a
  -- | Proposed replacement for 'readList' using new-style parsers (GHC only).
  readListPrec :: ReadPrec [a]
  
  -- default definitions
  readsPrec    = readPrec_to_S readPrec
  readList     = readPrec_to_S (list readPrec) 0
  readPrec     = readS_to_Prec readsPrec
  readListPrec = readS_to_Prec (\_ -> readList)

readListDefault :: Read a => ReadS [a]
-- ^ Use this to define the 'readList' method, if you don't want a special
--   case (GHC only; for other systems the default suffices).
readListDefault = readPrec_to_S readListPrec 0

readListPrecDefault :: Read a => ReadPrec [a]
-- ^ Use this to define the 'readListPrec' method, if you
--   don't want a special case (GHC only).
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
lex s  = readP_to_S L.hsLex s

-- | Read a string representation of a character, using Haskell
-- source-language escape conventions.  For example:
--
-- > lexLitChar  "\\nHello"  =  [("\\n", "Hello")]
--
lexLitChar :: ReadS String	-- As defined by H98
lexLitChar = readP_to_S (do { (s, _) <- P.gather L.lexChar ;
			      return s })
	-- There was a skipSpaces before the P.gather L.lexChar,
	-- but that seems inconsistent with readLitChar

-- | Read a string representation of a character, using Haskell
-- source-language escape conventions, and convert it to the character
-- that it encodes.  For example:
--
-- > readLitChar "\\nHello"  =  [('\n', "Hello")]
--
readLitChar :: ReadS Char	-- As defined by H98
readLitChar = readP_to_S L.lexChar

lexDigits :: ReadS String
lexDigits = readP_to_S (P.munch1 isDigit)

------------------------------------------------------------------------
-- utility parsers

lexP :: ReadPrec L.Lexeme
-- ^ Parse a single lexeme
lexP = lift L.lex

paren :: ReadPrec a -> ReadPrec a
-- ^ @(paren p)@ parses \"(P0)\"
--	where @p@ parses \"P0\" in precedence context zero
paren p = do L.Punc "(" <- lexP
	     x          <- reset p
	     L.Punc ")" <- lexP
	     return x

parens :: ReadPrec a -> ReadPrec a
-- ^ @(parens p)@ parses \"P\", \"(P0)\", \"((P0))\", etc, 
--	where @p@ parses \"P\"  in the current precedence context
--	        parses \"P0\" in precedence context zero
parens p = optional
 where
  optional  = p +++ mandatory
  mandatory = paren optional

list :: ReadPrec a -> ReadPrec [a]
-- ^ @(list p)@ parses a list of things parsed by @p@,
-- using the usual square-bracket syntax.
list readx =
  parens
  ( do L.Punc "[" <- lexP
       (listRest False +++ listNext)
  )
 where
  listRest started =
    do L.Punc c <- lexP
       case c of
         "]"           -> return []
         "," | started -> listNext
         _             -> pfail
  
  listNext =
    do x  <- reset readx
       xs <- listRest True
       return (x:xs)

choose :: [(String, ReadPrec a)] -> ReadPrec a
-- ^ Parse the specified lexeme and continue as specified.
-- Esp useful for nullary constructors; e.g.
--    @choose [(\"A\", return A), (\"B\", return B)]@
choose sps = foldr ((+++) . try_one) pfail sps
	   where
	     try_one (s,p) = do { L.Ident s' <- lexP ;
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
    ( do L.Char c <- lexP
         return c
    )

  readListPrec =
    parens
    ( do L.String s <- lexP	-- Looks for "foo"
         return s
     +++
      readListPrecDefault	-- Looks for ['f','o','o']
    )				-- (more generous than H98 spec)

  readList = readListDefault

instance Read Bool where
  readPrec =
    parens
    ( do L.Ident s <- lexP
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
    ( do L.Ident s <- lexP
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

'appPrec' is just the precedence level of function application.  So,
if we are parsing function application, we'd better require the
precedence level to be at least 'appPrec'. Otherwise, we have to put
parentheses around it.

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
    (do L.Ident "Nothing" <- lexP
        return Nothing
     +++
     prec appPrec (
	do L.Ident "Just" <- lexP
           x              <- step readPrec
           return (Just x))
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b) => Read (Either a b) where
  readPrec =
    parens
    ( prec appPrec
      ( do L.Ident "Left" <- lexP
           x            <- step readPrec
           return (Left x)
       +++
        do L.Ident "Right" <- lexP
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

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readPrec = parens $ prec appPrec $
	       do L.Ident "array" <- lexP
		  bounds <- step readPrec
		  vals   <- step readPrec
		  return (array bounds vals)

    readListPrec = readListPrecDefault
    readList     = readListDefault

instance Read L.Lexeme where
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
readNumber :: Num a => (L.Lexeme -> Maybe a) -> ReadPrec a
-- Read a signed number
readNumber convert =
  parens
  ( do x <- lexP
       case x of
         L.Symbol "-" -> do n <- readNumber convert
                            return (negate n)
       
         _   -> case convert x of
                   Just n  -> return n
                   Nothing -> pfail
  )

convertInt :: Num a => L.Lexeme -> Maybe a
convertInt (L.Int i) = Just (fromInteger i)
convertInt _         = Nothing

convertFrac :: Fractional a => L.Lexeme -> Maybe a
convertFrac (L.Int i) = Just (fromInteger i)
convertFrac (L.Rat r) = Just (fromRational r)
convertFrac _         = Nothing

instance Read Int where
  readPrec     = readNumber convertInt
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Integer where
  readPrec     = readNumber convertInt
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Float where
  readPrec     = readNumber convertFrac
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Double where
  readPrec     = readNumber convertFrac
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Integral a, Read a) => Read (Ratio a) where
  readPrec =
    parens
    ( prec ratioPrec
      ( do x            <- step readPrec
           L.Symbol "%" <- lexP
           y            <- step readPrec
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
           L.Punc "," <- lexP
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
           L.Punc "," <- lexP
           y <- readPrec
           L.Punc "," <- lexP
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
           L.Punc "," <- lexP
           x <- readPrec
           L.Punc "," <- lexP
           y <- readPrec
           L.Punc "," <- lexP
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
           L.Punc "," <- lexP
           w <- readPrec
           L.Punc "," <- lexP
           x <- readPrec
           L.Punc "," <- lexP
           y <- readPrec
           L.Punc "," <- lexP
           z <- readPrec
           return (v,w,x,y,z)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault
\end{code}
