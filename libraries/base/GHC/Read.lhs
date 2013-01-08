\begin{code}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, StandaloneDeriving, PatternGuards,
             ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

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

-- #hide
module GHC.Read
  ( Read(..)   -- class

  -- ReadS type
  , ReadS

  -- H2010 compatibility
  , lex
  , lexLitChar
  , readLitChar
  , lexDigits

  -- defining readers
  , lexP, expectP
  , paren
  , parens
  , list
  , choose
  , readListDefault, readListPrecDefault
  , readNumber

  -- Temporary
  , readParen
  )
 where

import qualified Text.ParserCombinators.ReadP as P

import Text.ParserCombinators.ReadP
  ( ReadS
  , readP_to_S
  )

import qualified Text.Read.Lex as L
-- Lex exports 'lex', which is also defined here,
-- hence the qualified import.
-- We can't import *anything* unqualified, because that
-- confuses Haddock.

import Text.ParserCombinators.ReadPrec

import Data.Maybe

import {-# SOURCE #-} GHC.Unicode       ( isDigit )
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.Show
import GHC.Base
import GHC.Err
import GHC.Arr
\end{code}


\begin{code}
-- | @'readParen' 'True' p@ parses what @p@ parses, but surrounded with
-- parentheses.
--
-- @'readParen' 'False' p@ parses what @p@ parses, but optionally
-- surrounded with parentheses.
readParen       :: Bool -> ReadS a -> ReadS a
-- A Haskell 2010 function
readParen b g   =  if b then mandatory else optional
                   where optional r  = g r ++ mandatory r
                         mandatory r = do
                                ("(",s) <- lex r
                                (x,t)   <- optional s
                                (")",u) <- lex t
                                return (x,u)
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Read@ class}
%*                                                      *
%*********************************************************

\begin{code}
------------------------------------------------------------------------
-- class Read

-- | Parsing of 'String's, producing values.
--
-- Minimal complete definition: 'readsPrec' (or, for GHC only, 'readPrec')
--
-- Derived instances of 'Read' make the following assumptions, which
-- derived instances of 'Text.Show.Show' obey:
--
-- * If the constructor is defined to be an infix operator, then the
--   derived 'Read' instance will parse only infix applications of
--   the constructor (not the prefix form).
--
-- * Associativity is not used to reduce the occurrence of parentheses,
--   although precedence may be.
--
-- * If the constructor is defined using record syntax, the derived 'Read'
--   will parse only the record-syntax form, and furthermore, the fields
--   must be given in the same order as the original declaration.
--
-- * The derived 'Read' instance allows arbitrary Haskell whitespace
--   between tokens of the input string.  Extra parentheses are also
--   allowed.
--
-- For example, given the declarations
--
-- > infixr 5 :^:
-- > data Tree a =  Leaf a  |  Tree a :^: Tree a
--
-- the derived instance of 'Read' in Haskell 2010 is equivalent to
--
-- > instance (Read a) => Read (Tree a) where
-- >
-- >         readsPrec d r =  readParen (d > app_prec)
-- >                          (\r -> [(Leaf m,t) |
-- >                                  ("Leaf",s) <- lex r,
-- >                                  (m,t) <- readsPrec (app_prec+1) s]) r
-- >
-- >                       ++ readParen (d > up_prec)
-- >                          (\r -> [(u:^:v,w) |
-- >                                  (u,s) <- readsPrec (up_prec+1) r,
-- >                                  (":^:",t) <- lex s,
-- >                                  (v,w) <- readsPrec (up_prec+1) t]) r
-- >
-- >           where app_prec = 10
-- >                 up_prec = 5
--
-- Note that right-associativity of @:^:@ is unused.
--
-- The derived instance in GHC is equivalent to
--
-- > instance (Read a) => Read (Tree a) where
-- >
-- >         readPrec = parens $ (prec app_prec $ do
-- >                                  Ident "Leaf" <- lexP
-- >                                  m <- step readPrec
-- >                                  return (Leaf m))
-- >
-- >                      +++ (prec up_prec $ do
-- >                                  u <- step readPrec
-- >                                  Symbol ":^:" <- lexP
-- >                                  v <- step readPrec
-- >                                  return (u :^: v))
-- >
-- >           where app_prec = 10
-- >                 up_prec = 5
-- >
-- >         readListPrec = readListPrecDefault

class Read a where
  -- | attempts to parse a value from the front of the string, returning
  -- a list of (parsed value, remaining string) pairs.  If there is no
  -- successful parse, the returned list is empty.
  --
  -- Derived instances of 'Read' and 'Text.Show.Show' satisfy the following:
  --
  -- * @(x,\"\")@ is an element of
  --   @('readsPrec' d ('Text.Show.showsPrec' d x \"\"))@.
  --
  -- That is, 'readsPrec' parses the string produced by
  -- 'Text.Show.showsPrec', and delivers the value that
  -- 'Text.Show.showsPrec' started with.

  readsPrec    :: Int   -- ^ the operator precedence of the enclosing
                        -- context (a number from @0@ to @11@).
                        -- Function application has precedence @10@.
                -> ReadS a

  -- | The method 'readList' is provided to allow the programmer to
  -- give a specialised way of parsing lists of values.
  -- For example, this is used by the predefined 'Read' instance of
  -- the 'Char' type, where values of type 'String' should be are
  -- expected to use double quotes, rather than square brackets.
  readList     :: ReadS [a]

  -- | Proposed replacement for 'readsPrec' using new-style parsers (GHC only).
  readPrec     :: ReadPrec a

  -- | Proposed replacement for 'readList' using new-style parsers (GHC only).
  -- The default definition uses 'readList'.  Instances that define 'readPrec'
  -- should also define 'readListPrec' as 'readListPrecDefault'.
  readListPrec :: ReadPrec [a]
  
  -- default definitions
  readsPrec    = readPrec_to_S readPrec
  readList     = readPrec_to_S (list readPrec) 0
  readPrec     = readS_to_Prec readsPrec
  readListPrec = readS_to_Prec (\_ -> readList)

readListDefault :: Read a => ReadS [a]
-- ^ A possible replacement definition for the 'readList' method (GHC only).
--   This is only needed for GHC, and even then only for 'Read' instances
--   where 'readListPrec' isn't defined as 'readListPrecDefault'.
readListDefault = readPrec_to_S readListPrec 0

readListPrecDefault :: Read a => ReadPrec [a]
-- ^ A possible replacement definition for the 'readListPrec' method,
--   defined using 'readPrec' (GHC only).
readListPrecDefault = list readPrec

------------------------------------------------------------------------
-- H2010 compatibility

-- | The 'lex' function reads a single lexeme from the input, discarding
-- initial white space, and returning the characters that constitute the
-- lexeme.  If the input string contains only white space, 'lex' returns a
-- single successful \`lexeme\' consisting of the empty string.  (Thus
-- @'lex' \"\" = [(\"\",\"\")]@.)  If there is no legal lexeme at the
-- beginning of the input string, 'lex' fails (i.e. returns @[]@).
--
-- This lexer is not completely faithful to the Haskell lexical syntax
-- in the following respects:
--
-- * Qualified names are not handled properly
--
-- * Octal and hexadecimal numerics are not recognized as a single token
--
-- * Comments are not treated properly
lex :: ReadS String             -- As defined by H2010
lex s  = readP_to_S L.hsLex s

-- | Read a string representation of a character, using Haskell
-- source-language escape conventions.  For example:
--
-- > lexLitChar  "\\nHello"  =  [("\\n", "Hello")]
--
lexLitChar :: ReadS String      -- As defined by H2010
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
readLitChar :: ReadS Char       -- As defined by H2010
readLitChar = readP_to_S L.lexChar

-- | Reads a non-empty string of decimal digits.
lexDigits :: ReadS String
lexDigits = readP_to_S (P.munch1 isDigit)

------------------------------------------------------------------------
-- utility parsers

lexP :: ReadPrec L.Lexeme
-- ^ Parse a single lexeme
lexP = lift L.lex

expectP :: L.Lexeme -> ReadPrec ()
expectP lexeme = lift (L.expect lexeme)

paren :: ReadPrec a -> ReadPrec a
-- ^ @(paren p)@ parses \"(P0)\"
--      where @p@ parses \"P0\" in precedence context zero
paren p = do expectP (L.Punc "(")
             x <- reset p
             expectP (L.Punc ")")
             return x

parens :: ReadPrec a -> ReadPrec a
-- ^ @(parens p)@ parses \"P\", \"(P0)\", \"((P0))\", etc, 
--      where @p@ parses \"P\"  in the current precedence context
--          and parses \"P0\" in precedence context zero
parens p = optional
 where
  optional  = p +++ mandatory
  mandatory = paren optional

list :: ReadPrec a -> ReadPrec [a]
-- ^ @(list p)@ parses a list of things parsed by @p@,
-- using the usual square-bracket syntax.
list readx =
  parens
  ( do expectP (L.Punc "[")
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
-- We match both Ident and Symbol because the constructor
-- might be an operator eg (:=:)
choose sps = foldr ((+++) . try_one) pfail sps
           where
             try_one (s,p) = do { token <- lexP ;
                                  case token of
                                    L.Ident s'  | s==s' -> p
                                    L.Symbol s' | s==s' -> p
                                    _other              -> pfail }
\end{code}


%*********************************************************
%*                                                      *
\subsection{Simple instances of Read}
%*                                                      *
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
    ( do L.String s <- lexP     -- Looks for "foo"
         return s
     +++
      readListPrecDefault       -- Looks for ['f','o','o']
    )                           -- (more generous than H2010 spec)

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
%*                                                      *
\subsection{Structure instances of Read: Maybe, List etc}
%*                                                      *
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
    (do expectP (L.Ident "Nothing")
        return Nothing
     +++
     prec appPrec (
        do expectP (L.Ident "Just")
           x <- step readPrec
           return (Just x))
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read a => Read [a] where
  readPrec     = readListPrec
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readPrec = parens $ prec appPrec $
               do expectP (L.Ident "array")
                  theBounds <- step readPrec
                  vals   <- step readPrec
                  return (array theBounds vals)

    readListPrec = readListPrecDefault
    readList     = readListDefault

instance Read L.Lexeme where
  readPrec     = lexP
  readListPrec = readListPrecDefault
  readList     = readListDefault
\end{code}


%*********************************************************
%*                                                      *
\subsection{Numeric instances of Read}
%*                                                      *
%*********************************************************

\begin{code}
readNumber :: Num a => (L.Lexeme -> ReadPrec a) -> ReadPrec a
-- Read a signed number
readNumber convert =
  parens
  ( do x <- lexP
       case x of
         L.Symbol "-" -> do y <- lexP
                            n <- convert y
                            return (negate n)

         _   -> convert x
  )


convertInt :: Num a => L.Lexeme -> ReadPrec a
convertInt (L.Number n)
 | Just i <- L.numberToInteger n = return (fromInteger i)
convertInt _ = pfail

convertFrac :: forall a . RealFloat a => L.Lexeme -> ReadPrec a
convertFrac (L.Ident "NaN")      = return (0 / 0)
convertFrac (L.Ident "Infinity") = return (1 / 0)
convertFrac (L.Number n) = let resRange = floatRange (undefined :: a)
                           in case L.numberToRangedRational resRange n of
                              Nothing -> return (1 / 0)
                              Just rat -> return $ fromRational rat
convertFrac _            = pfail

instance Read Int where
  readPrec     = readNumber convertInt
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Word where
    readsPrec p s = [(fromInteger x, r) | (x, r) <- readsPrec p s]

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
      ( do x <- step readPrec
           expectP (L.Symbol "%")
           y <- step readPrec
           return (x % y)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault
\end{code}


%*********************************************************
%*                                                      *
        Tuple instances of Read, up to size 15
%*                                                      *
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
  readPrec = wrap_tup read_tup2
  readListPrec = readListPrecDefault
  readList     = readListDefault

wrap_tup :: ReadPrec a -> ReadPrec a
wrap_tup p = parens (paren p)

read_comma :: ReadPrec ()
read_comma = expectP (L.Punc ",")

read_tup2 :: (Read a, Read b) => ReadPrec (a,b)
-- Reads "a , b"  no parens!
read_tup2 = do x <- readPrec
               read_comma
               y <- readPrec
               return (x,y)

read_tup4 :: (Read a, Read b, Read c, Read d) => ReadPrec (a,b,c,d)
read_tup4 = do  (a,b) <- read_tup2
                read_comma
                (c,d) <- read_tup2
                return (a,b,c,d)


read_tup8 :: (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h)
          => ReadPrec (a,b,c,d,e,f,g,h)
read_tup8 = do  (a,b,c,d) <- read_tup4
                read_comma
                (e,f,g,h) <- read_tup4
                return (a,b,c,d,e,f,g,h)


instance (Read a, Read b, Read c) => Read (a, b, c) where
  readPrec = wrap_tup (do { (a,b) <- read_tup2; read_comma 
                          ; c <- readPrec 
                          ; return (a,b,c) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
  readPrec = wrap_tup read_tup4
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
  readPrec = wrap_tup (do { (a,b,c,d) <- read_tup4; read_comma
                          ; e <- readPrec
                          ; return (a,b,c,d,e) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f)
        => Read (a, b, c, d, e, f) where
  readPrec = wrap_tup (do { (a,b,c,d) <- read_tup4; read_comma
                          ; (e,f) <- read_tup2
                          ; return (a,b,c,d,e,f) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g)
        => Read (a, b, c, d, e, f, g) where
  readPrec = wrap_tup (do { (a,b,c,d) <- read_tup4; read_comma
                          ; (e,f) <- read_tup2; read_comma
                          ; g <- readPrec
                          ; return (a,b,c,d,e,f,g) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h)
        => Read (a, b, c, d, e, f, g, h) where
  readPrec     = wrap_tup read_tup8
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i)
        => Read (a, b, c, d, e, f, g, h, i) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; i <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j)
        => Read (a, b, c, d, e, f, g, h, i, j) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j) <- read_tup2
                          ; return (a,b,c,d,e,f,g,h,i,j) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k)
        => Read (a, b, c, d, e, f, g, h, i, j, k) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j) <- read_tup2; read_comma
                          ; k <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i,j,k) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l, Read m)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4; read_comma
                          ; m <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l,m) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l, Read m, Read n)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4; read_comma
                          ; (m,n) <- read_tup2
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l,m,n) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l, Read m, Read n, Read o)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4; read_comma
                          ; (m,n) <- read_tup2; read_comma
                          ; o <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) })
  readListPrec = readListPrecDefault
  readList     = readListDefault
\end{code}

