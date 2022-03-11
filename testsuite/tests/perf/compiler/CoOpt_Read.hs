{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module CoOpt_Read where

import Text.ParserCombinators.ReadP
  ( ReadS
  , readP_to_S
  )
import qualified Text.Read.Lex as L
import Text.ParserCombinators.ReadPrec
import GHC.Unicode
import GHC.Num
import GHC.Base

class Read a where
  {-# MINIMAL readsPrec | readPrec #-}

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

lex :: ReadS String             -- As defined by H2010
lex s  = readP_to_S L.hsLex s

------------------------------------------------------------------------
-- utility parsers

lexP :: ReadPrec L.Lexeme
-- ^ Parse a single lexeme
lexP = lift L.lex

expectP :: L.Lexeme -> ReadPrec ()
expectP lexeme = lift (L.expect lexeme)

expectCharP :: Char -> ReadPrec a -> ReadPrec a
expectCharP c a = do
  q <- get
  if q == c
    then a
    else pfail
{-# INLINE expectCharP #-}

skipSpacesThenP :: ReadPrec a -> ReadPrec a
skipSpacesThenP m =
  do s <- look
     skip s
 where
   skip (c:s) | isSpace c = get *> skip s
   skip _ = m

paren :: ReadPrec a -> ReadPrec a
-- ^ @(paren p)@ parses \"(P0)\"
--      where @p@ parses \"P0\" in precedence context zero
paren p = skipSpacesThenP (paren' p)

paren' :: ReadPrec a -> ReadPrec a
paren' p = expectCharP '(' $ reset p >>= \x ->
              skipSpacesThenP (expectCharP ')' (pure x))

parens :: ReadPrec a -> ReadPrec a
-- ^ @(parens p)@ parses \"P\", \"(P0)\", \"((P0))\", etc,
--      where @p@ parses \"P\"  in the current precedence context
--          and parses \"P0\" in precedence context zero
parens p = optional
  where
    optional = skipSpacesThenP (p +++ mandatory)
    mandatory = paren' optional

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

------------------------------------------------------------------------
-- Tuple instances of Read, up to size 15
------------------------------------------------------------------------

-- | @since 2.01
instance Read () where
  readPrec =
    parens
    ( paren
      ( return ()
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
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


-- | @since 2.01
instance (Read a, Read b, Read c) => Read (a, b, c) where
  readPrec = wrap_tup (do { (a,b) <- read_tup2; read_comma
                          ; c <- readPrec
                          ; return (a,b,c) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
  readPrec = wrap_tup read_tup4
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
  readPrec = wrap_tup (do { (a,b,c,d) <- read_tup4; read_comma
                          ; e <- readPrec
                          ; return (a,b,c,d,e) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e, Read f)
        => Read (a, b, c, d, e, f) where
  readPrec = wrap_tup (do { (a,b,c,d) <- read_tup4; read_comma
                          ; (e,f) <- read_tup2
                          ; return (a,b,c,d,e,f) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g)
        => Read (a, b, c, d, e, f, g) where
  readPrec = wrap_tup (do { (a,b,c,d) <- read_tup4; read_comma
                          ; (e,f) <- read_tup2; read_comma
                          ; g <- readPrec
                          ; return (a,b,c,d,e,f,g) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h)
        => Read (a, b, c, d, e, f, g, h) where
  readPrec     = wrap_tup read_tup8
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i)
        => Read (a, b, c, d, e, f, g, h, i) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; i <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j)
        => Read (a, b, c, d, e, f, g, h, i, j) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j) <- read_tup2
                          ; return (a,b,c,d,e,f,g,h,i,j) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k)
        => Read (a, b, c, d, e, f, g, h, i, j, k) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j) <- read_tup2; read_comma
                          ; k <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i,j,k) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l, Read m)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4; read_comma
                          ; m <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l,m) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l, Read m, Read n)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4; read_comma
                          ; (m,n) <- read_tup2
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l,m,n) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- | @since 2.01
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
