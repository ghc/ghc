-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Combinator
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Commonly used generic combinators
--
-----------------------------------------------------------------------------

module Text.Parsec.Combinator
    ( choice
    , count
    , between
    , option, optionMaybe, optional
    , skipMany1
    , many1
    , sepBy, sepBy1
    , endBy, endBy1
    , sepEndBy, sepEndBy1
    , chainl, chainl1
    , chainr, chainr1
    , eof, notFollowedBy
    -- tricky combinators
    , manyTill, lookAhead, anyToken
    ) where

import Control.Monad
import Text.Parsec.Prim

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- parser.

choice :: (Stream s m t) => [ParsecT s u m a] -> ParsecT s u m a
choice ps           = foldr (<|>) mzero ps

-- | @option x p@ tries to apply parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- >  priority  = option 0 (do{ d <- digit
-- >                          ; return (digitToInt d)
-- >                          })

option :: (Stream s m t) => a -> ParsecT s u m a -> ParsecT s u m a
option x p          = p <|> return x

-- | @optionMaybe p@ tries to apply parser @p@.  If @p@ fails without
-- consuming input, it return 'Nothing', otherwise it returns
-- 'Just' the value returned by @p@.

optionMaybe :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (Maybe a)
optionMaybe p       = option Nothing (liftM Just p)

-- | @optional p@ tries to apply parser @p@.  It will parse @p@ or nothing.
-- It only fails if @p@ fails after consuming input. It discards the result
-- of @p@.

optional :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
optional p          = do{ _ <- p; return ()} <|> return ()

-- | @between open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- >  braces  = between (symbol "{") (symbol "}")

between :: (Stream s m t) => ParsecT s u m open -> ParsecT s u m close
            -> ParsecT s u m a -> ParsecT s u m a
between open close p
                    = do{ _ <- open; x <- p; _ <- close; return x }

-- | @skipMany1 p@ applies the parser @p@ /one/ or more times, skipping
-- its result.

skipMany1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
skipMany1 p         = do{ _ <- p; skipMany p }
{-
skipMany p          = scan
                    where
                      scan  = do{ p; scan } <|> return ()
-}

-- | @many1 p@ applies the parser @p@ /one/ or more times. Returns a
-- list of the returned values of @p@.
--
-- >  word  = many1 letter

many1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m [a]
many1 p             = do{ x <- p; xs <- many p; return (x:xs) }
{-
many p              = scan id
                    where
                      scan f    = do{ x <- p
                                    ; scan (\tail -> f (x:tail))
                                    }
                                <|> return (f [])
-}


-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- >  commaSep p  = p `sepBy` (symbol ",")

sepBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy p sep         = sepBy1 p sep <|> return []

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.

sepBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1 p sep        = do{ x <- p
                        ; xs <- many (sep >> p)
                        ; return (x:xs)
                        }


-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@.

sepEndBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy1 p sep     = do{ x <- p
                        ; do{ _ <- sep
                            ; xs <- sepEndBy p sep
                            ; return (x:xs)
                            }
                          <|> return [x]
                        }

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@, ie. haskell style
-- statements. Returns a list of values returned by @p@.
--
-- >  haskellStatements  = haskellStatement `sepEndBy` semi

sepEndBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy p sep      = sepEndBy1 p sep <|> return []


-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.

endBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy1 p sep        = many1 (do{ x <- p; _ <- sep; return x })

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.
--
-- >   cStatements  = cStatement `endBy` semi

endBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy p sep         = many (do{ x <- p; _ <- sep; return x })

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of
-- @n@ values returned by @p@.

count :: (Stream s m t) => Int -> ParsecT s u m a -> ParsecT s u m [a]
count n p           | n <= 0    = return []
                    | otherwise = sequence (replicate n p)

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are no occurrences of @p@, the value @x@ is
-- returned.

chainr :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainr p op x       = chainr1 p op <|> return x

-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are zero occurrences of @p@, the value @x@ is
-- returned.

chainl :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainl p op x       = chainl1 p op <|> return x

-- | @chainl1 p op@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. . This parser can for example be used to eliminate left
-- recursion which typically occurs in expression grammars.
--
-- >  expr    = term   `chainl1` addop
-- >  term    = factor `chainl1` mulop
-- >  factor  = parens expr <|> integer
-- >
-- >  mulop   =   do{ symbol "*"; return (*)   }
-- >          <|> do{ symbol "/"; return (div) }
-- >
-- >  addop   =   do{ symbol "+"; return (+) }
-- >          <|> do{ symbol "-"; return (-) }

chainl1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainl1 p op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    }
                                <|> return x

-- | @chainr1 p op x@ parses /one/ or more occurrences of |p|,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@.

chainr1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainr1 p op        = scan
                    where
                      scan      = do{ x <- p; rest x }

                      rest x    = do{ f <- op
                                    ; y <- scan
                                    ; return (f x y)
                                    }
                                <|> return x

-----------------------------------------------------------
-- Tricky combinators
-----------------------------------------------------------
-- | The parser @anyToken@ accepts any kind of token. It is for example
-- used to implement 'eof'. Returns the accepted token.

anyToken :: (Stream s m t, Show t) => ParsecT s u m t
anyToken            = tokenPrim show (\pos _tok _toks -> pos) Just

-- | This parser only succeeds at the end of the input. This is not a
-- primitive parser but it is defined using 'notFollowedBy'.
--
-- >  eof  = notFollowedBy anyToken <?> "end of input"

eof :: (Stream s m t, Show t) => ParsecT s u m ()
eof                 = notFollowedBy anyToken <?> "end of input"

-- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
-- does not consume any input. This parser can be used to implement the
-- \'longest match\' rule. For example, when recognizing keywords (for
-- example @let@), we want to make sure that a keyword is not followed
-- by a legal identifier character, in which case the keyword is
-- actually an identifier (for example @lets@). We can program this
-- behaviour as follows:
--
-- >  keywordLet  = try (do{ string "let"
-- >                       ; notFollowedBy alphaNum
-- >                       })

notFollowedBy :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()
notFollowedBy p     = try (do{ c <- try p; unexpected (show c) }
                           <|> return ()
                          )

-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@.
-- This parser can be used to scan comments:
--
-- >  simpleComment   = do{ string "<!--"
-- >                      ; manyTill anyChar (try (string "-->"))
-- >                      }
--
--    Note the overlapping parsers @anyChar@ and @string \"-->\"@, and
--    therefore the use of the 'try' combinator.

manyTill :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill p end      = scan
                    where
                      scan  = do{ _ <- end; return [] }
                            <|>
                              do{ x <- p; xs <- scan; return (x:xs) }
