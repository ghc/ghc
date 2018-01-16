-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compat.ReadP
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This is a library of parser combinators, originally written by Koen Claessen.
-- It parses all alternatives in parallel, so it never keeps hold of
-- the beginning of the input string, a common source of space leaks with
-- other parsers.  The '(+++)' choice combinator is genuinely commutative;
-- it makes no difference which branch is \"shorter\".
--
-- See also Koen's paper /Parallel Parsing Processes/
-- (<http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.19.9217>).
--
-- This version of ReadP has been locally hacked to make it H98, by
-- Martin Sj&#xF6;gren <mailto:msjogren@gmail.com>
--
-- The unit tests have been moved to UnitTest.Distribution.Compat.ReadP, by
-- Mark Lentczner <mailto:mark@glyphic.com>
-----------------------------------------------------------------------------

module Distribution.Compat.ReadP
  (
  -- * The 'ReadP' type
  ReadP,      -- :: * -> *; instance Functor, Monad, MonadPlus

  -- * Primitive operations
  get,        -- :: ReadP Char
  look,       -- :: ReadP String
  (+++),      -- :: ReadP a -> ReadP a -> ReadP a
  (<++),      -- :: ReadP a -> ReadP a -> ReadP a
  gather,     -- :: ReadP a -> ReadP (String, a)

  -- * Other operations
  pfail,      -- :: ReadP a
  eof,        -- :: ReadP ()
  satisfy,    -- :: (Char -> Bool) -> ReadP Char
  char,       -- :: Char -> ReadP Char
  string,     -- :: String -> ReadP String
  munch,      -- :: (Char -> Bool) -> ReadP String
  munch1,     -- :: (Char -> Bool) -> ReadP String
  skipSpaces, -- :: ReadP ()
  skipSpaces1,-- :: ReadP ()
  choice,     -- :: [ReadP a] -> ReadP a
  count,      -- :: Int -> ReadP a -> ReadP [a]
  between,    -- :: ReadP open -> ReadP close -> ReadP a -> ReadP a
  option,     -- :: a -> ReadP a -> ReadP a
  optional,   -- :: ReadP a -> ReadP ()
  many,       -- :: ReadP a -> ReadP [a]
  many1,      -- :: ReadP a -> ReadP [a]
  skipMany,   -- :: ReadP a -> ReadP ()
  skipMany1,  -- :: ReadP a -> ReadP ()
  sepBy,      -- :: ReadP a -> ReadP sep -> ReadP [a]
  sepBy1,     -- :: ReadP a -> ReadP sep -> ReadP [a]
  endBy,      -- :: ReadP a -> ReadP sep -> ReadP [a]
  endBy1,     -- :: ReadP a -> ReadP sep -> ReadP [a]
  chainr,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
  chainl,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
  chainl1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
  chainr1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
  manyTill,   -- :: ReadP a -> ReadP end -> ReadP [a]

  -- * Running a parser
  ReadS,      -- :: *; = String -> [(a,String)]
  readP_to_S, -- :: ReadP a -> ReadS a
  readS_to_P, -- :: ReadS a -> ReadP a

  -- ** Parsec
  parsecToReadP,
  )
 where

import Prelude ()
import Distribution.Compat.Prelude hiding (many, get)
import Control.Applicative (liftA2)

import qualified Distribution.Compat.MonadFail as Fail

import Control.Monad( replicateM, (>=>) )

import qualified Text.Parsec as P

infixr 5 +++, <++

-- ---------------------------------------------------------------------------
-- The P type
-- is representation type -- should be kept abstract

data P s a
  = Get (s -> P s a)
  | Look ([s] -> P s a)
  | Fail
  | Result a (P s a)
  | Final [(a,[s])] -- invariant: list is non-empty!

-- Monad, MonadPlus

instance Functor (P s) where
  fmap = liftM

instance Applicative (P s) where
  pure x = Result x Fail
  (<*>) = ap

instance Monad (P s) where
  return = pure

  (Get f)      >>= k = Get (f >=> k)
  (Look f)     >>= k = Look (f >=> k)
  Fail         >>= _ = Fail
  (Result x p) >>= k = k x `mplus` (p >>= k)
  (Final r)    >>= k = final [ys' | (x,s) <- r, ys' <- run (k x) s]

  fail = Fail.fail

instance Fail.MonadFail (P s) where
  fail _ = Fail

instance Alternative (P s) where
      empty = mzero
      (<|>) = mplus

instance MonadPlus (P s) where
  mzero = Fail

  -- most common case: two gets are combined
  Get f1     `mplus` Get f2     = Get (\c -> f1 c `mplus` f2 c)

  -- results are delivered as soon as possible
  Result x p `mplus` q          = Result x (p `mplus` q)
  p          `mplus` Result x q = Result x (p `mplus` q)

  -- fail disappears
  Fail       `mplus` p          = p
  p          `mplus` Fail       = p

  -- two finals are combined
  -- final + look becomes one look and one final (=optimization)
  -- final + sthg else becomes one look and one final
  Final r    `mplus` Final t    = Final (r ++ t)
  Final r    `mplus` Look f     = Look (\s -> Final (r ++ run (f s) s))
  Final r    `mplus` p          = Look (\s -> Final (r ++ run p s))
  Look f     `mplus` Final r    = Look (\s -> Final (run (f s) s ++ r))
  p          `mplus` Final r    = Look (\s -> Final (run p s ++ r))

  -- two looks are combined (=optimization)
  -- look + sthg else floats upwards
  Look f     `mplus` Look g     = Look (\s -> f s `mplus` g s)
  Look f     `mplus` p          = Look (\s -> f s `mplus` p)
  p          `mplus` Look f     = Look (\s -> p `mplus` f s)

-- ---------------------------------------------------------------------------
-- The ReadP type

newtype Parser r s a = R ((a -> P s r) -> P s r)
type ReadP r a = Parser r Char a

-- Functor, Monad, MonadPlus

instance Functor (Parser r s) where
  fmap h (R f) = R (\k -> f (k . h))

instance Applicative (Parser r s) where
  pure x  = R (\k -> k x)
  (<*>) = ap

instance Monad (Parser r s) where
  return = pure
  fail = Fail.fail
  R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))

instance Fail.MonadFail (Parser r s) where
  fail _    = R (const Fail)

--instance MonadPlus (Parser r s) where
--  mzero = pfail
--  mplus = (+++)

-- ---------------------------------------------------------------------------
-- Operations over P

final :: [(a,[s])] -> P s a
-- Maintains invariant for Final constructor
final [] = Fail
final r  = Final r

run :: P c a -> ([c] -> [(a, [c])])
run (Get f)      (c:s) = run (f c) s
run (Look f)     s     = run (f s) s
run (Result x p) s     = (x,s) : run p s
run (Final r)    _     = r
run _            _     = []

-- ---------------------------------------------------------------------------
-- Operations over ReadP

get :: ReadP r Char
-- ^ Consumes and returns the next character.
--   Fails if there is no input left.
get = R Get

look :: ReadP r String
-- ^ Look-ahead: returns the part of the input that is left, without
--   consuming it.
look = R Look

pfail :: ReadP r a
-- ^ Always fails.
pfail = R (const Fail)

eof :: ReadP r ()
-- ^ Succeeds iff we are at the end of input
eof = do { s <- look
         ; if null s then return ()
                     else pfail }

(+++) :: ReadP r a -> ReadP r a -> ReadP r a
-- ^ Symmetric choice.
R f1 +++ R f2 = R (\k -> f1 k `mplus` f2 k)

(<++) :: ReadP a a -> ReadP r a -> ReadP r a
-- ^ Local, exclusive, left-biased choice: If left parser
--   locally produces any result at all, then right parser is
--   not used.
R f <++ q =
  do s <- look
     probe (f return) s 0
 where
  probe (Get f')       (c:s) n = probe (f' c) s (n+1 :: Int)
  probe (Look f')      s     n = probe (f' s) s n
  probe p@(Result _ _) _     n = discard n >> R (p >>=)
  probe (Final r)      _     _ = R (Final r >>=)
  probe _              _     _ = q

  discard 0 = return ()
  discard n  = get >> discard (n-1 :: Int)

gather :: ReadP (String -> P Char r) a -> ReadP r (String, a)
-- ^ Transforms a parser into one that does the same, but
--   in addition returns the exact characters read.
--   IMPORTANT NOTE: 'gather' gives a runtime error if its first argument
--   is built using any occurrences of readS_to_P.
gather (R m) =
  R (\k -> gath id (m (\a -> return (\s -> k (s,a)))))
 where
  gath l (Get f)      = Get (\c -> gath (l.(c:)) (f c))
  gath _ Fail         = Fail
  gath l (Look f)     = Look (gath l . f)
  gath l (Result k p) = k (l []) `mplus` gath l p
  gath _ (Final _)    = error "do not use readS_to_P in gather!"

-- ---------------------------------------------------------------------------
-- Derived operations

satisfy :: (Char -> Bool) -> ReadP r Char
-- ^ Consumes and returns the next character, if it satisfies the
--   specified predicate.
satisfy p = do c <- get; if p c then return c else pfail

char :: Char -> ReadP r Char
-- ^ Parses and returns the specified character.
char c = satisfy (c ==)

string :: String -> ReadP r String
-- ^ Parses and returns the specified string.
string this = do s <- look; scan this s
 where
  scan []     _               = return this
  scan (x:xs) (y:ys) | x == y = get >> scan xs ys
  scan _      _               = pfail

munch :: (Char -> Bool) -> ReadP r String
-- ^ Parses the first zero or more characters satisfying the predicate.
munch p =
  do s <- look
     scan s
 where
  scan (c:cs) | p c = do _ <- get; s <- scan cs; return (c:s)
  scan _            = do return ""

munch1 :: (Char -> Bool) -> ReadP r String
-- ^ Parses the first one or more characters satisfying the predicate.
munch1 p =
  do c <- get
     if p c then do s <- munch p; return (c:s)
            else pfail

choice :: [ReadP r a] -> ReadP r a
-- ^ Combines all parsers in the specified list.
choice []     = pfail
choice [p]    = p
choice (p:ps) = p +++ choice ps

skipSpaces :: ReadP r ()
-- ^ Skips all whitespace.
skipSpaces =
  do s <- look
     skip s
 where
  skip (c:s) | isSpace c = do _ <- get; skip s
  skip _                 = do return ()

skipSpaces1 :: ReadP r ()
-- ^ Like 'skipSpaces' but succeeds only if there is at least one
-- whitespace character to skip.
skipSpaces1 = satisfy isSpace >> skipSpaces

count :: Int -> ReadP r a -> ReadP r [a]
-- ^ @ count n p @ parses @n@ occurrences of @p@ in sequence. A list of
--   results is returned.
count n p = replicateM n p

between :: ReadP r open -> ReadP r close -> ReadP r a -> ReadP r a
-- ^ @ between open close p @ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between open close p = do _ <- open
                          x <- p
                          _ <- close
                          return x

option :: a -> ReadP r a -> ReadP r a
-- ^ @option x p@ will either parse @p@ or return @x@ without consuming
--   any input.
option x p = p +++ return x

optional :: ReadP r a -> ReadP r ()
-- ^ @optional p@ optionally parses @p@ and always returns @()@.
optional p = (p >> return ()) +++ return ()

many :: ReadP r a -> ReadP r [a]
-- ^ Parses zero or more occurrences of the given parser.
many p = return [] +++ many1 p

many1 :: ReadP r a -> ReadP r [a]
-- ^ Parses one or more occurrences of the given parser.
many1 p = liftM2 (:) p (many p)

skipMany :: ReadP r a -> ReadP r ()
-- ^ Like 'many', but discards the result.
skipMany p = many p >> return ()

skipMany1 :: ReadP r a -> ReadP r ()
-- ^ Like 'many1', but discards the result.
skipMany1 p = p >> skipMany p

sepBy :: ReadP r a -> ReadP r sep -> ReadP r [a]
-- ^ @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy p sep = sepBy1 p sep +++ return []

sepBy1 :: ReadP r a -> ReadP r sep -> ReadP r [a]
-- ^ @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

endBy :: ReadP r a -> ReadP r sep -> ReadP r [a]
-- ^ @endBy p sep@ parses zero or more occurrences of @p@, separated and ended
--   by @sep@.
endBy p sep = many (do x <- p ; _ <- sep ; return x)

endBy1 :: ReadP r a -> ReadP r sep -> ReadP r [a]
-- ^ @endBy p sep@ parses one or more occurrences of @p@, separated and ended
--   by @sep@.
endBy1 p sep = many1 (do x <- p ; _ <- sep ; return x)

chainr :: ReadP r a -> ReadP r (a -> a -> a) -> a -> ReadP r a
-- ^ @chainr p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /right/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainr p op x = chainr1 p op +++ return x

chainl :: ReadP r a -> ReadP r (a -> a -> a) -> a -> ReadP r a
-- ^ @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl p op x = chainl1 p op +++ return x

chainr1 :: ReadP r a -> ReadP r (a -> a -> a) -> ReadP r a
-- ^ Like 'chainr', but parses one or more occurrences of @p@.
chainr1 p op = scan
  where scan   = p >>= rest
        rest x = do f <- op
                    y <- scan
                    return (f x y)
                 +++ return x

chainl1 :: ReadP r a -> ReadP r (a -> a -> a) -> ReadP r a
-- ^ Like 'chainl', but parses one or more occurrences of @p@.
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 +++ return x

manyTill :: ReadP r a -> ReadP [a] end -> ReadP r [a]
-- ^ @manyTill p end@ parses zero or more occurrences of @p@, until @end@
--   succeeds. Returns a list of values returned by @p@.
manyTill p end = scan
  where scan = (end >> return []) <++ (liftM2 (:) p scan)

-- ---------------------------------------------------------------------------
-- Converting between ReadP and Read

readP_to_S :: ReadP a a -> ReadS a
-- ^ Converts a parser into a Haskell ReadS-style function.
--   This is the main way in which you can \"run\" a 'ReadP' parser:
--   the expanded type is
-- @ readP_to_S :: ReadP a -> String -> [(a,String)] @
readP_to_S (R f) = run (f return)

readS_to_P :: ReadS a -> ReadP r a
-- ^ Converts a Haskell ReadS-style function into a parser.
--   Warning: This introduces local backtracking in the resulting
--   parser, and therefore a possible inefficiency.
readS_to_P r =
  R (\k -> Look (\s -> final [bs'' | (a,s') <- r s, bs'' <- run (k a) s']))

-- ---------------------------------------------------------------------------
-- Converting from Parsec to ReadP
--
-- | Convert @Parsec@ parser to 'ReadP'.
parsecToReadP
    :: P.Parsec [Char] u a
    -> u                 -- ^ initial user state
    -> ReadP r a
parsecToReadP p u = R $ \k -> Look $ \s ->
    case P.runParser (liftA2 (,) p P.getInput) u "<parsecToReadP>" s of
        Right (x, s') -> final (run (k x) s')
        Left _        -> Fail
