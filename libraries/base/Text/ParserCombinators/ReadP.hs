{-# OPTIONS -fglasgow-exts -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ReadP
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a library of parser combinators, originally written by Koen Claessen.
-- It parses all alternatives in parallel, so it never keeps hold of 
-- the beginning of the input string, a common source of space leaks with
-- other parsers.  The '(+++)' choice combinator is genuinely commutative;
-- it makes no difference which branch is \"shorter\".

-----------------------------------------------------------------------------

module Text.ParserCombinators.ReadP
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
  satisfy,    -- :: (Char -> Bool) -> ReadP Char
  char,       -- :: Char -> ReadP Char
  string,     -- :: String -> ReadP String
  munch,      -- :: (Char -> Bool) -> ReadP String
  munch1,     -- :: (Char -> Bool) -> ReadP String
  skipSpaces, -- :: ReadP ()
  choice,     -- :: [ReadP a] -> ReadP a
  
  -- * Conversions
  readP_to_S, -- :: ReadP a -> ReadS a
  readS_to_P, -- :: ReadS a -> ReadP a
  )
 where

import Control.Monad( MonadPlus(..) )
import GHC.Show( isSpace  )
import GHC.Base

infixr 5 +++, <++

-- We define a local version of ReadS here,
-- because its "real" definition site is in GHC.Read
type ReadS a = String -> [(a,String)]

-- ---------------------------------------------------------------------------
-- The P type
-- is representation type -- should be kept abstract

data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | Final [(a,String)] -- invariant: list is non-empty!

-- Monad, MonadPlus

instance Monad P where
  return x = Result x Fail

  (Get f)      >>= k = Get (\c -> f c >>= k)
  (Look f)     >>= k = Look (\s -> f s >>= k)
  Fail         >>= k = Fail
  (Result x p) >>= k = k x `mplus` (p >>= k)
  (Final r)    >>= k = final [ys' | (x,s) <- r, ys' <- run (k x) s]

  fail _ = Fail

instance MonadPlus P where
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

newtype ReadP a = R (forall b . (a -> P b) -> P b)

-- Functor, Monad, MonadPlus

instance Functor ReadP where
  fmap h (R f) = R (\k -> f (k . h))

instance Monad ReadP where
  return x  = R (\k -> k x)
  fail _    = R (\_ -> Fail)
  R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))

instance MonadPlus ReadP where
  mzero = pfail
  mplus = (+++)

-- ---------------------------------------------------------------------------
-- Operations over P

final :: [(a,String)] -> P a
-- Maintains invariant for Final constructor
final [] = Fail
final r  = Final r

run :: P a -> ReadS a
run (Get f)      (c:s) = run (f c) s
run (Look f)     s     = run (f s) s
run (Result x p) s     = (x,s) : run p s
run (Final r)    _     = r
run _            _     = []

-- ---------------------------------------------------------------------------
-- Operations over ReadP

get :: ReadP Char
get = R Get

look :: ReadP String
look = R Look

pfail :: ReadP a
pfail = R (\_ -> Fail)

(+++) :: ReadP a -> ReadP a -> ReadP a
-- ^ Symmetric choice.
R f1 +++ R f2 = R (\k -> f1 k `mplus` f2 k)

(<++) :: ReadP a -> ReadP a -> ReadP a
-- ^ Local, exclusive, left-biased choice: If left parser
--   locally produces any result at all, then right parser is
--   not used.
R f <++ q =
  do s <- look
     probe (f return) s 0#
 where
  probe (Get f)        (c:s) n = probe (f c) s (n+#1#)
  probe (Look f)       s     n = probe (f s) s n
  probe p@(Result _ _) s     n = discard n >> R (p >>=)
  probe (Final r)      _     _ = R (Final r >>=)
  probe _              _     _ = q

  discard 0# = return ()
  discard n  = get >> discard (n-#1#)

gather :: ReadP a -> ReadP (String, a)
-- ^ Transforms a parser into one that does the same, but
--   in addition returns the exact characters read.
--   IMPORTANT NOTE: 'gather' gives a runtime error if its first argument
--   is built using any occurrences of readS_to_P. 
gather (R m) =
  R (\k -> gath id (m (\a -> return (\s -> k (s,a)))))  
 where
  gath l (Get f)      = Get (\c -> gath (l.(c:)) (f c))
  gath l Fail         = Fail
  gath l (Look f)     = Look (\s -> gath l (f s))
  gath l (Result k p) = k (l []) `mplus` gath l p
  gath l (Final r)    = error "do not use readS_to_P in gather!"

-- ---------------------------------------------------------------------------
-- Derived operations

satisfy :: (Char -> Bool) -> ReadP Char
satisfy p = do c <- get; if p c then return c else pfail

char :: Char -> ReadP Char
char c = satisfy (c ==)

string :: String -> ReadP String
string s = scan s
 where
  scan []     = do return s
  scan (c:cs) = do char c; scan cs

munch :: (Char -> Bool) -> ReadP String
-- (munch p) parses the first zero or more characters satisfying p
munch p =
  do s <- look
     scan s
 where
  scan (c:cs) | p c = do get; s <- scan cs; return (c:s)
  scan _            = do return ""

munch1 :: (Char -> Bool) -> ReadP String
-- (munch p) parses the first one or more characters satisfying p
munch1 p =
  do c <- get
     if p c then do s <- munch p; return (c:s) else pfail

choice :: [ReadP a] -> ReadP a
choice []     = pfail
choice [p]    = p
choice (p:ps) = p +++ choice ps

skipSpaces :: ReadP ()
skipSpaces =
  do s <- look
     skip s
 where
  skip (c:s) | isSpace c = do get; skip s
  skip _                 = do return ()

-- ---------------------------------------------------------------------------
-- Converting between ReadP and Read

readP_to_S :: ReadP a -> ReadS a
readP_to_S (R f) = run (f return)

readS_to_P :: ReadS a -> ReadP a
readS_to_P r =
  R (\k -> Look (\s -> final [bs'' | (a,s') <- r s, bs'' <- run (k a) s']))

-- ---------------------------------------------------------------------------
-- QuickCheck properties that are supposed to hold

{-
type Bag a = [a]

(=~) :: Ord a => Bag a -> Bag a -> Bool
xs =~ ys = sort xs == sort ys

prop_Get_Nil =
  readP_to_S get [] =~ []
  
prop_Get_Cons c s =
  readP_to_S get (c:s) =~ [(c,s)]
  
prop_Look s =
  readP_to_S look s =~ [(s,s)]
  
prop_Fail s =
  readP_to_S pfail s =~ ([] :: Bag (Int,String))
  
prop_Return x s =
  readP_to_S (return x) s =~ ([(x,s)] :: Bag (Int,String))
  
prop_ReadS r s =
  readP_to_S (readS_to_P r) s =~ (r s :: Bag (Int,String))
  
prop_Bind p k s =
  readP_to_S ((p :: ReadP Int) >>= k) s =~
    ([ ys'' | (x,s') <- readP_to_S p s, ys'' <- readP_to_S (k x) s' ]
      :: Bag (Int,String)
      )

prop_Plus p q s =
  readP_to_S ((p :: ReadP Int) +++ q) s =~
    (readP_to_S p s ++ readP_to_S q s)

prop_LeftPlus p q s =
  readP_to_S ((p :: ReadP Int) <++ q) s =~
    (readP_to_S p s +<+ readP_to_S q s)
 where
  [] +<+ ys = ys
  xs +<+ _  = xs
-}
