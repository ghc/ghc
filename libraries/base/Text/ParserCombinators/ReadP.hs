{-# OPTIONS -fno-implicit-prelude #-}
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

-- ---------------------------------------------------------------------------
-- The ReadP type

newtype ReadP a = R (forall b . (a -> P b) -> P b)

data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | ReadS (ReadS a)

-- We define a local version of ReadS here,
-- because its "real" definition site is in GHC.Read
type ReadS a = String -> [(a,String)]

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
-- Operations over ReadP

get :: ReadP Char
get = R (\k -> Get k)

look :: ReadP String
look = R (\k -> Look k)

(+++) :: ReadP a -> ReadP a -> ReadP a
R f1 +++ R f2 = R (\k -> f1 k >|< f2 k)

gather :: ReadP a -> ReadP (String, a)
-- ^ Transforms a parser into one that does the same, but
--   in addition returns the exact characters read.
--   IMPORTANT NOTE: 'gather' gives a runtime error if its first argument
--   is built using any occurrences of readS_to_P. 
gather (R m) 
  = R (\k -> gath id (m (\a -> Result (\s -> k (s,a)) Fail)))  
  where
    gath l (Get f)      = Get (\c -> gath (l.(c:)) (f c))
    gath l Fail         = Fail
    gath l (Look f)     = Look (\s -> gath l (f s))
    gath l (Result k p) = k (l []) >|< gath l p
    gath l (ReadS r)    = error "do not use ReadS in gather!"

(>|<) :: P a -> P a -> P a
-- Not exported!  Works over the representation type
Get f1     >|< Get f2     = Get (\c -> f1 c >|< f2 c)
Fail       >|< p          = p
p          >|< Fail       = p
Look f     >|< Look g     = Look (\s -> f s >|< g s)
Result x p >|< q          = Result x (p >|< q)
p          >|< Result x q = Result x (p >|< q)
Look f     >|< p          = Look (\s -> f s >|< p)
p          >|< Look f     = Look (\s -> p >|< f s)
p          >|< q          = ReadS (\s -> run p s ++ run q s)

run :: P a -> ReadS a
run (Get f)      []    = []
run (Get f)      (c:s) = run (f c) s
run (Look f)     s     = run (f s) s
run (Result x p) s     = (x,s) : run p s
run (ReadS r)    s     = r s
run Fail         _     = []

-- ---------------------------------------------------------------------------
-- Derived operations

pfail :: ReadP a
pfail = fail ""

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
choice ps = foldr (+++) pfail ps

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
readP_to_S (R f) = run (f (\x -> Result x Fail))

readS_to_P :: ReadS a -> ReadP a
readS_to_P r = R (\k -> ReadS (\s -> [ bs''
                                     | (a,s') <- r s
                                     , bs''   <- run (k a) s'
                                     ]))
