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
-----------------------------------------------------------------------------

module Text.ParserCombinators.ReadP
  ( 
  -- * The 'ReadP' type
    ReadP      -- :: * -> *; instance Functor, Monad, MonadPlus
  
  -- * Primitive operations
  , get        -- :: ReadP Char
  , look       -- :: ReadP String
  , (+++)      -- :: ReadP a -> ReadP a -> ReadP a
  
  -- * Other operations
  , pfail      -- :: ReadP a
  , satisfy    -- :: (Char -> Bool) -> ReadP Char
  , char       -- :: Char -> ReadP Char
  , string     -- :: String -> ReadP String
  , munch      -- :: (Char -> Bool) -> ReadP String
  , munch1     -- :: (Char -> Bool) -> ReadP String
  , skipSpaces -- :: ReadP ()
  , choice     -- :: [ReadP a] -> ReadP a
  
  -- * Conversions
  , readP_to_S -- :: ReadP a -> ReadS a
  , readS_to_P -- :: ReadS a -> ReadP a
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
 where
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
