--!!! Testing Haskell 1.3 syntax

-- Haskell 1.3 syntax differs from Haskell 1.2 syntax in several ways:

-- * Qualified names in export lists
module TestSyntax where

-- * Qualified import/export

--   1) Syntax:

import qualified Prelude as P

import Prelude
import qualified Prelude

import Prelude ()
import Prelude (fst,snd)
import qualified Prelude(fst,snd)

-- bizarre syntax allowed in draft of Haskell 1.3 
import Prelude(,)
import Prelude(fst,snd,)
import Prelude(Ord(..),Eq((==),(/=)),)
import Prelude hiding (fst,snd,)

import Prelude hiding (fst,snd)
import qualified Prelude hiding (fst,snd)

import Prelude as P
import qualified Prelude as P

import Prelude as P(fst,snd)
import Prelude as P(,)
import qualified Prelude as P(fst,snd)

import Prelude as P hiding (fst,snd)
import qualified Prelude as P hiding (fst,snd)

-- 2) Use of qualified type names
-- 3) Use of qualified constructors
-- 4) Use of qualified variables

-- * No n+k patterns (yippee!)
--   (No tests yet)

-- Some things are unchanged.

-- * Unqualified imports and use of hiding/selective import.
--
--   Note: it's not clear how these various imports are supposed to
--         interact with one another.
--         John explains: 
--         1) "hiding" lists etc are just abbreviations for very long
--            lists.
--         2) Multiple imports are additive.
--         (This makes the meaning order-independent!)
--   Note: Hugs allows imports anywhere a topdecl is allowed.
--         This isn't legal Haskell - but it does no harm.

-- import Prelude(lex)
-- import Prelude
-- import Prelude hiding (lex)
-- lex = 1 :: Int -- error unless we've hidden lex.



-- * Qualified names

-- Function/operator names
myfilter  x = Prelude.filter x  -- argument added to avoid monomorphism restn
mycompose = (Prelude..)

-- Use of module synonyms
myfilter2 p = P.filter p

-- Method names
myplus :: Num a => a -> a -> a
myplus = (Prelude.+) 

-- Tycons
myminus = (Prelude.-) :: Prelude.Int -> Prelude.Int -> Prelude.Int

-- Type synonyms
foo :: P.ShowS
foo = foo

-- Class names in instances
instance P.Num P.Bool where
  (+) = (P.||)
  (*) = (P.&&)
  negate = P.not

instance (P.Num a, P.Num b) => P.Num (a,b) where
  x + y = (fst x + fst y, snd x + snd y)

-- Constructor names in expressions

-- this used to break tidyInfix in parser.y
-- Note that P.[] is _not_ legal!
testInfixQualifiedCon = 'a' P.: [] :: String

-- Constructor names in patterns
f (P.Just x)  = True
f (P.Nothing) = False

g (x P.: xs) = x

y P.: ys = ['a'..]

-- * Support for octal and hexadecimal numbers
--   Note: 0xff and 0xFF are legal but 0Xff and 0XFF are not.
--   ToDo: negative tests to make sure invalid numbers are excluded.

d = (  -1,  -0,  0,  1)    :: (Int,Int,Int,Int)
o = (-0o1,-0o0,0o0,0o1)    :: (Int,Int,Int,Int)
x = (-0x1,-0x0,0x0,0x1)    :: (Int,Int,Int,Int)
x' = (0xff,0xFf,0xfF,0xFF) :: (Int,Int,Int,Int)

-- * No renaming or interface files
--   We test that "interface", "renaming" and "to" are not reserved.

interface = 1  :: Int
renaming  = 42 :: Int
to        = 2  :: Int

