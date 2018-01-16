-----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Copyright   :  (c) Simon Marlow 2002
-- License     :  BSD-style
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module illustrates & tests most of the features of Haddock.
-- Testing references from the description: 'T', 'f', 'g', 'Visible.visible'.
--
-----------------------------------------------------------------------------

-- This is plain comment, ignored by Haddock.
{-# LANGUAGE Rank2Types, GADTs #-}
module Test (

	-- Section headings are introduced with '-- *':
	-- * Type declarations

	-- Subsection headings are introduced with '-- **' and so on.
	-- ** Data types
	T(..), T2, T3(..), T4(..), T5(..), T6(..),
	N1(..), N2(..), N3(..), N4, N5(..), N6(..), N7(..),

	-- ** Records
	R(..), R1(..),

	-- | test that we can export record selectors on their own:
	p, q, u,

	-- * Class declarations
	C(a,b), D(..), E, F(..),

	-- | Test that we can export a class method on its own:
	a,

	-- * Function types
	f, g,

	-- * Auxiliary stuff

	-- $aux1

	-- $aux2

	-- $aux3

	-- $aux4

	-- $aux5

	-- $aux6

	-- $aux7

	-- $aux8

	-- $aux9

	-- $aux10

	-- $aux11

	-- $aux12

	-- | This is some inline documentation in the export list
	--
	-- > a code block using bird-tracks
	-- > each line must begin with > (which isn't significant unless it
	-- > is at the beginning of the line).

	-- * A hidden module
	module Hidden,

	-- * A visible module
	module Visible,

	{-| nested-style doc comments -}

	-- * Existential \/ Universal types
	Ex(..),

	-- * Type signatures with argument docs
	k, l, m, o,

	-- * A section
	-- and without an intervening comma:
	-- ** A subsection

{-|
 > a literal line

 $ a non /literal/ line $
-}

	f',

  withType, withoutType
   ) where

import Hidden
import Visible
import Data.Maybe

bla = Nothing

-- | This comment applies to the /following/ declaration
-- and it continues until the next non-comment line
data T a b
 = A Int (Maybe Float) -- ^ This comment describes the 'A' constructor
 | -- | This comment describes the 'B' constructor
   B (T a b, T Int Float) -- ^

-- | An abstract data declaration
data T2 a b = T2 a b

-- | A data declaration with no documentation annotations on the constructors
data T3 a b = A1 a | B1 b

-- A data declaration with no documentation annotations at all
data T4 a b = A2 a | B2 b

-- A data declaration documentation on the constructors only
data T5 a b
  = A3 a -- ^ documents 'A3'
  | B3 b -- ^ documents 'B3'

-- | Testing alternative comment styles
data T6
  -- | This is the doc for 'A4'
  = A4
  | B4
  | -- ^ This is the doc for 'B4'

    -- | This is the doc for 'C4'
    C4

-- | A newtype
newtype N1 a = N1 a

-- | A newtype with a fieldname
newtype N2 a b = N2 {n :: a b}

-- | A newtype with a fieldname, documentation on the field
newtype N3 a b = N3 {n3 :: a b -- ^ this is the 'n3' field
		    }

-- | An abstract newtype - we show this one as data rather than newtype because
-- the difference isn\'t visible to the programmer for an abstract type.
newtype N4 a b = N4 a

newtype N5 a b = N5 {n5 :: a b -- ^ no docs on the datatype or the constructor
		    }

newtype N6 a b = N6 {n6 :: a b
		    }
		 -- ^ docs on the constructor only

-- | docs on the newtype and the constructor
newtype N7 a b = N7 {n7 :: a b
		    }
		-- ^ The 'N7' constructor


class (D a) => C a  where
   -- |this is a description of the 'a' method
   a :: IO a
   b :: [a]
   -- ^ this is a description of the 'b' method
   c :: a -- c is hidden in the export list
   c = undefined

-- ^ This comment applies to the /previous/ declaration (the 'C' class)

class D a where
   d :: T a b
   e :: (a,a)
-- ^ This is a class declaration with no separate docs for the methods

instance D Int where
  d = undefined
  e = undefined

-- instance with a qualified class name
instance Test.D Float where
  d = undefined
  e = undefined

class E a where
  ee :: a
-- ^ This is a class declaration with no methods (or no methods exported)

-- This is a class declaration with no documentation at all
class F a where
  ff :: a

-- | This is the documentation for the 'R' record, which has four fields,
-- 'p', 'q', 'r', and 's'.
data R =
  -- | This is the 'C1' record constructor, with the following fields:
  C1 { p :: Int -- ^ This comment applies to the 'p' field
     , q :: forall a . a->a  -- ^ This comment applies to the 'q' field
     , -- | This comment applies to both 'r' and 's'
       r,s :: Int
     }
  | C2 { t :: T1 -> (T2 Int Int)-> (T3 Bool Bool) -> (T4 Float Float) -> T5 () (),
       u,v :: Int
     }
  -- ^ This is the 'C2' record constructor, also with some fields:

-- | Testing different record commenting styles
data R1
  -- | This is the 'C3' record constructor
  = C3 {
	-- | The 's1' record selector
	  s1 :: Int
	-- | The 's2' record selector
	, s2 :: Int
	, s3 :: Int  -- NOTE: In the original examples/Test.hs in Haddock, there is an extra "," here.
                     -- Since GHC doesn't allow that, I have removed it in this file.
	-- ^ The 's3' record selector
     }

-- These section headers are only used when there is no export list to
-- give the structure of the documentation:

-- * This is a section header (level 1)
-- ** This is a section header (level 2)
-- *** This is a section header (level 3)

{-|
In a comment string we can refer to identifiers in scope with
single quotes like this: 'T', and we can refer to modules by
using double quotes: "Foo".  We can add emphasis /like this/.

   * This is a bulleted list

   - This is the next item (different kind of bullet)

   (1) This is an ordered list

   2. This is the next item (different kind of bullet)

   [cat] a small, furry, domesticated mammal

   [pineapple] a fruit grown in the tropics

@
     This is a block of code, which can include other markup: 'R'
     formatting
               is
                 significant
@

> this is another block of code

We can also include URLs in documentation: <http://www.haskell.org/>.
-}

f :: C a => a -> Int

-- | we can export foreign declarations too
foreign import ccall g :: Int -> IO CInt

-- | this doc string has a parse error in it: \'
h :: Int
h = 42


-- $aux1 This is some documentation that is attached to a name ($aux1)
-- rather than a source declaration.  The documentation may be
-- referred to in the export list using its name.
--
-- @ code block in named doc @

-- $aux2 This is some documentation that is attached to a name ($aux2)

-- $aux3
-- @ code block on its own in named doc @

-- $aux4
--
-- @ code block on its own in named doc (after newline) @

{- $aux5 a nested, named doc comment

   with a paragraph,

   @ and a code block @
-}

-- some tests for various arrangements of code blocks:

{- $aux6
>test
>test1

@ test2
  test3
@
-}

{- $aux7
@
test1
test2
@
-}

{- $aux8
>test3
>test4
-}

{- $aux9
@
test1
test2
@

>test3
>test4
-}

{- $aux10
>test3
>test4

@
test1
test2
@
-}

-- This one is currently wrong (Haddock 0.4).  The @...@ part is
-- interpreted as part of the bird-tracked code block.
{- $aux11
aux11:

>test3
>test4

@
test1
test2
@
-}

-- $aux12
-- > foo
--
-- > bar
--

-- | A data-type using existential\/universal types
data Ex a
  = forall b . C b => Ex1 b
  | forall b . Ex2 b
  | forall b . C a => Ex3 b -- NOTE: I have added "forall b" here make GHC accept this file
  | Ex4 (forall a . a -> a)

-- | This is a function with documentation for each argument
k :: T () () 	  -- ^ This argument has type 'T'
  -> (T2 Int Int) -- ^ This argument has type 'T2 Int Int'
  -> (T3 Bool Bool -> T4 Float Float) -- ^ This argument has type @T3 Bool Bool -> T4 Float Float@
  -> T5 () ()	  -- ^ This argument has a very long description that should
		  -- hopefully cause some wrapping to happen when it is finally
		  -- rendered by Haddock in the generated HTML page.
  -> IO ()	  -- ^ This is the result type

-- This function has arg docs but no docs for the function itself
l :: (Int, Int, Float) -- ^ takes a triple
  -> Int -- ^ returns an 'Int'

-- | This function has some arg docs
m :: R
  -> N1 ()	-- ^ one of the arguments
  -> IO Int	-- ^ and the return value

-- | This function has some arg docs but not a return value doc

-- can't use the original name ('n') with GHC
newn :: R		-- ^ one of the arguments, an 'R'
     -> N1 ()		-- ^ one of the arguments
     -> IO Int
newn = undefined


-- | A foreign import with argument docs
foreign import ccall unsafe
 o :: Float  -- ^ The input float
   -> IO Float  -- ^ The output float

-- | We should be able to escape this: \#\#\#

-- p :: Int
-- can't use the above original definition with GHC
newp :: Int
newp = undefined

-- | a function with a prime can be referred to as 'f''
-- but f' doesn't get link'd 'f\''
f' :: Int

-- | Comment on a definition without type signature
withoutType = undefined

-- | Comment on a definition with type signature
withType :: Int
withType = 1

-- Add some definitions here so that this file can be compiled with GHC

data T1
f = undefined
f' = undefined
type CInt = Int
k = undefined
l = undefined
m = undefined
