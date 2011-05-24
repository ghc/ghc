-- !!! this module supposedly includes one of each Haskell construct

-- HsImpExp stuff

module OneOfEverything (
	fixn, 
	FooData,
	FooDataB(..),
	FooDataC( .. ),
	Tree(Leaf, Branch),
	EqClass(..),
	OrdClass(orda, ordb),
	module OneC ,
	module OneOfEverything
    ) where

import Prelude
import System.IO ( putStr )
import System.Environment hiding ( getArgs )
import Control.Monad

-- HsDecls stuff

infix	6 `fixn`
infixl	7 +#
infixr	8 `fixr`

fixn x y = x
fixl x y = x
fixr x y = x

type Pair a b = (a, b)

data FooData = FooCon Int

data FooDataB = FooConB Double

data Tree a = Leaf a | Branch (Leaf a) (Leaf a)

class (Eq a) => EqClass a where
    eqc :: a -> Char
    eqc x = '?'

class (Ord a) => OrdClass a where
    orda :: a -> Char
    ordb :: a -> Char
    ordc :: a -> Char

instance (Eq a) => EqClass (Tree a) where
    eqc x = 'a'

default (Integer, Rational)

-- HsBinds stuff

singlebind x = x

bindwith :: (OrdClass a, OrdClass b) => a -> b -> b
bindwith a b = b

reca a = recb a
recb a = reca a

(~(a,b,c)) | nullity b	= a
	   | nullity c	= a
	   | otherwise	= a
	   where
	    nullity = null

-- HsMatches stuff

mat a b c d | foof a b = d
	    | foof a c = d
	    | foof b c = d
	    where
		foof a b = a == b

-- HsExpr stuff
expr a b c d
  = a
  + (:) a b
  + (a : b)
  + (1 - 'c' - "abc" - 1.293)
  + ( \ x y z -> x ) 42
  + (9 *)
  + (* 8)
  + (case x of
	[] | null x	-> 99
	   | otherwise	-> 98
	   | True	-> 97
	   where
	     null x = False
    )
  + [ z | z <- c, isSpace z ]
  + let y = foo
    in  y
  + [1,2,3,4]
  + (4,3,2,1)
  + (4 :: Num a => a)
  + (if 42 == 42.0 then 1 else 4)
  + [1..]
  + [2,4..]
  + [3..5]
  + [4,8..999]

-- HsPat stuff
f _ x 1 1.93 'c' "dog" ~y z@(Foo a b) (c `Bar` d) [1,2] (3,4) = y

-- HsLit stuff -- done above

-- HsTypes stuff
g :: (Num a, Eq b) => Foo a -> [b] -> (a,a,a) -> b
g x y z = head y
