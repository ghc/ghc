-- test reification of data declarations

module TH_reifyDecl1 where

import Language.Haskell.TH
import Text.PrettyPrint.HughesPJ

infixl 3 `m`

-- simple
data T = A | B

-- parametric
data R a = C a | D

-- recursive
data List a = Nil | Cons a (List a)

-- infix operator
data Tree a = Leaf | Tree a :+: Tree a

-- type declaration
type IntList = [Int]

-- newtype declaration
newtype Length = Length Int

-- simple class
class C a where
  m :: a -> Int

test :: ()
test = $(let 
	  display :: Name -> Q ()
	  display q = do { i <- reify q; report False (pprint i) }
	in do { display ''T
	      ; display ''R
	      ; display ''List
	      ; display ''Tree
	      ; display ''IntList
	      ; display ''Length
	      ; display 'Leaf
	      ; display 'm
	      ; [| () |] })


