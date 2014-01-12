-- test reification of data declarations

{-# LANGUAGE TypeFamilies #-}
module TH_reifyDecl1 where

import System.IO
import Language.Haskell.TH
import Text.PrettyPrint.HughesPJ

infixl 3 `m1`

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
class C1 a where
  m1 :: a -> Int

-- class with instances
class C2 a where
  m2 :: a -> Int
instance C2 Int where
  m2 x = x

-- associated types
class C3 a where
  type AT1 a
  data AT2 a

instance C3 Int where
  type AT1 Int = Bool
  data AT2 Int = AT2Int

-- type family
type family TF1 a

-- type family, with instances
type family TF2 a
type instance TF2 Bool = Bool

-- data family
data family DF1 a

-- data family, with instances
data family DF2 a
data instance DF2 Bool = DBool

$(return [])

test :: ()
test = $(let 
	  display :: Name -> Q ()
	  display q = do { i <- reify q; runIO $ hPutStrLn stderr (pprint i) }
	in do { display ''T
	      ; display ''R
	      ; display ''List
	      ; display ''Tree
	      ; display ''IntList
	      ; display ''Length
	      ; display 'Leaf
	      ; display 'm1
	      ; display ''C1
	      ; display ''C2
	      ; display ''C3
	      ; display ''AT1
	      ; display ''AT2
	      ; display ''TF1
	      ; display ''TF2
	      ; display ''DF1
	      ; display ''DF2
	      ; [| () |] })


