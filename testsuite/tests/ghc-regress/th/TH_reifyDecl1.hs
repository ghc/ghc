-- test reification of data declarations

module TH_reifyDecl1
where

import Language.Haskell.THSyntax

-- simple
data T = A | B

decl_T :: Decl
decl_T = reifyDecl T

-- parametric
data R a = C a | D

decl_R :: Decl
decl_R = reifyDecl R

-- recursive
data List a = Nil | Cons a (List a)

decl_List :: Decl
decl_List = reifyDecl List

-- infix operator
data Tree a = Leaf | Tree a :+: Tree a

decl_Tree :: Decl
decl_Tree = reifyDecl Tree
