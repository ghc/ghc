-- test reification of data declarations

module TH_reifyDecl1
where

import Language.Haskell.THSyntax

-- simple
data T = A | B

decl_T :: DecQ
decl_T = reifyDecl T

-- parametric
data R a = C a | D

decl_R :: DecQ
decl_R = reifyDecl R

-- recursive
data List a = Nil | Cons a (List a)

decl_List :: DecQ
decl_List = reifyDecl List

-- infix operator
data Tree a = Leaf | Tree a :+: Tree a

decl_Tree :: DecQ
decl_Tree = reifyDecl Tree
