{-
Copyright(C) 1999 Erik Meijer
-}
module Pretty where

{-

Quick reference for the simple Pretty-print Combinators

  |---|     |----|   |-------|
  |koe| <|> |beer| = |koebeer|
  |---|     |----|   |-------|

  |---|     |----|   |--------|
  |koe| <+> |beer| = |koe beer|
  |---|     |----|   |--------|

  |---|     |----|   |----|
  |koe| <-> |beer| = |koe |
  |---|     |----|   |beer|
                     |----|

  |---|            |----|   |-------|
  |koe| <|> nest 2 |beer| = |koebeer|
  |---|            |----|   |-------|

  |---|            |----|   |------|
  |koe| <-> nest 2 |beer| = |koe   |
  |---|            |----|   |  beer|
                            |------|
                            
  empty =                            
-}

{-

Extremely simplified version of John Hughes' combinators, 
without (sep), but with (empty).

TODO: use Okasaki-style catenable dequeues to represent Doc

(c) Erik Meijer and Arjan van IJzendoorn

October 199

-}

infixl 7 <+>
infixl 6 <|>
infixr 5 <->
  
instance Show Doc where
  { showsPrec = showsPrecDoc }

showsPrecDoc i = \d ->
  case d of
    { Empty -> id
    ; Doc ds -> layout ds
    }
 
data Doc
  = Doc [(Int,ShowS)]
  | Empty
  
layout :: [(Int,ShowS)] -> ShowS
layout = \ds ->
  case ds of
    { []       -> showString ""
    ; [(n,s)]  -> indent n.s
    ; (n,s):ds -> indent n.s.showString "\n".layout ds
    }

width :: Doc -> Int
width = \d ->
  case d of
    { Empty -> 0
    ; Doc ds -> maximum [ i + length (s "") | (i,s) <- ds ]
    }
  
text :: String -> Doc
text = \s -> Doc [(0,showString s)]

nest :: Int -> Doc -> Doc
nest n = \d ->
  case d of
    { Empty -> Empty
    ; Doc ds -> Doc [ (i+n,d) | (i,d) <- ds ]
    }

(<->) :: Doc -> Doc -> Doc
Empty <-> Empty = Empty
Empty <-> (Doc d2) = Doc d2
(Doc d1) <-> Empty = Doc d1
(Doc d1) <-> (Doc d2) = Doc (d1++d2)

(<+>) :: Doc -> Doc -> Doc
a <+> b = a <|> (text " ") <|> b

(<|>) :: Doc -> Doc -> Doc
Empty <|> Empty = Empty
Empty <|> (Doc d2) = Doc d2
(Doc d1) <|> Empty = Doc d1
(Doc d1) <|> (Doc d2) =
  let 
    { (d,(i,s)) = (init d1,last d1)
    ; ((j,t),e) = (head d2,tail d2)
    }
  in
    (    Doc d 
     <-> Doc [(i,s.t)] 
     <-> nest (i + length (s "") - j) (Doc e)
    )
    
-- Derived operations

empty :: Doc
empty = Empty

{-

horizontal s [a,b,c] =
  a <|> (s <|> b) <|> (s <|> c)

-}

horizontal :: Doc -> [Doc] -> Doc
horizontal s = \ds ->
  case ds of
    { [] -> empty
    ; ds -> foldr1 (\d -> \ds -> d <|> s <|> ds) ds
    }

{-

vertical s [a,b,c] =
  a
  <->
  (s <|> b)
  <->
  (s <|> c)

-}

vertical :: [Doc] -> Doc
vertical = \ds ->
  case ds of
    { [] -> empty
    ; d:ds -> d <-> vertical ds
    }

block (o,s,c) = \ds ->
  case ds of
    { [] -> o<|>c
    ; [d] -> o<|>d<|>c    ; d:ds -> (vertical ((o <|> d):[s <|> d | d <- ds ])) <-> c
    }
    
-- Helper function

indent :: Int -> ShowS
indent = \n ->
  showString [ ' ' | i <- [1..n] ]
