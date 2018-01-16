{-
    Haskell version of ...

! Lisp-like functions which allow easy hand translation from Lisp to Hope+ 
! Started by Tony Kitto on 30th March 1988 
! Changes Log 
! 18-05-88 added LUT functions and regularized assoc
! 25-05-88 added Lisptochar and DisplayLUT

Haskell version::

    23-06-93 JSM initial version

-}

module Lisplikefns (
    Token, Lisplist(..), LUT, 
    mkLisplist, strToToken, tv,
    atom, car, cdr, cadr, caddr, cadddr, assoc,
    newLUT, addtoLUT, getLUT
)

where

type Token = String -- "(" or ")" or "Lisp Symbol"

data Lisplist = Nil | Atom Token | Cons (Lisplist, Lisplist) deriving (Eq,Show{-was:Text-})

-- These functions create a Lisplist from a list of characters 

mkLisplist :: [Token] -> Lisplist
mkLisplist ("(":t) = if r /= [] then Nil else l
		     where (r, l) = sublist t
mkLisplist _       = Nil

sublist :: [Token] -> ([Token], Lisplist)
sublist []      = ([], Nil)
sublist ("(":t) = (r2, Cons (l1, l2))
		  where (r1, l1) = sublist t
		        (r2, l2) = sublist r1
sublist (")":t) = (t, Nil)
sublist (h:t)   = (r, Cons (Atom h, l))
		  where (r, l) = sublist t

strToToken :: String -> [Token]
strToToken "" = []
strToToken s  = a : strToToken b
		where (a, b) = getToken s
                         
getToken :: String -> (Token, String)
getToken ""                           = ([], "")
getToken (h:t) | h == ' '             = getToken t
	       | h == '(' || h == ')' = ([h], t)
	       | otherwise            = (h:a, b)
		 where (a, b) = restOfToken t

restOfToken :: String -> (Token, String)
restOfToken ""                                       = ([], "")
restOfToken (h:t) | h == '(' || h == ')' || h == ' ' = ([], h:t)
		  | otherwise                        = (h:a, b)
		    where (a, b) = restOfToken t

tv :: Lisplist -> Token
tv (Atom x) = x
tv _ 	    = error "Not an atom"


-- These functions provide simple Lisplist operations

atom :: Lisplist -> Bool
atom (Atom x) = True
atom _ 	      = False

car :: Lisplist -> Lisplist
car (Cons (x, y)) = x
car _ 	    	  = Nil

cdr :: Lisplist -> Lisplist
cdr (Cons (x, y)) = y
cdr _ 	    	  = Nil

cadr :: Lisplist -> Lisplist
cadr = car . cdr

caddr :: Lisplist -> Lisplist
caddr = car . cdr . cdr

cadddr :: Lisplist -> Lisplist
cadddr = car . cdr . cdr . cdr

assoc :: (Lisplist, Lisplist) -> Lisplist
assoc (term, Cons (x, y)) = case x of
    Cons (head@(Atom key), rest) | term == head -> x 
    	    	    	    	 | otherwise -> assoc (term, y)
    _ -> Nil
assoc (_, _) 	    	  = Nil

{-
  These functions provide more complex operations based on a Lisp-like       
  functionality, they do not exactly match the equivalent Lisp functions
-}

type LUTentry = (Token, [Lisplist] )
data LUT = Empty | Node (LUT, LUTentry, LUT) deriving (Show{-was:Text-})


newLUT :: LUT
newLUT = Empty

addtoLUT :: (Token, Lisplist, LUT) -> LUT
addtoLUT (k, l, Empty) = Node (Empty, (k, [l]), Empty)
addtoLUT (k, l, Node (left, (k1, kl), right)) 
    | k == k1   = Node (left, (k1, l:kl), right)
    | k <  k1   = Node (addtoLUT (k, l, left), (k1, kl), right)
    | otherwise = Node (left, (k1, kl), addtoLUT (k, l, right))

getLUT :: (Token, LUT) -> [Lisplist]
getLUT (t, Empty) = []
getLUT (t, Node (left, (k, kl), right))
    | t == k    = kl
    | t <  k    = getLUT (t, left)
    | otherwise = getLUT (t, right)

