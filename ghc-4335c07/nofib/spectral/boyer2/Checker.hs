{-
    Haskell version of ...

! The Checker module for the Boyer benchmark 
! Started by Tony Kitto on March 30th 1988 

! Changes Log                                              
! 07-04-88 ADK Tautp function removed to main body
! 08-04-88 ADK bug fix = Truep returns True for (t) 
!                        Falsep returns True for (f)  

Haskell version::

    23-06-93 JSM initial version

-}

module Checker (tautologyp) where

import Lisplikefns

tautologyp :: (Lisplist, Lisplist, Lisplist) -> Bool
tautologyp (Nil, _, _) = False
tautologyp (term@(Atom x), truelst, _) = truep (term, truelst)
tautologyp (term@(Cons (x, y)), truelst, falselst) = 
    if truep (term, truelst) then True
    else if falsep (term, falselst) then False
    else case x of
    	Atom "if" -> if truep (car y, truelst) then
                     	tautologyp (cadr y, truelst, falselst)
                     else if falsep (car y, falselst) then 
                     	tautologyp (caddr y, truelst, falselst)
                     else
                        (tautologyp (cadr y, Cons (car y, truelst), falselst)) &&
                        (tautologyp (caddr y, truelst, Cons (car y, falselst)))
    	_         -> False

truep :: (Lisplist, Lisplist) -> Bool
truep (Nil, _) = False
truep (Cons (Atom "t", Nil), _) = True
truep (term, l) = lispmember (term, l)

falsep :: (Lisplist, Lisplist) -> Bool
falsep (Nil, _) = False
falsep (Cons (Atom "f", Nil), _) = True
falsep (term, l) = lispmember (term, l)

lispmember :: (Lisplist, Lisplist) -> Bool
lispmember (e, Cons (x, xs)) | e == x    = True
                             | otherwise = lispmember (e, xs)

lispmember (_, _) = False


