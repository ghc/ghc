{-
    Haskell version of ...

! rewrite functions for Boyer benchmark 
! Started by Tony Kitto on 30 March 1988

! Changes Log
! 08-04-88 ADK bug fix  = rewrite(Atom(x),A) returns Atom(x) not Nil
! 25-05-88 ADK added applysubst to this module and assoclist replaced by LUT

Haskell version:

    23-06-93 JSM initial version

-}

module Rewritefns (applysubst, rewrite) where

import Lisplikefns

applysubst :: Lisplist -> Lisplist -> Lisplist
applysubst alist Nil           = Nil
applysubst alist term@(Atom x) = 
    case assoc (term, alist) of 
    	Cons (yh, yt) -> yt
    	_             -> term
applysubst alist (Cons (x, y)) = Cons (x, applysubstlst alist y)

applysubstlst :: Lisplist -> Lisplist -> Lisplist
applysubstlst alist Nil           = Nil
applysubstlst alist (Atom x)      = error "Malformed list"
applysubstlst alist (Cons (x, y)) = 
    Cons (applysubst alist x, applysubstlst alist y)


rewrite :: Lisplist -> LUT -> Lisplist
rewrite Nil term             = Nil
rewrite expr@(Atom x) term   = expr
rewrite (Cons (l1, l2)) term = 
    rewritewithlemmas (Cons (l1, rewriteargs l2 term)) 
	(getLUT (tv l1, term)) term

rewriteargs :: Lisplist -> LUT -> Lisplist
rewriteargs Nil term           = Nil
rewriteargs (Atom x) term      = error "Malformed list"
rewriteargs (Cons (x, y)) term = Cons (rewrite x term, rewriteargs y term)

rewritewithlemmas :: Lisplist -> [Lisplist] -> LUT -> Lisplist
rewritewithlemmas t [] term = t
rewritewithlemmas t (lh:lt) term 
    | b = rewrite (applysubst u (caddr lh)) term
    | otherwise = rewritewithlemmas t lt term
      where (b, u) = onewayunify t (cadr lh)


onewayunify :: Lisplist -> Lisplist -> (Bool, Lisplist)
onewayunify t1 t2 = onewayunify1 t1 t2 Nil

onewayunify1 :: Lisplist -> Lisplist -> Lisplist -> (Bool, Lisplist)
onewayunify1 t1 t2 u | atom t2          = case assoc (t2, u) of
		            Cons (x, y) -> (t1 == y, u)
   	    		    _           -> (True, Cons (Cons (t2, t1), u))
                     | atom t1          = (False, u)
    		     | car t1 == car t2 = onewayunify1lst (cdr t1) (cdr t2) u
		     | otherwise        = (False, u)

onewayunify1lst :: Lisplist -> Lisplist -> Lisplist -> (Bool, Lisplist)
onewayunify1lst Nil _  u = (True, u)
onewayunify1lst l1  l2 u 
    | b = onewayunify1lst (cdr l1) (cdr l2) u1
    | otherwise = (False, u1)
      where (b, u1) = onewayunify1 (car l1) (car l2) u

