{-# OPTIONS -Wall #-}

{- This module contains a few primitive types that need to be wired in.
   Most are defined in PrimEnv, which is automatically generated from
   GHC's primops.txt. -}

module Prims(initialEnv, primEnv) where

import Core
import Env
import Check
import PrimCoercions

import PrimEnv

initialEnv :: Menv
initialEnv = efromlist [(primMname,primEnv),
		     (errMname,errorEnv)]

primEnv :: Envs
-- Tediously, we add defs for ByteArray# etc. because these are
-- declared as ByteArr# (etc.) in primops.txt, and GHC has
-- ByteArray# etc. wired-in.
-- At least this is better than when all primops were wired-in here.
primEnv = Envs {tcenv_=efromlist $ map (\ (t,k) -> (t,Kind k)) $ 
                  [(snd tcByteArrayzh,ktByteArrayzh), 
                   (snd tcMutableArrayzh, ktMutableArrayzh),
                   (snd tcMutableByteArrayzh, ktMutableByteArrayzh)] ++
                 ([(snd $ tcUtuple n, ktUtuple n) | n <- [1..maxUtuple]] 
                   ++ ((snd tcArrow,ktArrow):primTcs)),
		tsenv_=eempty,
		cenv_=efromlist primDcs,
		venv_=efromlist (opsState ++ primVals)}

errorEnv :: Envs
errorEnv = Envs {tcenv_=eempty,
		 tsenv_=eempty,
		 cenv_=eempty,
		 venv_=efromlist errorVals}


primDcs :: [(Dcon,Ty)]
primDcs = map (\ ((_,c),t) -> (c,t))
	      [(dcUtuple n, dcUtupleTy n) | n <- [1..maxUtuple]]

tRWS :: Ty
tRWS = tStatezh tRealWorld

opsState :: [(Var, Ty)]
opsState = [
  ("realWorldzh", tRWS)]

{- Arrays -}

tcByteArrayzh, tcMutableArrayzh, tcMutableByteArrayzh :: Qual Tcon
ktByteArrayzh, ktMutableArrayzh, ktMutableByteArrayzh :: Kind

tcByteArrayzh = pvz "ByteArray"
ktByteArrayzh = Kunlifted

tcMutableArrayzh = pvz "MutableArray"
ktMutableArrayzh = Karrow Klifted (Karrow Klifted Kunlifted)

tcMutableByteArrayzh = pvz "MutableByteArray"
ktMutableByteArrayzh = Karrow Klifted Kunlifted

{- Real world and state. -}

-- tjc: why isn't this one unboxed?
tcRealWorld :: Qual Tcon
tcRealWorld = pv "RealWorld"
tRealWorld :: Ty
tRealWorld = Tcon tcRealWorld

tcStatezh :: Qual Tcon
tcStatezh = pvz "State"
tStatezh :: Ty -> Ty
tStatezh t = Tapp (Tcon tcStatezh) t

{- Properly defined in PrelError, but needed in many modules before that. -}
errorVals :: [(Var, Ty)]
errorVals = [
 ("error", Tforall ("a",Kopen) (tArrow tString (Tvar "a"))),
 ("irrefutPatError", str2A),
 ("patError", str2A),
 ("divZZeroError", forallAA),
 ("overflowError", forallAA)]

{- Non-primitive, but mentioned in the types of primitives. -}

bv :: a -> Qual a
bv = qual baseMname

str2A, forallAA :: Ty  
str2A = Tforall ("a",Kopen) (tArrow tAddrzh (Tvar "a"))
forallAA = Tforall ("a",Kopen) (Tvar "a")

tcChar :: Qual Tcon
tcChar = bv "Char"
tChar :: Ty
tChar = Tcon tcChar
tcList :: Qual Tcon
tcList = bv "ZMZN"
tList :: Ty -> Ty
tList t = Tapp (Tcon tcList) t
tString :: Ty
tString = tList tChar
