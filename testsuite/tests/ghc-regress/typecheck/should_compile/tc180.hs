{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances #-}

-- This tests an aspect of functional dependencies, revealing a bug in GHC 6.0.1
-- discovered by Martin Sulzmann


module ShouldCompile where

data PHI = PHI 
data EMPT = EMPT 
data LAB l a = LAB l a 
data Phi = Phi 

data A = A 
data A_H = A_H [Char] 


class LNFyV r1 r2 h1 h2 | r1 -> r2, r1 r2 -> h1 h2 where
    lnfyv :: r1->r2->h1->h2

instance ( REtoHT (LAB l c) h) 
	=> LNFyV (LAB l c) ((LAB l c),EMPT) h (h,[Phi]) where -- (L2)
    lnfyv = error "urk"

class REtoHT s t | s->t 
instance REtoHT (LAB A [Char]) A_H    -- (R4)

foo = lnfyv (LAB A "") ((LAB A ""),EMPT) (A_H "1")


{-
ghci 6.0.1

*Test> :t (lnfyv (LAB A "") ((LAB A ""),EMPT) (A_H "1") )

No instance for (LNFyV (LAB A [Char])
                       (LAB A [Char], EMPT)
                       A_H
                       (h, [Phi]))
  arising from use of `lnfyv' at <No locn>


hugs November 2002

Test> :t (lnfyv (LAB A "") ((LAB A ""),EMPT) (A_H "1"))
lnfyv (LAB A "") (LAB A "",EMPT) (A_H "1") :: (A_H,[Phi])


hugs is right, here's why


(lnfyv (LAB A "") ((LAB A ""),EMPT) (A_H "1")) yields


                  LNFyV (LAB A Char) ((LAB A Char),EMPT) (A_H) c

improve by (L2)   LNFyV (LAB A Char) ((LAB A Char),EMPT) (A_H) (A_H,[Phi]), c=(A_H,[Phi])
reduce by (L2)    REtoHT (LAB A Char) A_H, c=(A_H,[Phi])
reduce by (R4)    c=(A_H,[Phi])


-}
