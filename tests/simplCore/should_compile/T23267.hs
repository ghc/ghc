module T23267 where

data N = Z | S N

union :: N -> ()
union Z = ()
union t = splitS t

splitS :: N -> ()
splitS Z = ()
splitS (S l) = splitS l

{- Results in this error:

*** Core Lint errors : in result of SpecConstr ***
T23267.hs:10:1: warning:
    Out of scope: l_aBE :: N
                  [LclId]
    In the RHS of $ssplitS_sJx :: N -> ()
    In the body of lambda with binder sc_sJw :: N
    Substitution: <InScope = {}
                   IdSubst   = []
                   TvSubst   = []
                   CvSubst   = []>
-}
