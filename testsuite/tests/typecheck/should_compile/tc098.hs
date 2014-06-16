-- !!! Ambiguity in local declarations

module ShouldSucceed where

type Cp a =  a -> a -> Ordering

m :: Eq a => Cp a -> [a] -> a
m            _       [x,y,z] =  if x==y then x else z  
     
cpPairs :: Cp [j] -> (a,[j]) -> (a,[j]) -> Ordering
cpPairs    cp        (_,p)      (_,q)   =  cp p q

mp :: (Eq i,Eq j) => Cp [j] -> [(i,[j])] -> (i,[j])
mp                   cp        dD          =  
                                    let  minInRow = m (cpPairs cp)
                                    in   minInRow dD

{- GHC 3.02 reported

    T.hs:24:
	Ambiguous type variable(s)
	`j' in the constraint `Eq (aYD, [j])'
	    arising from use of `m' at T.hs:24
	In an equation for function `mp':
	    mp cp dD = let minInRow = m (cpPairs cp) in minInRow dD

This was because the ambiguity test in tcSimplify didn't
take account of the type variables free in the environment.

It should compile fine.
-}
