{- Environments.
   Uses lists for simplicity and to make the semantics clear.
   A real implementation should use balanced trees or hash tables.
-}

module Env (Env,
	    eempty,
	    elookup,
	    eextend,
            edomain,
	    efromlist,
	    efilter,
	    eremove)
where

import List

data Env a b = Env [(a,b)] 
 deriving (Show)

eempty :: Env a b 
eempty = Env []

{- In case of duplicates, returns most recently added entry. -}
elookup :: (Eq a) => Env a b -> a -> Maybe b
elookup (Env l) k = lookup k l 

{- May hide existing entries. -}
eextend :: Env a b -> (a,b) -> Env a b
eextend (Env l) (k,d) = Env ((k,d):l)

edomain :: (Eq a) => Env a b -> [a]
edomain (Env l) = nub (map fst l)

{- In case of duplicates, first entry hides others. -}
efromlist :: [(a,b)] -> Env a b
efromlist l = Env l

eremove :: (Eq a)  => Env a b -> a -> Env a b
eremove (Env l) k = Env (filter ((/= k).fst) l)

efilter :: Env a b -> (a -> Bool) -> Env a b
efilter (Env l) p = Env (filter (p.fst) l)

