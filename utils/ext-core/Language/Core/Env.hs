{- Environments.
  The original version used lists. I changed it to use Data.Map.
  Sadly it doesn't seem to matter much. --tjc
-}

module Language.Core.Env (Env,
	    eempty,
	    elookup,
	    eextend,
            edomain,
	    efromlist,
            etolist,
	    efilter,
	    eremove)
where

import qualified Data.Map as M

data Env a b = Env (M.Map a b)
 deriving Show

eempty :: Env a b 
eempty = Env M.empty

{- In case of duplicates, returns most recently added entry. -}
elookup :: (Eq a, Ord a) => Env a b -> a -> Maybe b
elookup (Env l) k = M.lookup k l 

{- May hide existing entries. -}
eextend :: Ord a => Env a b -> (a,b) -> Env a b
eextend (Env l) (k,d) = Env (M.insert k d l)

edomain :: (Eq a) => Env a b -> [a]
edomain (Env l) = M.keys l

{- In case of duplicates, first entry hides others. -}
efromlist :: Ord a => [(a,b)] -> Env a b
efromlist = Env . M.fromList

etolist :: Env a b -> [(a,b)]
etolist (Env l) = M.toList l

eremove :: (Eq a, Ord a)  => Env a b -> a -> Env a b
eremove (Env l) k = Env (M.delete k l)

efilter :: Ord a => Env a b -> (a -> Bool) -> Env a b
efilter (Env l) p = Env (M.filterWithKey (\ k _ -> p k) l)

