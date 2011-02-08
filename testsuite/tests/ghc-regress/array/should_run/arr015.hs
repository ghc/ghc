-- !!! Array test
-- This one fails in Hugs (Feb 2001)

module Main where

import Data.Array

-- All in main is only to show the strange behaviour.
-- 
-- arrS is the array that foo (NB (1.0,1)) shows in Hugs.
-- But (foo (NB (1.0,1)))==arrS is False.

-- If I write NB (f,p) -> hCMt [(p,listArray ((1,1),(1,1)) [f])] instead of line 16
-- the bug disappears. That is also the reason why I have to keep the data declaration RD.
-- If I put the type signature of line 18 in scope the bug also disappears.
-- If I write hCMt po_arL = (accumArray (\a _-> a) ZM ((1,1),(1,2)) []) // 
--			    (map (\(po,ar) -> ((1,po),M ar)) po_arL)
-- instead of line 19 and 20 it also vanishes. 

data CM a = ZM | M (Array (Int,Int) a)  deriving (Show,Eq)

data RD =    NB !(Double,Int)

main  = do 
  let arr = foo (NB (1.0,1))
	-- arr = { (1,1) -> M { (1,1) -> 1.0 }, (1,2) -> ZM }

	-- All these should return True
  putStr ("arr==arrS "++show (arr==arrS)++"\n")
  putStr ("arrS==arr "++show (arrS==arr)++"\n")
  putStr ("bnds arr arrS "++show ((bounds arr)==(bounds arrS))++"\n")
  putStr ("bnds +id arr arrS "++show (((bounds.id) arr)==((bounds) arrS))++"\n")
  putStr ("id +bnds arr arrS "++show (((id.bounds) arr)==((bounds) arrS))++"\n")


foo :: RD -> Array (Int,Int) (CM Double)
foo rd = case rd of
    NB (f,p) -> h where h = hCMt [(p,listArray ((1,1),(1,1)) [f])]
			-- h = { (1,p) -> M { (1,1) -> f }, other -> ZM }
   where
   --h0CMt :: Array (Int, Int) (CM Double)
   -- h0CMt = { (1,1) -> ZM, (1,2) -> ZM }
   h0CMt = accumArray (\a _-> a) ZM ((1,1),(1,2)) []

   hCMt prs = h0CMt // (map (\(po,ar) -> ((1,po),M ar)) prs)
			-- [ (1,p), M { (1,1) -> f } ]


arrS :: Array (Int,Int) (CM Double)
arrS = listArray ((1,1),(1,2)) [M (listArray ((1,1),(1,1)) [1.0]),ZM]
