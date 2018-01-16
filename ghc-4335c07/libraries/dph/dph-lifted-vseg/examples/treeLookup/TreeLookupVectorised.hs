{-# LANGUAGE TypeOperators, ExplicitForAll #-}

module TreeLookupVectorised where

import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.PArray
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import qualified Data.Array.Parallel.Unlifted   as U

{-
treeLookup :: [:Int:] -> [:Int:] -> [:Int:]
treeLookup table xx
 | lengthP xx == 1
 = [: table !: (xx !: 0) :]
        
 | otherwise
 = let   len     = lengthP xx
         half    = len `div` 2
         s1      = sliceP 0    half xx
         s2      = sliceP half half xx           
   in    concatP (mapP (treeLookup table) [: s1, s2 :])
-}

ex_tree'
 = let  arr     = fromListPA [0..65535]
   in   treeLookupPA arr arr
                                

{-# NOINLINE treeLookupPA #-}
treeLookupPA :: PArray Int -> PArray Int -> PArray Int
treeLookupPA arr ixs
        = ltree 1 (replicatePR 1 arr) (replicatePR 1 ixs) `indexPR` 0
        

treePP :: PArray Int :-> PArray Int :-> PArray Int
treePP = closure2 undefined ltree


ltree :: Int -> PData (PArray Int) -> PData (PArray Int) -> PData (PArray Int)
ltree 0 _ _     = emptyPR 
ltree c table xx
 = let  
        ($$)    :: forall a b. PData (a :-> b) -> PData a -> PData b
        ($$)    = liftedApply c

        rep     :: forall a. PR a => a -> PData a
        rep     = replicatePR c

        -- if length xx == 1 then ... else ...
        scrut   :: PData Int
        scrut@(PInt tags)
                = (rep eqPP_int) $$ (rep lengthPP $$ xx) $$ (rep 1)

        sel2    = U.tagsToSel2 tags

        -- False branch
        tableF  = packByTagPR table (U.tagsSel2 sel2) 0
        xxF     = packByTagPR xx    (U.tagsSel2 sel2) 0
        resultF = ltreeF (U.elementsSel2_0 sel2) tableF xxF

        -- True branch
        tableT  = packByTagPR table (U.tagsSel2 sel2) 1
        xxT     = packByTagPR xx    (U.tagsSel2 sel2) 1
        resultT = ltreeT (U.elementsSel2_1 sel2) tableT xxT

   in   combine2PR sel2 resultF resultT


-- False branch
ltreeF :: Int -> PData (PArray Int) -> PData (PArray Int) -> PData (PArray Int)
ltreeF c table xx
 = let  ($$)    :: forall a b. PData (a :-> b) -> PData a -> PData b
        ($$)    = liftedApply c

        rep     :: forall a. PR a => a -> PData a
        rep     = replicatePR c

        -- div (lengthP xx) 2
        half    = rep divPP_int $$ (rep lengthPP $$ xx) $$ rep 2

        -- s1 = sliceP 0 half xx
        s1      = rep slicePP $$ rep 0 $$ half $$ xx

        -- s2 = sliceP half half xx
        s2      = rep slicePP $$ half  $$ half $$ xx

        -- [: s1, s2 :]
        ss      :: PData (PArray (PArray Int))
        ss      = rep appendPP  $$ (rep singletonPP $$ s1) 
                                $$ (rep singletonPP $$ s2)
        
   in   rep concatPP $$ (rep mapPP $$ (rep treePP $$ table) $$ ss)


-- True branch
ltreeT :: Int -> PData (PArray Int) -> PData (PArray Int) -> PData (PArray Int)
ltreeT c table xx
 = let  ($$)    :: forall a b. PData (a :-> b) -> PData a -> PData b
        ($$)    = liftedApply c

        rep     :: forall a. PR a => a -> PData a
        rep     = replicatePR c

        -- [: table !: (xx !: 0) :]
   in   rep singletonPP $$ (rep indexPP $$ table $$ (rep indexPP $$ xx $$ rep 0))

