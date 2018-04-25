{-# LANGUAGE
	CPP,
        NoMonomorphismRestriction,
        TypeOperators, RankNTypes,
        FlexibleContexts #-}
#include "fusion-phases.h"

module SMVMVectorised (smvmPA) 
where
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.PArray
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr


{-# NOINLINE smvmPA #-}
smvmPA :: PArray (PArray (Int, Double)) -> PArray Double -> PArray Double
smvmPA m v
        = v_smvm $: m $: v

{-# INLINE lap #-}
lap = liftedApply

-- smvm = (\m v. mapP (\r. sumP (mapP (\z. case z of (i, x) -> x * (v !: i)))) m)
{-# INLINE_USER v_smvm #-}
v_smvm :: PArray (PArray (Int, Double)) :-> PArray Double :-> PArray Double
v_smvm          = Clo (\_ m -> Clo vsmvm undefined m) undefined ()


-- mapP (\r. sumP (mapP (\z. case z of (i, x) -> x * (v !: i)) r)) m  
{-# INLINE_USER vsmvm #-}
vsmvm :: PArray (PArray (Int, Double)) -> PArray Double -> PArray Double
vsmvm m v      = mapPP $: (Clo undefined lsmvm2 v) $: m


-- (\r. sumP (mapP (\z. case z of (i, x) -> x * (v !: i)) r))
{-# INLINE_USER lsmvm2 #-}
lsmvm2  :: Int -> PData (PArray Double) -> PData (PArray (Int, Double)) -> PData Double
lsmvm2 c vs rs 
        = lap c (replicatePR c sumPP_double) 
                (lap c  (lap c  (replicatePR c mapPP) 
                                (AClo undefined lsmvm3 vs))
                        rs)

-- \z. case z of (i, x) -> x * (v !: i)
{-# INLINE_USER lsmvm3 #-}
lsmvm3  :: Int -> PData (PArray Double) -> PData (Int, Double) -> PData Double
lsmvm3 c vs zs
 = case zs of
    PTuple2 is xs
     -> lap c    (lap c (replicatePR c multPP_double) xs)
                 (lap c (lap c (replicatePR c indexPP) vs) is)
