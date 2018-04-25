{-# LANGUAGE
	TypeFamilies, MultiParamTypeClasses, 
	FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators,
        NoMonomorphismRestriction #-}

import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray
import Test

main 
 = do   putStrLn $ (show $ lengthPA arr10)
        putStr "foo"

arr10           = fromListPA [1..10 :: Int]
arr3		= fromListPA [1..3  :: Int]
arr5            = fromListPA [1..5  :: Int]
arrN            = fromListPA [arr5, arr10, fromListPA [1..100]]

arrError        :: PArray (PArray Int)
arrError        = error "denied!"

lap = liftedApply

-- length examples
ex_length
 = lengthPP $: arr10
 
ex_length_l
 = mapPP $: lengthPP $: arrN


-- plus examples
--   The constant 5 is replicated by the implementation of plusPA_l.
ex_plus_l	 
 = mapPP $: (plusPP_int $: 5) $: arr10


-- index examples
ex_index_l
 = mapPP $: (indexPP $: arr10) $: arr5
  
