{-# LANGUAGE CPP, ImpredicativeTypes, NoExperimentalDependentTypes #-}
module RankedFilterReject where

import Filter (filterVec, toList)
import ForwardSchemas (keepInput, lateResult)
import Vector (Vec, consumeVector)

#ifdef ESCAPE_LENGTH
escapeLength :: Vec n Int -> ()
escapeLength xs =
    let result = consumeVector keepInput (filterVec odd xs)
    in result `seq` ()
#elif defined(LATE_BINDER)
lateBinder :: Vec n Int -> ()
lateBinder xs =
    let result = consumeVector (lateResult True) (filterVec odd xs)
    in result `seq` ()
#else
badPredicate :: Vec n Int -> [Int]
badPredicate xs =
    let result = consumeVector (filterVec (\_ -> ())) (filterVec odd xs)
    in toList result
#endif
