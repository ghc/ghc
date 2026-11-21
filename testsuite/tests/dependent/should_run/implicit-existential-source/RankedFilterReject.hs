{-# LANGUAGE CPP, ImpredicativeTypes, NoExperimentalDependentTypes #-}
module RankedFilterReject where

import Filter (Hidden, filterVec, toList)
import ForwardSchemas (keepInput, lateResult)
import Vector (Vec, consumeVector)
#if defined(SHARED_CALLBACK) || defined(SHARED_INPUTS) || defined(SHARED_RESULT) || defined(ESCAPING_SECOND)
import ForwardSchemas (Restricted, restrict)
import Nested (packPair)
import NestedSignature (PackedPair)
#endif

#ifdef ESCAPE_LENGTH
escapeLength :: Vec n Int -> ()
escapeLength xs =
    let result = consumeVector keepInput (filterVec odd xs)
    in result `seq` ()
#elif defined(LATE_CONSTRAINT)
qualifiedResult :: Bool -> forall j a. Show a => Vec j a -> Hidden a
qualifiedResult _ = filterVec (const True)

missingShow :: Vec n a -> [a]
missingShow xs =
    let result = consumeVector (qualifiedResult True) (filterVec (const True) xs)
    in toList result
#elif defined(LATE_BINDER)
lateBinder :: Vec n Int -> ()
lateBinder xs =
    let result = consumeVector (lateResult True) (filterVec odd xs)
    in result `seq` ()
#elif defined(SHARED_CALLBACK) || defined(SHARED_INPUTS) || defined(SHARED_RESULT) || defined(ESCAPING_SECOND)
reject :: Vec i Int -> Vec j Int -> ()
reject xs ys =
    let result = withBoth producer (filterVec odd xs) (filterVec even ys)
#ifdef SHARED_RESULT
    in commonLength result
#else
    in result `seq` ()
#endif
  where
#if defined(SHARED_CALLBACK)
    producer :: forall n. Vec n Int -> Vec n Int -> PackedPair Int
    producer left right = packPair left right
#elif defined(ESCAPING_SECOND)
    producer :: forall n m. Vec n Int -> Vec m Int -> Restricted m Int
    producer left right = restrict right left
#else
    producer :: forall n m. Vec n Int -> Vec m Int -> PackedPair Int
    producer left right = packPair left right
#endif

#ifdef SHARED_INPUTS
    withBoth :: (forall n m. Vec n Int -> Vec m Int -> r) -> Vec k Int -> Vec k Int -> r
#else
    withBoth :: (forall n m. Vec n Int -> Vec m Int -> r) -> Vec l Int -> Vec k Int -> r
#endif
    withBoth use left right = use left right

#ifdef SHARED_RESULT
    commonLength :: (Vec n Int, Vec n Int) -> ()
    commonLength _ = ()
#endif
#elif defined(CONDITIONAL_ELEMENT)
badConditional :: Bool -> Vec n Int -> Vec m Bool -> [Int]
badConditional flag xs ys =
    consumeVector (foldr (:) [])
        (if flag then filterVec (const True) xs
                 else filterVec (const True) ys)
#else
badPredicate :: Vec n Int -> [Int]
badPredicate xs =
    let result = consumeVector (filterVec (\_ -> ())) (filterVec odd xs)
    in toList result
#endif
