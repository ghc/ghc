{-# LANGUAGE DataKinds, GADTs, Modifiers, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeFilterAppendProof (FilterDecisions(..), filterAppend, showPreludeFilterAppend) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeAppendLengthSource (Appended)
import PreludeFilterRuntime (BooleanResult(..))
import PreludeFilterSource (FilterWith, Negation)

-- | Witness a predicate result for each element of a finite list.
data FilterDecisions (p :: a %U -> Bool) (xs :: [a]) where
    Done :: FilterDecisions p '[]
    Decide :: BooleanResult (p x) -> FilterDecisions p xs -> FilterDecisions p (x ': xs)

-- | Prove that filter distributes over append by induction on the left list.
filterAppend :: FilterDecisions p xs -> Proxy ys
             -> FilterWith p (Appended xs ys) :~: Appended (FilterWith p xs) (FilterWith p ys)
filterAppend Done _ = Refl
filterAppend (Decide decision xs) ys = case filterAppend xs ys of
    Refl -> case decision of
        IsTrue -> Refl
        IsFalse -> Refl

-- | Filter the concatenation.
{-# NOINLINE filterAfterAppend #-}
filterAfterAppend :: (a -> Bool) -> [a] -> [a] -> [a]
filterAfterAppend p xs ys = filter p (xs ++ ys)

-- | Concatenate the filtered lists.
{-# NOINLINE appendAfterFilter #-}
appendAfterFilter :: (a -> Bool) -> [a] -> [a] -> [a]
appendAfterFilter p xs ys = filter p xs ++ filter p ys

-- | Observe the equality and the result after skipping the first element.
showPreludeFilterAppend :: String
showPreludeFilterAppend =
    show ( filterAppend decisions (Proxy :: Proxy '[ 'True, 'False])
         , filterAfterAppend odd ([2, 1] :: [Int]) [4, 3]
         , appendAfterFilter odd ([2, 1] :: [Int]) [4, 3]
         , (hasCons (filterAfterAppend not left right), hasCons (appendAfterFilter not left right))
         )
  where
    decisions :: FilterDecisions Negation '[ 'True, 'False]
    decisions = Decide IsFalse (Decide IsTrue Done)
    left = True : False : error "filter forced the left tail"
    right = error "filter forced the right list"
    hasCons (_ : _) = True
    hasCons [] = False
