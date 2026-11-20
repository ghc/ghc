{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeAppendLengthProof (lengthAppend, showPreludeAppendLength) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (Appended, Plus, plus)
import PreludeMapLengthProof (Spine(..))
import qualified PreludeMapLengthSize as S
import PreludeMapLengthSource (Count)

-- | Prove the length of append by induction on the first list.
lengthAppend :: Spine xs -> Proxy ys -> Count (Appended xs ys) :~: Plus (Count xs) (Count ys)
lengthAppend Empty _ = Refl
lengthAppend (Cons xs) ys = case lengthAppend xs ys of
    Refl -> Refl

-- | Count the list constructors produced by append.
{-# NOINLINE observedLength #-}
observedLength :: [a] -> [a] -> S.Size
observedLength xs ys = S.size (xs ++ ys)

-- | Observe the proof, the two counts, and the first constructor of a partial append.
showPreludeAppendLength :: String
showPreludeAppendLength =
    show ( lengthAppend (Cons (Cons Empty) :: Spine '[ 'True, 'False]) (Proxy :: Proxy '[ 'False])
         , observedLength left right
         , plus (S.size left) (S.size right)
         , case observedLength (badHead : error "append forced the left tail") (error "append forced the right list") of
              S.Succ _ -> True
              S.Zero -> False
         )
  where
    left = [badHead, badHead]
    right = [badHead]
    badHead = error "counting forced a list element" :: Bool
