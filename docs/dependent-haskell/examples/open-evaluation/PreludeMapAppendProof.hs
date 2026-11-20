{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeMapAppendProof (mapAppend, showPreludeMapAppend) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (Appended)
import PreludeMapLengthProof (Spine(..))
import PreludeMapLengthSource (Mapped)

-- | Prove that map distributes over append by induction on the first list.
mapAppend :: Proxy f -> Spine xs -> Proxy ys
          -> Mapped f (Appended xs ys) :~: Appended (Mapped f xs) (Mapped f ys)
mapAppend _ Empty _ = Refl
mapAppend f (Cons xs) ys = case mapAppend f xs ys of
    Refl -> Refl

-- | Map over the concatenation.
{-# NOINLINE mapAfterAppend #-}
mapAfterAppend :: (a -> b) -> [a] -> [a] -> [b]
mapAfterAppend f xs ys = map f (xs ++ ys)

-- | Concatenate the mapped lists.
{-# NOINLINE appendAfterMap #-}
appendAfterMap :: (a -> b) -> [a] -> [a] -> [b]
appendAfterMap f xs ys = map f xs ++ map f ys

-- | Observe the proof, both mapped lists, and the demand for the first cons.
showPreludeMapAppend :: String
showPreludeMapAppend =
    show ( mapAppend Proxy (Cons (Cons Empty) :: Spine '[ 'True, 'False]) (Proxy :: Proxy '[ 'True])
         , mapAfterAppend not [True, False] [True]
         , appendAfterMap not [True, False] [True]
         , (hasCons (mapAfterAppend bottom left right), hasCons (appendAfterMap bottom left right))
         )
  where
    bottom = error "map forced the callback" :: Bool -> Bool
    left = error "map forced an element" : error "map forced the left tail"
    right = error "append forced the right list"
    hasCons (_ : _) = True
    hasCons [] = False
