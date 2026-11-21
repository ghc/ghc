{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeMapCompositionProof (mapComposition, showPreludeMapComposition) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapCompositionSource (ComposedMap)
import PreludeMapLengthProof (Spine(..))
import PreludeMapLengthSource (Mapped)

-- | Prove map composition by induction on a finite list spine.
mapComposition :: Proxy f -> Proxy g -> Spine xs
               -> Mapped f (Mapped g xs) :~: ComposedMap f g xs
mapComposition _ _ Empty = Refl
mapComposition f g (Cons xs) = case mapComposition f g xs of
    Refl -> Refl

-- | Apply the callbacks in separate traversals.
{-# NOINLINE mapTwice #-}
mapTwice :: (b -> c) -> (a -> b) -> [a] -> [c]
mapTwice f g xs = map f (map g xs)

-- | Apply the composed callbacks in one traversal.
{-# NOINLINE mapOnce #-}
mapOnce :: (b -> c) -> (a -> b) -> [a] -> [c]
mapOnce f g xs = map (f . g) xs

-- | Observe the proof, both results, and the demand for the first cons and head.
showPreludeMapComposition :: String
showPreludeMapComposition =
    show ( mapComposition Proxy Proxy (Cons (Cons Empty) :: Spine '[ 'True, 'False])
         , mapTwice length show [True, False]
         , mapOnce length show [True, False]
         , (hasCons (mapTwice outer inner input), hasCons (mapOnce outer inner input))
         , (hasTrueHead (mapTwice (const True) inner input), hasTrueHead (mapOnce (const True) inner input))
         )
  where
    inner = error "map forced the inner callback" :: Bool -> Char
    outer = error "map forced the outer callback" :: Char -> Int
    input = error "map forced the head" : error "map forced the tail"
    hasCons (_ : _) = True
    hasCons [] = False
    hasTrueHead (True : _) = True
    hasTrueHead _ = False
