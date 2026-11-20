{-# LANGUAGE DataKinds, GADTs, Modifiers, NoExperimentalDependentTypes #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module ListFoldrProof (foldNil, foldCons, foldAppend, showListFoldAppend) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import ListFoldrSource (Folded, HeadStep)
import qualified GHC.Internal.List as List
import PreludeAppendLengthSource (Appended)
import PreludeMapLengthProof (Spine(..))

-- | An empty list returns the seed.
foldNil :: forall a b (k :: a %U -> b %U -> b) (z :: b).
           Proxy k -> Proxy z -> Folded k z '[] :~: z
foldNil _ _ = Refl

-- | A cons passes its head and the fold of its tail to the combiner.
foldCons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
            Proxy k -> Proxy z -> Proxy x -> Proxy xs
         -> Folded k z (x ': xs) :~: k x (Folded k z xs)
foldCons _ _ _ _ = Refl

-- | Fold the concatenation by using the right fold as the seed of the left.
foldAppend :: Proxy k -> Proxy z -> Spine xs -> Proxy ys
           -> Folded k z (Appended xs ys) :~: Folded k (Folded k z ys) xs
foldAppend _ _ Empty _ = Refl
foldAppend k z (Cons xs) ys = case foldAppend k z xs ys of
    Refl -> Refl

-- | Apply the theorem to a finite left spine and run the installed fold.
showListFoldAppend :: String
showListFoldAppend = show (proof, List.foldr const False ([True] ++ error "forced the right list"))
  where
    left :: Spine '[ 'True, 'False]
    left = Cons (Cons Empty)
    proof = foldAppend (Proxy @HeadStep) (Proxy @'False) left (Proxy @'[ 'True])
