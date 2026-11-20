{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeMapLengthProof (Spine(..), Count, lengthMap, showPreludeMapLength) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import qualified PreludeMapLengthSize as S
import PreludeMapLengthSource (Count, Mapped)

-- | Witness the constructors without storing the elements.
data Spine (xs :: [a]) where
    Empty :: Spine '[]
    Cons :: Spine xs -> Spine (x ': xs)

-- | Prove that map preserves the length for any callback.
lengthMap :: Proxy f -> Spine xs -> Count (Mapped f xs) :~: Count xs
lengthMap _ Empty = Refl
lengthMap f (Cons xs) = case lengthMap f xs of
    Refl -> Refl

-- | Count the result of map without demanding the callback.
{-# NOINLINE observedLength #-}
observedLength :: (a -> b) -> [a] -> S.Size
observedLength f xs = S.size (map f xs)

-- | Observe the proof and count two elements with a bottom callback.
showPreludeMapLength :: String
showPreludeMapLength =
    show (lengthMap Proxy (Cons (Cons Empty) :: Spine '[ 'True, 'False]),
          observedLength (error "map forced the callback" :: Bool -> Char) [True, False])
