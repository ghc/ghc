{-# LANGUAGE ExistentialQuantification, RankNTypes, MultiParamTypeClasses,
              FunctionalDependencies, FlexibleInstances, FlexibleContexts
 #-}

module XMonad.Layout.MultiToggle where

import Data.Typeable

-- This appears to be the culprit
expand :: (HList ts a) => MultiToggleS ts l a -> MultiToggle ts l a
expand (MultiToggleS b ts) =
    resolve ts id
        (\x mt -> let g = transform' x in mt{ currLayout = g $ currLayout mt })
        (MultiToggle (EL b id) ts)

class (Typeable t) => Transformer t a | t -> a where
    transform :: t
              -> l a
              -> (forall l'. l' a -> (l' a -> l a) -> b)
              -> b

data  EL l a = forall l'. EL (l' a) (l' a -> l a)

transform' :: (Transformer t a) => t -> EL l a -> EL l a
transform' t (EL l det) = undefined

data MultiToggleS ts l a = MultiToggleS (l a) ts
                         deriving (Read, Show)

data MultiToggle ts l a = MultiToggle{
    currLayout :: EL l a,
    transformers :: ts
    }

class HList c a where
    resolve :: c -> b -> (forall t. (Transformer t a) => t -> b) -> b
