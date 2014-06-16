{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
             FunctionalDependencies, RankNTypes #-}

module Q where

class Transformer t a | t -> a where
    transform :: t -> l a -> (forall l'. l' a -> b) -> b

data EL a = forall l. EL (l a)

unEL :: EL a -> (forall l. l a -> b) -> b
unEL = error "unEL"

transform' :: (Transformer t a) => t -> EL a -> EL a
transform' = error "transform'"

data MultiToggleS ts a = MultiToggleS ts

data MultiToggle = MultiToggle

expand :: HList ts a => MultiToggleS ts a -> MultiToggle
expand (MultiToggleS ts) =
    resolve ts
        (\x mt ->
            let g = transform' x in
            mt
        )
        MultiToggle

class HList c a | c -> a where
    resolve :: c -> (forall t. (Transformer t a) => t -> b) -> b
