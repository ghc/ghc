{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Linear14 where

-- Inference-related behaviour. Slightly sub-optimal still.

bind1 :: (d ⊸ (a ⊸ b) ⊸ c) ⊸ d ⊸ (a⊸b) ⊸ c
bind1 b x f = b x (\a -> f a)

newtype I a = I a

bind2 :: (d ⊸ (a ⊸ b) ⊸ c) ⊸ d ⊸ (I a⊸b) ⊸ c
bind2 b x f = b x (\a -> f (I a))

bind3 :: (d ⊸ I (a ⊸ b) ⊸ c) ⊸ d ⊸ (a⊸b) ⊸ c
bind3 b x f = b x (I (\a -> f a))

bind4 :: (d ⊸ I ((a ⊸ a') ⊸ b) ⊸ c) ⊸ d ⊸ ((a⊸a')⊸b) ⊸ c
bind4 b x f = b x (I (\g -> f g))

bind5 :: (d ⊸ ((a ⊸ a') ⊸ b) ⊸ c) ⊸ d ⊸ ((a⊸a')⊸b) ⊸ c
bind5 b x f = b x (\g -> f (\a -> g a))

bind6 :: (d ⊸ I ((a ⊸ a') ⊸ b) ⊸ c) ⊸ d ⊸ ((a⊸a')⊸b) ⊸ c
bind6 b x f = b x (I (\g -> f (\a -> g a)))
