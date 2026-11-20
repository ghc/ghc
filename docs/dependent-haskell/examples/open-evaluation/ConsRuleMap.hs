{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fobject-code -fno-omit-interface-pragmas -fno-ignore-interface-pragmas -Wno-inline-rule-shadowing #-}
module ConsRuleMap (mapper) where

-- | Apply the callback to each element and keep the result fields lazy.
{-# NOINLINE [0] mapper #-}
mapper :: (a -> b) -> [a] -> [b]
mapper _ [] = []
mapper f (x : xs) = f x : mapper f xs

{-# RULES "mapper/cons" [~1] forall f x xs.
    mapper f (x : xs) = f x : mapper f xs #-}

{-# RULES "mapper/compose" [~1] forall f g xs.
    mapper f (mapper g xs) = mapper (\x -> f (g x)) xs #-}
