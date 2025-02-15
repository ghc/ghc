{-# OPTIONS_GHC -Wunknown-modifiers #-}
{-# LANGUAGE Modifiers, DataKinds #-}

module ModifiersSuggestLinear where

%1 %01 %()
data D = D { d %1 %01 %() :: Int }

l :: Int %1 %01 %() -> Int
l = undefined
