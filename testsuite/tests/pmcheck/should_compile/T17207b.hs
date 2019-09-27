{-# OPTIONS_GHC -Wincomplete-patterns -Wno-missing-methods -fforce-recomp #-}
{-# LANGUAGE GADTs, TypeFamilies, ViewPatterns, TypeOperators, PatternSynonyms #-}
module Lib where

import Data.Type.Equality

data family T a

data instance T () = A

pattern B :: a
pattern B <- (const False -> True)

pattern C :: a
pattern C <- (const True -> True)

{-# COMPLETE B, C :: T #-}

f :: a :~: () -> T a -> ()
f _    B = ()
f Refl A = ()
