{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Lib where

type family T

pattern P :: T
pattern P <- _
{-# COMPLETE P #-}

data U = U
type instance T = U

f :: U -> ()
f P = ()
