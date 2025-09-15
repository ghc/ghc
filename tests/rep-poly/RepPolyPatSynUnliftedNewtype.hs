{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedNewtypes #-}

module RepPolyPatSynUnliftedNewtype where

import GHC.Exts

type X :: TYPE rep -> TYPE rep
newtype X a = MkX a

pattern Pat :: forall rep (a :: TYPE rep). a -> X a
pattern Pat bndr_a <- MkX bndr_a
