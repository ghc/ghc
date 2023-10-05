{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module RepPolyPatSynArg where

import Data.Kind
import GHC.Exts

type X :: TYPE rep -> Type
data X a = MkX

pattern Pat :: forall rep (a :: TYPE rep). a -> X a
pattern Pat bndr_a <- ( undefined -> bndr_a )
