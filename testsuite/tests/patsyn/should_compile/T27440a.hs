{-# LANGUAGE DataKinds, PatternSynonyms, TypeAbstractions #-}
module T27440a where

import Data.Kind (Type)

newtype Lit = Lit { litName :: String }

type LitOfValue :: Bool -> Type
newtype LitOfValue v = LitOfValue { underlyingLit :: Lit }

pattern FalseLit :: Lit -> LitOfValue False
pattern FalseLit a = LitOfValue @False a
