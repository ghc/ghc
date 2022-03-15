{-# LANGUAGE GADTs, RankNTypes, PatternSynonyms, PolyKinds, ViewPatterns, TypeOperators #-}

module DCo_TypeRep where

import Type.Reflection (SomeTypeRep(SomeTypeRep), pattern Fun, typeRepKind )

------------------------------------------------------------------------

getSomeTypeRep :: SomeTypeRep
getSomeTypeRep
  | SomeTypeRep f <- getSomeTypeRep
  = case typeRepKind f of
      Fun _ _ -> error (show f)
      _       -> error "not fun"
