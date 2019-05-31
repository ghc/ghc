{-# LANGUAGE PatternSynonyms, ViewPatterns, ConstraintKinds, TypeFamilies,
             PolyKinds, KindSignatures, TopLevelKindSignatures #-}
module T10997_1a where

import Data.Kind

type Showable :: k -> Constraint
type family Showable a where
  Showable (a :: Type) = (Show a)
  Showable a           = ()

extractJust :: Maybe a -> (Bool, a)
extractJust (Just a) = (True, a)
extractJust _        = (False, undefined)

pattern Just' :: Showable a => a -> (Maybe a)
pattern Just' a <- (extractJust -> (True, a)) where
  Just' a = Just a

