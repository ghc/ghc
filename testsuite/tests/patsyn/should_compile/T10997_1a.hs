{-# LANGUAGE PatternSynonyms, ViewPatterns, ConstraintKinds, TypeFamilies, PolyKinds, KindSignatures #-}
module T10997_1a where

import Data.Kind

type family Showable (a :: k) :: Constraint where
  Showable (a :: Type) = (Show a)
  Showable a           = ()

extractJust :: Maybe a -> (Bool, a)
extractJust (Just a) = (True, a)
extractJust _        = (False, undefined)

pattern Just' :: Showable a => a -> (Maybe a)
pattern Just' a <- (extractJust -> (True, a)) where
  Just' a = Just a

