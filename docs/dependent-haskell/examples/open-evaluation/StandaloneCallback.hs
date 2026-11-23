{-# LANGUAGE Haskell2010, UnsaturatedTypeFamilies #-}

module StandaloneCallback (Apply, FamilyResult) where

type family Elements a where
    Elements a = [a]

-- | Apply the callback at the kind inferred from its argument.
type Apply f a = f a

-- | The provider supplies the partial family.
type FamilyResult = Apply Elements Bool
