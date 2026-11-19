{-# LANGUAGE DataKinds, ExplicitForAll, GADTs, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, NoExperimentalDependentTypes, TypeOperators #-}

module BoundaryEvidence (Member(..), Locate(..), here, there, Fields(..), fieldAt) where

import GHC.TypeNats (Nat)

-- | A path to a field with the stated label in this exact list.
data Member (label :: Nat) (labels :: [Nat]) where
    Here :: Member label (label ': rest)
    There :: Member label rest -> Member label (other ': rest)

-- | A request for a membership witness.
class Locate (label :: Nat) (labels :: [Nat]) where
    location :: Member label labels

-- | Introduce the head path with explicit type argument order for the plugin.
here :: forall label rest. Member label (label ': rest)
here = Here

-- | Extend a path with one preceding field.
there :: forall label other rest. Member label rest -> Member label (other ': rest)
there = There

-- | Runtime fields in the order given by their labels.
data Fields (labels :: [Nat]) a where
    End :: Fields '[] a
    Field :: a -> Fields rest a -> Fields (label ': rest) a

-- | Use the path to select the corresponding runtime field.
fieldAt :: Member label labels -> Fields labels a -> a
fieldAt Here (Field value _) = value
fieldAt (There path) (Field _ rest) = fieldAt path rest
