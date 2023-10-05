{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances,
             DataKinds, TypeFamilies, KindSignatures, PolyKinds, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module T10564 where

import Data.Kind (Type)

class HasFieldM (l :: k) r (v :: Maybe Type)
        | l r -> v

class HasFieldM1 (b :: Maybe [Type]) (l :: k) r v
        | b l r -> v

class HMemberM (e1 :: k) (l :: [k]) (r :: Maybe [k])
        | e1 l -> r

data Label a
type family LabelsOf (a :: [Type]) ::  [Type]

instance (HMemberM (Label (l::k)) (LabelsOf xs) b,
            HasFieldM1 b l (r xs) v)
         => HasFieldM l (r xs) v where
