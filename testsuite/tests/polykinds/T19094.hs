{-# Language RankNTypes, TypeApplications, PolyKinds, DataKinds, TypeOperators, StandaloneKindSignatures, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module T19094 where

import Data.Type.Equality
import Data.Kind

type PolyKinded :: Type -> Type
type PolyKinded res = (forall (k :: Type). k -> res)

infix 4
 ===
type
 (===) :: PolyKinded (PolyKinded Bool)
type family
 a === b where
 a === a = True
 _ === _ = False

type  TryUnify :: Bool -> PolyKinded (PolyKinded Constraint)
class (a === b) ~ cond
   => TryUnify cond a b
instance (a === b) ~ False
      => TryUnify False @k a @j b
instance {-# Incoherent #-}
         ( (a === b) ~ True
         , a ~~ b
         )
      => TryUnify True @k a @j b
