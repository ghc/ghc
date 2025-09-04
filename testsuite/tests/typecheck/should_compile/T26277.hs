module T26277 where

import Data.Kind ( Type, Constraint )
import GHC.Exts ( TYPE )

type FunLike :: forall {k}. (k -> k -> Type) -> Constraint
class FunLike p where
  myId :: p a a
instance FunLike (->) where
  myId x = x

-- This caused a panic
test x = myId @(->) x
