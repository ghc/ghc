{-# language TypeInType, ScopedTypeVariables #-}
module Silly where
import Type.Reflection (Typeable, typeRep, TypeRep)
import Type.Reflection.Unsafe (mkTrApp)
import GHC.Exts (TYPE, RuntimeRep (..))
import Data.Kind (Type)

mkTrFun :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                  (a :: TYPE r1) (b :: TYPE r2).
           TypeRep a -> TypeRep b -> TypeRep ((a -> b) :: Type)
mkTrFun a b = typeRep `mkTrApp` a `mkTrApp` b

-- originally reported that there was no (Typeable LiftedRep) instance,
-- presumably to overeager RuntimeRep defaulting
