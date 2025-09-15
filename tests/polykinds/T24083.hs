{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module T24083 where
import Data.Kind (Constraint, Type)

data family Pi t :: Type

type FunctionSymbol :: Type -> Type
type FunctionSymbol t = Type

type IsSum :: forall s. FunctionSymbol s -> Constraint
class IsSum (sumf :: FunctionSymbol t) where
    sumConNames :: Pi t
