{-# LANGUAGE TypeFamilies, GADTs, ScopedTypeVariables, KindSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}

-- Tests whether a type signature can refine a type
-- See the definition of bug2a

module ShouldCompile where

data Typed
data Untyped

type family TU a b :: *
type instance TU Typed b = b
type instance TU Untyped b = ()

-- A type witness type, use eg. for pattern-matching on types
data Type a where
    TypeInt     :: Type Int
    TypeBool    :: Type Bool
    TypeString  :: Type String
    TypeList    :: Type t -> Type [t]

data Expr :: * -> * -> * {- tu a -} where
    Const :: Type a -> a -> Expr tu (TU tu a)
    Var2  :: a -> TU tu (Type a) -> Expr tu (TU tu a)

bug1 :: Expr Typed Bool -> ()
bug1 (Const TypeBool False) = ()

bug2a :: Expr Typed Bool -> ()
bug2a (Var2 x (TypeBool :: Type Bool)) = ()

bug2c :: Expr Typed Bool -> ()
bug2c (Var2 x TypeBool) = ()

bug2b :: Expr Typed (TU Typed Bool) -> ()
bug2b (Var2 x TypeBool) = ()

