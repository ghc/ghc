{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE UndecidableInstances   #-}

module T16512a where

import Data.Kind
  ( Type )

-- HOAS representation
data AST (a :: Type) :: Type where
  (:$) :: AST ( a -> b ) -> AST a -> AST b
  -- Lam :: ( AST a -> AST b ) -> AST ( a -> b )
  -- PrimOp :: PrimOp op a => AST a
  -- ...

data ASTs (as :: [Type]) :: Type where
  NilASTs :: ASTs '[]
  ConsAST :: AST a -> ASTs as -> ASTs (a ': as)

type family ListVariadic (as :: [Type]) (b :: Type) = (r :: Type) | r -> as b where
  ListVariadic (a ': as) b = a -> ListVariadic as b
  -- ListVariadic '[] ()     = ()
  -- ListVariadic '[] Bool   = Bool
  -- ListVariadic '[] Word   = Word
  -- ListVariadic '[] Int    = Int
  -- ListVariadic '[] Float  = Float
  -- ListVariadic '[] Double = Double
  -- ...

data AnApplication b where
  AnApplication :: AST (ListVariadic as b) -> ASTs as -> AnApplication b

unapply :: AST b -> AnApplication b
unapply (f :$ a)
  = case unapply f of
        AnApplication g as ->
          AnApplication g (a `ConsAST` as)
-- no other cases with this simplified AST
