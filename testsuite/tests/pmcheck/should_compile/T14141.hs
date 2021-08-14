{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T14141 where

import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import Data.Kind
  ( Constraint, Type )

-- Example 1: from #14141

data D where
  MkD :: C => D

type C :: Constraint
type family C where
    C = TypeError ('Text "error")

f :: D -> ()
f MkD = ()

-- Example 2: from #16377

type F :: Type -> Constraint
type family F a :: Constraint
type instance F Int  = ()
type instance F Char = TypeError ('Text "Nope")

data T where
  A :: F Int  => T
  B :: F Char => T

exhaustive :: T -> ()
exhaustive A = ()
exhaustive B = ()
