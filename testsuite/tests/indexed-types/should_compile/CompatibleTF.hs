{-# LANGUAGE StandaloneKindSignatures, TypeFamilies #-}

module CompatibleTF where

import Data.Kind (Type)

type F :: Type -> Type -> Type -> Type -> Type
type family F a b c d where
  F () b c d = c
  F a () c d = d
  F a b  c c = c

-- Here we need to select the third branch of F, even though the first one is
-- stuck and the second one is blocked on the first one, because the third
-- branch is compatible with the first two.
foo :: F a () () ()
foo = ()



type family Stuck :: Type

type family G :: Type where
  G = Int

type K :: Type -> Type -> Type
type family K a b where
  K Int Char = Bool
  K x y = y


-- Here we need to select the second branch of K, because reducing G shows that
-- the applications are apart from the first branch.
bar :: (K a G, K Stuck G)
bar = (3, 3)



type family F2 a where
  F2 Int = Char

-- Here we need to reduce the second argument deeply to see that it is equal to
-- the first argument, and hence the first branch of K cannot match, so we can
-- select the second branch.
wurble :: F2 Char -> K (F2 Char) (F2 (F2 Int))
wurble x = x
