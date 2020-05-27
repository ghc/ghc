{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T11342e where

import GHC.TypeLits ( Symbol, ConsSymbol, UnconsSymbol )
import Data.Type.Equality ( (:~:)(..) )

type Reverse :: Symbol -> Symbol
type family Reverse word where
  Reverse word = Reverse1 (UnconsSymbol word) ""

type Reverse1 :: Maybe (Char, Symbol) -> Symbol -> Symbol
type family Reverse1 xs ys where
  Reverse1 Nothing acc         = acc
  Reverse1 (Just '(x, xs)) acc = Reverse1 (UnconsSymbol xs) (ConsSymbol x acc)

reverseTest
  :: Reverse "tiw fo luos eht si ytiverB" :~: "Brevity is the soul of wit"
reverseTest = Refl

reverseTest2 :: Reverse (Reverse "know thyself") :~: "know thyself"
reverseTest2 = Refl
