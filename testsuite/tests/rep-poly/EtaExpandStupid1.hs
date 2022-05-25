
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}

module EtaExpandStupid1 where

import Data.Kind
import Data.Typeable ( Typeable )
import GHC.Exts


--T4809-like
type D3 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data family D3 a1 a2 a3 a4 xxx1 xxx2 xxx3 c1 c2 c3 c4
newtype instance D3 a1 a2 a34 a34 Int Word Char c1 c2 c34 c34 c555 c555 c555 where
  MkD3 :: forall a34' c555' a1' a2' c1' c2' c34'. Maybe c2' -> D3 a1' a2' a34' a34' Int Word Char c1' c2' c34' c34' c555' c555' c555'

foo :: forall b1 b2 b34 d1 d2 d34 d555. Maybe d2 -> D3 b1 b2 b34 b34 Int Word Char d1 d2 d34 d34 d555 d555 d555
foo = MkD3 @_ @d555 @b1 @b2

--tcrun029-like
data Eq a => D a = MkD { fld1 :: a }

bar :: D Bool
bar = bar { fld1 = True }


type D4 :: TYPE FloatRep -> Type -> Type -> Type
data (Ord b, Typeable c, Num c) => D4 a b c = forall d. Eq d => MkD4 a b c d

foo4 :: (Num c, Typeable c, Eq d) => [Maybe Int] -> c -> d -> D4 Float# [Maybe Int] c
foo4 = MkD4 @Float# ( 9.0# `timesFloat#` 17.0# )

bar4 :: D4 Float# [Maybe Int] Int
bar4 = foo4 [Just 2, Nothing] 11 False


type C :: TYPE r -> Constraint
class C a where
instance C Double#

type N :: TYPE r -> TYPE r
newtype C a => N a = MkN a

quux :: Double# -> N Double#
quux = MkN

wibble _ = quux 2.0##
