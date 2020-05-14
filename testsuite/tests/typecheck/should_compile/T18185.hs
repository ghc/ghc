{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T18185 where

import GHC.TypeLits
import Type.Reflection

class iss :|+ is  ~ oss => AddT (iss :: [Symbol]) (is :: Symbol) (oss :: [Symbol]) where
 type iss :|+ is :: [Symbol]

class (CmpSymbol is ish ~ ord, AddT'I ord is ish ist ~ oss) => AddT' ord is ish ist oss where
 type AddT'I ord is ish ist :: [Symbol]

class (CmpSymbol "a" "a" ~ o) => C1 o
class (CmpNat 1 1 ~ o) => C2 o
class ((CmpSymbol "a" "a" :: Ordering) ~ o) => C3 o
class ((CmpNat 1 1 :: Ordering) ~ o) => C4 o

f1 :: TypeRep (CmpSymbol "a" "a")
f1 = typeRep

f2 :: TypeRep (CmpNat 1 1)
f2 = typeRep

f3 :: TypeRep (CmpSymbol "a" "a" :: Ordering)
f3 = typeRep

f4 :: TypeRep (CmpNat 1 1 :: Ordering)
f4 = typeRep
