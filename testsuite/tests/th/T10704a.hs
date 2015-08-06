{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
module T10704a where

import Language.Haskell.TH

infixl 1 :=>
infixl 2 :+:
infix  3 :*:
infix  4 :%:
infixr 5 :?:
infixr 6 :@:

class a :=> b
type a :+: b = Either a b
data a :*: b = a :*: b
newtype a :%: b = Percent (a, b)
data family a :?: b
type family a :@: b where a :@: b = Int

fixityExp :: Name -> Q Exp
fixityExp n = reifyFixity n >>= stringE . show
