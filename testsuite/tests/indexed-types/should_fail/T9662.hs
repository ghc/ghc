{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module T9662 where

import Data.Kind (Type)

data Exp a = Exp
data (a:.b) = a:.b

type family Plain e :: Type
type instance Plain (Exp (a :: Type)) = a
type instance Plain (a:.b) = Plain a :. Plain b

class (Plain (Unlifted pattern) ~ Tuple pattern) => Unlift pattern where
   type Unlifted pattern
   type Tuple pattern

modify :: (Unlift pattern) =>
   pattern ->
   (Unlifted pattern -> a) ->
   Exp (Tuple pattern) -> Exp (Plain a)
modify p f = undefined


data Atom a = Atom

atom :: Atom a
atom = Atom


instance (Unlift pa, int ~ Atom Int) => Unlift (pa :. int) where
   type Unlifted (pa :. int) = Unlifted pa :. Exp Int
   type Tuple (pa :. int) = Tuple pa :. Int


data Shape sh = Shape

backpermute ::
   (Exp sh -> Exp sh') ->
   (Exp sh' -> Exp sh) ->
   Shape sh -> Shape sh'
backpermute = undefined

test :: Shape (sh:.k:.m:.n) -> Shape (sh:.m:.n:.k)
test =
   backpermute
      (modify (atom:.atom:.atom:.atom)
         (\(sh:.k:.m:.n) -> (sh:.m:.n:.k)))
      id

-- With this arg instead of just 'id', it worked
--    (modify (atom:.atom:.atom:.atom)
--       (\(ix:.m:.n:.k) -> (ix:.k:.m:.n)))
