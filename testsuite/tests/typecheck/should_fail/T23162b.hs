{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE UndecidableInstances   #-}

module T23162b where

import Data.Kind  ( Type )
import Data.Proxy

type family LV (as :: [Type]) (b :: Type) = (r :: Type) | r -> as b where
  LV (a ': as) b = a -> LV as b

eq :: a -> a -> ()
eq x y = ()

foo :: Proxy a -> b -> LV a b
foo = foo

bar :: (c->()) -> ()
bar =  bar

f1 :: Int -> ()
-- LV alpha Bool ~ LV alpha Char
f1 x = bar (\y -> eq (foo y True) (foo y 'c'))

f2 :: Int -> ()
-- LV alpha Bool ~ Int -> LV alpha Char
f2 x = bar (\y -> eq (foo y True) (\(z::Int) -> foo y 'c'))


