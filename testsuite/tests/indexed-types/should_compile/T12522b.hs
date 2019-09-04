{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T12522b where

import Data.Kind (Type)

newtype I a = I a

type family Curry (as :: [Type]) b = f | f -> as b where
    Curry '[] b = I b
    Curry (a:as) b = a -> Curry as b

data Uncurried (as :: [Type]) b

def :: Curry as b -> Uncurried as b
def = undefined

-- test2 :: Uncurried [Bool, Bool] Bool
test2 = def $ \a b -> I $ a && b
