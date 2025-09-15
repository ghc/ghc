{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module T12926 where

import GHC.TypeLits

data A (b :: [Symbol]) = A deriving Show

class Works a (b :: [Symbol]) where
   works :: a -> A b

instance Works Integer a where
   works _ = A

addA :: A a -> A a -> A a
addA A A = A

test2 :: A x -- Note this is able to have a polymorphic type
test2 = addA (works 5) (works 5)
