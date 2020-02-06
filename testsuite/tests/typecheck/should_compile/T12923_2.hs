{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module T12924 where

import GHC.TypeLits

data A (b :: [Symbol]) = A deriving Show

-- Test that ExtendedDefaultRules defaults multiparameter typeclasses with only
-- one parameter of kind Type.
class Works a (b :: [Symbol]) where
   works :: a -> A b

instance Works Integer a where
   works _ = A

main :: IO ()
main = print (addA (works 5) (works 10)) -- :: A '[])

-- | Note argument types aren't concrete
addA :: A a -> A a -> A '[]
addA A A = A
