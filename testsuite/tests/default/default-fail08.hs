-- | Default for a partially applied constraint doesn't trigger panic

{-# LANGUAGE Haskell2010, ConstraintKinds, MultiParamTypeClasses, NamedDefaults #-}

import Data.Functor.Identity (Identity (Identity))

class C a b where
  meth :: a -> b
instance C Int Integer where
  meth = fromIntegral

type CInt = C Int

default CInt (Integer)

main = print (meth (4 :: Int))
