{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             ExistentialQuantification, GADTSyntax, FlexibleContexts #-}
{-# OPTIONS_GHC -ddump-types #-}

module FloatFDs2 where

class C a b | a -> b where
  meth :: a -> b -> ()

data Ex where
  MkEx :: a -> Ex

f x = (\y -> case x of MkEx _ -> meth x y, \z -> case x of MkEx _ -> meth x z)
