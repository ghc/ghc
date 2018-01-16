{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, GADTs #-}

module FDsFromGivens2 where

class C a b | a -> b where
   cop :: a -> b -> ()

data KCC where
  KCC :: C Char Char => () -> KCC

f :: C Char [a] => a -> a
f = undefined

bar :: KCC -> a -> a
bar (KCC _) = f
