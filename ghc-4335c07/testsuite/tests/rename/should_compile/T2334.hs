{-# LANGUAGE TypeFamilies,EmptyDataDecls,MultiParamTypeClasses #-}
module Test where

type family Fam1 a

class C b where
    f :: ( Fam1 s ~ () ) => b -> s
