{-# OPTIONS_GHC -XFunctionalDependencies -XMultiParamTypeClasses #-}

module T2688 where

class VectorSpace v s | v -> s where
    (*^)    :: s -> v -> v
    (^/)    :: v -> s -> v
    v ^/ s = v *^ (1/s)
