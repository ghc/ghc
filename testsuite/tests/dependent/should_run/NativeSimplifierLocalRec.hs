{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}
module NativeSimplifierLocalRec (observe, observeInGroup, ordinary) where

data Flag (b :: Bool) where
    IsFalse :: Flag 'False
    IsTrue :: Flag 'True

{-# NOINLINE choose #-}
choose :: foreach (b :: Bool) -> Flag b
choose False = IsFalse
choose True = IsTrue

{-# NOINLINE demand #-}
demand :: Flag b -> Int
demand IsFalse = 0
demand IsTrue = 7

observe :: Bool -> Int
observe seed =
    let source :: Bool
        source = step seed
        step x = if x then True else source
    in demand (choose source)

observeInGroup :: Bool -> Int
observeInGroup seed =
    let source :: Bool
        source = step seed
        always _ = True
        step x = if always x then True else observeFlag == 0
        observeFlag :: Int
        observeFlag = demand (choose source)
    in observeFlag

ordinary :: Bool -> Int
ordinary seed =
    let source = step seed
        step x = if x then True else source
    in if source then 9 else 10
