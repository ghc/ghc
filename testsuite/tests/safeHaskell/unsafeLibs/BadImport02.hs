{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

-- | Here we used typeable to produce an illegal value
module Main where

import Data.OldTypeable

import BadImport02_A

deriving instance Typeable Nat

data NInt = NInt Int deriving Show

instance Typeable NInt where
    typeOf _ = typeOf (undefined::Nat)

main = do
    let a = succ' $ zero
        Just n@(NInt z) = (cast a) :: Maybe NInt
        n' = NInt (-z)
        Just m = (cast n') :: Maybe Nat
     
    putStrLn $ showNat a
    putStrLn $ show n
    putStrLn $ showNat m
    return ()

