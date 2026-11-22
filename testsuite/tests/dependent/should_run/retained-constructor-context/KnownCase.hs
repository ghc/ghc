{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module Main (main, proof, answer) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import Provider (Nat(..), T(..))

proof :: T a -> a :~: Int
proof (Mk _ _) = Refl

#ifdef WRONG_WITNESS
wrong :: T a -> a :~: Bool
wrong (Mk _ _) = Refl
#endif

-- Keep the exported case while the known application can inline.
answer :: Int
answer = case proof (Mk (S Z) Proxy :: T Int) of Refl -> 1

main :: IO ()
main = do
    print answer
    let n = error "forced retained index" :: Nat
    case proof (Mk n (error "forced dependent payload") :: T Int) of
        Refl -> print (2 :: Int)
