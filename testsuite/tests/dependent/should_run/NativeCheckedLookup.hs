{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, StandaloneKindSignatures, TypeOperators #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.Kind (Type)
import Data.List (isPrefixOf)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeCheckedLookupProvider as P

type Found :: forall (n :: P.Nat). P.Env n -> P.Fin n -> Bool -> Type
data Found e i v where
    AtZero :: Found ('P.Cons x e) 'P.FZ x
    AtNext :: Found e i x -> Found ('P.Cons y e) ('P.FS i) x

lookupEvidence :: Found e i x -> P.lookupEnv e i :~: x
lookupEvidence AtZero = Refl
lookupEvidence (AtNext p) = lookupEvidence p

#ifdef WRONG_INDEX
wrong :: Proxy x -> Proxy y -> P.lookupEnv ('P.Cons x ('P.Cons y 'P.Nil)) ('P.FS 'P.FZ) :~: x
wrong _ _ = Refl
#endif

#ifdef ACTIVE_RULE
{-# RULES "override checked lookup" forall e i. P.lookupEnv e i = True #-}
#endif

main :: IO ()
main = do
    let left = P.lookupEnv (P.Cons True (error "tail" :: P.Env 'P.Z)) P.FZ
        next = P.lookupEnv (P.Cons (error "head") (P.Cons True P.Nil)) (P.FS P.FZ)
    order <- try (evaluate (P.lookupEnv (error "environment" :: P.Env ('P.S 'P.Z)) (error "position")))
    let leftFirst = either (isPrefixOf "environment" . displayException) (const False)
                    (order :: Either ErrorCall Bool)
    print (left, next, leftFirst)
