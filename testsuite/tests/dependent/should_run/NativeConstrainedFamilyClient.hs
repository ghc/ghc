{-# LANGUAGE GADTs, NoExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module NativeConstrainedFamilyClient where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeConstrainedFamilyProvider (Allowed, Collects(..), Defaulted(..), Pair(..))

assumed :: forall c. Collects c => Proxy c -> Proxy (Elem c)
assumed _ =
    let local :: Proxy (Elem c)
        local = Proxy
    in local

assumedInPattern :: forall c. Witness c -> ()
assumedInPattern (Witness _) =
    let local :: Proxy (Elem c)
        local = Proxy
    in local `seq` ()
assumedInPattern (ByEquality _) = ()

localContext :: forall c. Proxy c -> ()
localContext _ =
    let local :: Collects c => Proxy (Elem c)
        local = Proxy
    in ()

resolved :: Allowed a => Elem [a] :~: a
resolved = Refl

ground :: Elem [Bool] :~: Bool
ground = Refl

defaulted :: Def Int :~: Char
defaulted = Refl

permuted :: Swapped Int Bool :~: Char
permuted = Refl

data Witness c where
    Witness :: Collects c => Proxy (Elem c) -> Witness c
    ByEquality :: c ~ [Bool] => Proxy (Elem c) -> Witness c

newtype Known = Known (Proxy (Elem [Bool]))
