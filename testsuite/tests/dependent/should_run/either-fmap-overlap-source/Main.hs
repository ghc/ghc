{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main (main, originalChoice, specializedChoice) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl), sym, trans)
import GHC.Exts (U)
import DownstreamInstance ()
import ResultFunctor
import UniversalResultProof (MappedResult, fmapRight, originalRuntime)

originalChoice :: forall a (f :: a %U -> Type) (x :: a).
                  Proxy f -> Proxy x ->
                  MappedResult f ('Succeeded x :: Result Type a) :~: ('Succeeded (f x) :: Result Type Type)
originalChoice = fmapRight @Type

specializedChoice :: forall a (f :: a %U -> Type) (x :: a).
                     Proxy f -> Proxy x ->
                     translate f ('Succeeded x :: Result Type a) :~: ('Failed (f x) :: Result Type Type)
specializedChoice _ _ = Refl

#ifdef FALSE_EQUALITY
falseNominal :: forall a (f :: a %U -> Type) (x :: a).
                Proxy f -> Proxy x ->
                ('Failed (f x) :: Result Type Type) :~: 'Succeeded (f x)
falseNominal f x = trans (sym (specializedChoice f x)) (originalChoice f x)
#endif

type family TypeOf (x :: Bool) :: Type where
    TypeOf x = Bool

main :: IO ()
main = do
    let input = Succeeded True :: Result Type Bool
        specialized = translate (const (error "specialized callback forced")) input
    print (case originalRuntime input of Succeeded _ -> True; _ -> False)
    print (case specialized of Failed _ -> True; _ -> False)
    print (originalChoice (Proxy @TypeOf) (Proxy @'True), specializedChoice (Proxy @TypeOf) (Proxy @'True))
