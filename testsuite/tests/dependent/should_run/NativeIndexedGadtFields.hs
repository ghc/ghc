{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoExperimentalDependentTypes, TypeOperators #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:) (Refl))
import NativeIndexedGadtFieldsProvider
    ( Held(..), Hold(..), Shape(..)
    , inspectAnonymous, inspectCons, inspectEquations, inspectHold, inspectKnown )
import qualified NativeNonuniformGadtFieldsProvider as N
import PackedVecLibrary (Nat(..), Vec(..))

observe :: Vec Bool n -> Bool
observe xs = case inspectCons xs of
    Just ConsSeen -> True
    Nothing -> False

observeAnonymous :: Vec Bool n -> Bool
observeAnonymous xs = case inspectAnonymous xs of
    Just ConsSeen -> True
    Nothing -> False

observeEquations :: Vec Bool n -> Bool
observeEquations xs = case inspectEquations xs of
    Just ConsSeen -> True
    Nothing -> False

observeKnown :: Vec Bool ('S n) -> Bool
observeKnown xs = case inspectKnown xs of
    ConsSeen -> True

observeHold :: Hold 'Z -> Bool
observeHold x = case inspectHold x of
    Held -> True

observeNonuniform :: N.UTerm ('N.Succ 'N.Zero) -> Bool
observeNonuniform term = outer (N.parReflCase term) && outer (N.parReflEq term)
  where
    outer :: N.Par (x :: N.UTerm ('N.Succ 'N.Zero)) y -> Bool
    outer N.PVar = True
    outer (N.PLam _) = True
    outer (N.PApp _ _) = True

observeLegacy :: N.Swap Bool Bool -> Bool
observeLegacy term = case N.inspectSwap term of
    N.Stopped -> False
    N.Advanced -> True

observeDiagonal :: Bool :~: Bool -> Bool
observeDiagonal proof = case N.diagonal proof of Refl -> True

main :: IO ()
main = do
    value <- newIORef (VCons (error "head") (error "tail") :: Vec Bool ('S 'Z)) >>= readIORef
    held <- newIORef (Hold (error "held field" :: Vec Bool 'Z)) >>= readIORef
    spine <- try (evaluate (observe (error "spine" :: Vec Bool ('S 'Z))))
    print (observe value && observeAnonymous value && observeEquations value,
           observeKnown value && observeHold held,
           observe VNil || observeAnonymous VNil || observeEquations VNil,
           either (const True) (const False) (spine :: Either ErrorCall Bool))
    variable <- newIORef (N.UVar (error "variable field")) >>= readIORef
    lambda <- newIORef (N.ULam (error "lambda field")) >>= readIORef
    application <- newIORef (N.UApp (error "left field") (error "right field")) >>= readIORef
    unknown <- newIORef (error "term") >>= readIORef
    swapped <- newIORef (N.Next (error "swap field")) >>= readIORef
    term <- try (evaluate (observeNonuniform unknown))
    print (observeNonuniform variable && case N.inspectOuter variable of
               Just N.PVar -> True
               _ -> False,
           observeNonuniform lambda, observeNonuniform application,
           either (const True) (const False) (term :: Either ErrorCall Bool),
           observeLegacy swapped, observeDiagonal Refl)
