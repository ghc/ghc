{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Functor.Const (Const(..))
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import DependentPair (DPair(..), withPair)
import DependentProjection (second)
import NativeDefaultO1InterfaceProducer
import qualified PackedVecLibrary as Vec
import System.Environment (getArgs)

observeFlag :: (Bool -> Bool) -> Bool -> Char
observeFlag f b = case classify f b of
    Off -> 'f'
    On  -> 't'

main :: IO ()
main = do
    [arg] <- getArgs
    let n = read arg
    print (exposure (error "type-only index") (error "unused retained argument") n,
           ordinaryWorker n (error "unused ordinary argument"))
    bad <- try (evaluate (ordinaryWorker (error "strict ordinary argument") False))
        :: IO (Either ErrorCall Int)
    case bad of
        Left err | "strict ordinary argument" `isInfixOf` show err -> putStrLn "strict argument forced"
        _ -> fail "strict argument should be forced"
    ref <- newIORef (error "unused IO witness" :: IO Int)
    witness <- readIORef ref
    let pair = Pair witness (Const 'i') :: DPair (IO Int) (Const Char)
    print (getConst (second pair))
    let values :: [Vec.Vec Int ('Vec.S 'Vec.Z)]
        values = [Vec.VCons 19 Vec.VNil]
    print (map Vec.toList (Vec.toList (Vec.genericRefinedNested (Vec.S Vec.Z) values)))
    let chars = Pair (Vec.S Vec.Z) (Vec.VCons 'x' Vec.VNil) :: DPair Vec.Nat (Vec.Vec Char)
    firstPair <- readIORef =<< newIORef chars
    secondPair <- readIORef =<< newIORef pair
    print (withPair firstPair (\x payload -> (Vec.natToInt x, Vec.toList payload)),
           withPair secondPair (\_ payload -> getConst payload))
    strict <- try (evaluate (observeFlag id (error "forced flag"))) :: IO (Either ErrorCall Char)
    print (observeFlag not True, observeFlag (const True) (error "unused input"),
           either (isInfixOf "forced flag" . show) (const False) strict)
