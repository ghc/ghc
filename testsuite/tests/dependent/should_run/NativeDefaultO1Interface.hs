{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Functor.Const (Const(..))
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import DependentPair (DPair(..))
import DependentProjection (second)
import NativeDefaultO1InterfaceProducer
import qualified PackedVecLibrary as Vec
import System.Environment (getArgs)

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
