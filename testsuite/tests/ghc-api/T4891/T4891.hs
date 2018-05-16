{-# LANGUAGE BangPatterns #-}
module Main where

import ByteCodeLink
import CoreMonad
import Data.Array
import DataCon
import GHC
import GHC.Exts.Heap
import HscTypes
import Linker
import RtClosureInspect
import TcEnv
import Type
import TcRnMonad
import TcType
import Control.Applicative
import Name (getOccString)
import Unsafe.Coerce
import Control.Monad
import Data.Maybe
import Bag
import Outputable
import GhcMonad
import X

import System.Environment

main :: IO ()
main = do [libdir] <- getArgs
          runGhc (Just libdir) doit

doit :: Ghc ()
doit = do
  dflags' <- getSessionDynFlags
  primPackages <- setSessionDynFlags dflags'
  dflags <- getSessionDynFlags
  target <- guessTarget "X.hs" Nothing
  setTargets [target]
  load LoadAllTargets

  () <- chaseConstructor (unsafeCoerce False)
  () <- chaseConstructor (unsafeCoerce [1,2,3])
  () <- chaseConstructor (unsafeCoerce (3 :-> 2))
  () <- chaseConstructor (unsafeCoerce (4 :->. 4))
  () <- chaseConstructor (unsafeCoerce (4 :->.+ 4))
  return ()

chaseConstructor :: (GhcMonad m) => HValue -> m ()
chaseConstructor !hv = do
  dflags <- getDynFlags
  liftIO $ putStrLn "====="
  closure <- liftIO $ getClosureData hv
  case closure of
    IndClosure{indirectee=ind} ->
      (\(Box a) -> chaseConstructor (unsafeCoerce a)) ind
    ConstrClosure{} -> do
      withSession $ \hscEnv -> liftIO $ do
        eDcname <- constrClosToName hscEnv closure
        case eDcname of
          Left _       -> return ()
          Right dcName -> do
            putStrLn $ "Name: "      ++ showPpr dflags dcName
            putStrLn $ "OccString: " ++ "'" ++ getOccString dcName ++ "'"
            dc <- ioLookupDataCon hscEnv dcName
            putStrLn $ "DataCon: "   ++ showPpr dflags dc
    _ -> return ()
