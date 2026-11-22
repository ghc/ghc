{-# LANGUAGE ExplicitNamespaces, GADTs, LinearTypes, MagicHash, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments, TemplateHaskell, TypeAbstractions, TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import GHC.Exts (Int(I#), Int#, RuntimeRep(IntRep))
import GadtOrderedNewtypeProvider
import qualified Language.Haskell.TH as TH

construct :: $(TH.reifyType 'N)
construct = N

afterField :: Bool %1 -> forall b. N Bool b
afterField = construct @Bool

readN :: N a b %1 -> a
readN (N v) = v

readR :: R a b %1 -> a
readR (R v (type _)) = v

readFamily :: Family Char a b %1 -> a
readFamily (Family v (type _)) = v

readRaw :: Raw a %1 -> Int
readRaw (Raw v (type _)) = I# v

castAfterField :: Int# %1 -> forall b -> Cast @IntRep Int# b
castAfterField value (type b) = Cast @IntRep @Int# value (type b)

readCast :: Cast @IntRep Int# b %1 -> Int
readCast (Cast value (type _)) = I# value

ignoresPayload :: N Bool b -> Bool
{-# NOINLINE ignoresPayload #-}
ignoresPayload (N _) = True

main :: IO ()
main = do
  print (readN (afterField True @Int), readR (R @Int 31 (type Int)),
         readFamily (Family @Int 63 (type Int)), readRaw (Raw 47# (type Int)),
         ignoresPayload (afterField (error "erased newtype") @Int))
  result <- try @SomeException (evaluate (afterField (error "newtype WHNF") @Int `seq` False))
  print (case result of Left _ -> True; Right _ -> False)
  print (readCast (castAfterField 73# (type Bool)))
