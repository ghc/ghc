{-# LANGUAGE ExplicitNamespaces, GADTs, Haskell2010, ImpredicativeTypes, LinearTypes, NoExperimentalDependentTypes, RequiredTypeArguments, ScopedTypeVariables, TypeAbstractions, TypeApplications #-}
module Main where

import Control.Exception
import GadtInteriorForAllProvider

throws :: a -> IO Bool
throws x = either (const True) (const False) <$> try @SomeException (evaluate x)

readBox :: Box a -> (Int, a)
readBox (Box n @a (x :: a)) = (n, x)

readPack :: Pack -> Int
readPack (Pack n @a (_ :: a)) = n

contrasting :: Later a a -> Int
contrasting (Later IsInt @Int x) = x
contrasting (Later Any _) = 0

readShadow :: Shadow -> ()
readShadow (Shadow @outer (_ :: outer) @inner (_ :: inner)) = ()

readRequired :: Required a -> (Int, a)
readRequired (Required n (type b) (x :: b)) = (n, x)

readRequiredPack :: RequiredPack -> Int
readRequiredPack (RequiredPack n (type a) (_ :: a)) = n

refined :: RequiredLater a a -> Int
refined (RequiredLater IsInt (type Int) x) = x
refined (RequiredLater Any (type a) (_ :: a)) = 0

main :: IO ()
main = do
  print (readBox (Box 23 @Bool False), readPack (Pack 29 @Char 'q'),
         contrasting (Later IsInt @Int 37), contrasting (Later @Bool Any @Bool True),
         readShadow (Shadow @Bool True @Char 'z'))
  let bottom = error "strict prefix" :: Int
  partial <- throws (seq @(forall a. a -> Box a) (Box bottom) True)
  applied <- throws (Box bottom @Bool)
  saturated <- throws (Box bottom @Bool True)
  lazySuffix <- throws (Box 5 @Bool (error "lazy suffix"))
  print (partial, applied, saturated, lazySuffix)
  print (readRequired (Required 41 (type Bool) True),
         readRequiredPack (RequiredPack 43 Char 'x'),
         refined (RequiredLater IsInt (type Int) 47),
         refined (RequiredLater Any (type Bool) False))
  requiredPartial <- throws (seq @(forall a -> a %1 -> Required a) (Required bottom) True)
  requiredApplied <- throws (Required bottom (type Bool))
  requiredSaturated <- throws (Required bottom (type Bool) True)
  requiredLazy <- throws (Required 5 (type Bool) (error "lazy suffix"))
  print (requiredPartial, requiredApplied, requiredSaturated, requiredLazy)
