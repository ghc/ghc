{-# LANGUAGE GADTs, Haskell2010, ImpredicativeTypes, NoExperimentalDependentTypes, ScopedTypeVariables, TypeAbstractions, TypeApplications #-}
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
