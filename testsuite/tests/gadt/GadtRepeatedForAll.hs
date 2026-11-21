{-# LANGUAGE GADTs, Haskell2010, ImpredicativeTypes, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeAbstractions, TypeApplications #-}
module Main where

import Control.Exception
import GadtRepeatedForAllProvider

openParcel :: Parcel -> Int
openParcel (Parcel n @l (_ :: l) @r (_ :: r)) = n

openIndexed :: Indexed l r -> (Int, l, r)
openIndexed (Indexed n @l (x :: l) @r (y :: r)) = (n, x, y)

throws :: a -> IO Bool
throws x = either (const True) (const False) <$> try @SomeException (evaluate x)

main :: IO ()
main = do
  print (openParcel (Parcel 17 @Bool True @Char 'q'),
         openIndexed (Indexed 23 @Bool False @Char 'x'))
  let bottom = error "strict prefix" :: Int
  prefix <- throws (seq @(forall a. a -> forall b. b -> Parcel) (Parcel bottom) True)
  middle <- throws (seq @(forall a. a -> Parcel) (Parcel bottom @Bool True) True)
  late <- throws (Parcel bottom @Bool True @Char)
  saturated <- throws (Parcel bottom @Bool True @Char 'r')
  lazy <- throws (openParcel (Parcel 5 @Bool (error "left") @Char (error "right")))
  print (prefix, middle, late, saturated, lazy)
