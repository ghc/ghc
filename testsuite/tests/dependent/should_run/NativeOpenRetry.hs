{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE LinearTypes, NamedWildCards, PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Main (main) where

import qualified Data.Char
import Data.Proxy (Proxy(..))
import GHC.TypeLits (charVal, natVal)
import GHC.Types (Multiplicity(Many))
import qualified NativeOpenRetryLib

upper :: Proxy _c -> Proxy (Data.Char.toTitle _c)
upper (_ :: Proxy 'a') = Proxy @'A'

mixed :: Proxy _c -> ( Proxy (Data.Char.toTitle 'x'), Proxy (Data.Char.toTitle _c)
                     , Proxy (Prelude.sum (Prelude.reverse '[2, 3, 5])) )
mixed (_ :: Proxy 'a') = (Proxy @'X', Proxy @'A', Proxy @10)

nested :: Char
nested = charVal (local (Proxy @'a'))
  where
    local :: Proxy _c -> Proxy (Data.Char.toTitle _c)
    local (_ :: Proxy 'a') = Proxy @'A'

expression :: Char
expression = charVal (local (Proxy @'a'))
  where
    local = ((\(_ :: Proxy 'a') -> Proxy @'A') :: Proxy _c -> Proxy (Data.Char.toTitle _c))

data Same (x :: Char) (y :: Char) where
  Same :: Same c c

-- The toLower operand becomes known only after replay evaluates toTitle.
chain :: Proxy _c -> (Proxy (Data.Char.toLower _upper), Same (Data.Char.toTitle _c) _upper)
chain (_ :: Proxy 'a') = (Proxy @'a', Same)

empty :: Proxy _xs -> Proxy (NativeOpenRetryLib.total _xs)
empty (_ :: Proxy '[]) = Proxy @0

large :: Proxy _xs -> Proxy (NativeOpenRetryLib.total _xs)
large (_ :: Proxy '[18446744073709551616, 7]) = Proxy @18446744073709551623

ordinaryNoGen :: (Proxy 'A', Bool)
ordinaryNoGen =
  let x :: Proxy _c
      %Many !(x, y) = (Proxy @'A', True)
  in (x, y)

main :: IO ()
main = do
  print (charVal (upper (Proxy @'a')), nested, expression, charVal (fst (chain (Proxy @'a'))))
  print (natVal (empty (Proxy @'[])), natVal (large (Proxy @'[18446744073709551616, 7])))
  print (snd ordinaryNoGen)
  let (ready, inferred, total) = mixed (Proxy @'a')
  print (charVal ready, charVal inferred, natVal total)
