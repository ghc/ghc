{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Kind
import Type.Reflection

type family F a
type instance F Int = Type

data D1 = forall (a :: F Int). MkD1 a
data D2 = forall (a :: F Int). MkD2

tr1a :: TypeRep (MkD1 True)
tr1a = typeRep

tr1b :: TypeRep (MkD1 Bool)
tr1b = typeRep

tr2 :: TypeRep (MkD2 @Bool)
tr2 = typeRep

test :: TypeRep a -> IO ()
test tr = do
  print tr
  print $ typeRepKind tr

main :: IO ()
main = do
  test tr1a
  test tr1b
  test tr2
