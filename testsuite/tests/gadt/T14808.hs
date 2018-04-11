{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module T14808 where

import Data.Kind

data ECC ctx f a where
  ECC :: ctx => f a -> ECC ctx f a

f :: [()] -> ECC () [] ()
f = ECC @() @[] @()

data ECC2 f a ctx where
  ECC2 :: ctx => f a -> ECC2 f a ctx

f2 :: [()] -> ECC2 [] () ()
f2 = ECC2 @() @[] @()
