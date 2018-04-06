{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PolyKinds #-}
module Main where

import T13750a

import GHC.Exts (Constraint)
import Unsafe.Coerce
import Data.Proxy

class MyShow a where
  myShow :: a -> String

instance MyShow Char where
  myShow a = [a]

gshowS :: (All2 MyShow xss) => NS xss -> String
gshowS (Z xs)  = gshowP xs
gshowS (S xss) = gshowS xss

gshowP :: (All MyShow xs) => NP xs -> String
gshowP (x :* Nil) = myShow x

class (AllF c xs) => All (c :: k -> Constraint) (xs :: [k])
  -- where foo :: Proxy c     -- This makes it not seg-fault

instance All c '[]
instance (c x, All c xs) => All c (x ': xs)

type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance AllF _c '[]       = ()
type instance AllF  c (x ': xs) = (c x, All c xs)

type All2 f = All (All f)

main :: IO ()
main = do
  let t = 'x' :* Nil
  print (gshowS (Z ('x' :* Nil) :: NS '[ '[ Char ] ]))
