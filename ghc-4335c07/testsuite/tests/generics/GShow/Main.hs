{-# LANGUAGE DeriveGeneric, MagicHash #-}

module Main where

import GHC.Exts
import GHC.Generics hiding (C, D)
import GShow

-- We should be able to generate a generic representation for these types
data D a = D0 | D1 { d11 :: a, d12 :: (D a) } deriving Generic
data U a = U a Char# Double# Float# Int# Word# deriving Generic

-- Example values
d0 :: D Char
d0 = D0
d1 = D1 (Just 'p') D0

d2 :: D (Int,Float)
d2 = D1 (3,0.14) D0

u0 :: U Int
u0 = U 1 '1'# -1.0## -1.0# -1# 1##

-- Generic instances
instance (GShow a) => GShow (D a)
instance (GShow a) => GShow (U a)

-- Tests
main = mapM_ putStrLn [gshow d0, gshow d1, gshow d2, gshow u0]
