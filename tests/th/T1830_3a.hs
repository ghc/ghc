{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
module T1830_3a where

import GHC.Exts
import Language.Haskell.TH.Syntax (Lift(..))

data AlgDT a b c = NormalCon a b
                 | RecCon { recCon1 :: a, recCon2 :: b}
                 | a :^: b
  deriving (Eq, Lift)

data Prim = Prim Char# Double# Int# Float# Word#
  deriving (Eq, Lift)

-- We can't test this for equality easily due to the unstable nature of
-- primitive string literal equality. We include this anyway to ensure that
-- deriving Lift for datatypes with Addr# in them does in fact work.
data AddrHash = AddrHash Addr#
  deriving Lift

data Empty deriving Lift

data family DataFam a b c

data instance DataFam Int b c = DF1 Int | DF2 b
  deriving (Eq, Lift)

newtype instance DataFam Char b c = DF3 Char
  deriving (Eq, Lift)

algDT1, algDT2, algDT3 :: AlgDT Int String ()
algDT1 = NormalCon 1 "foo"
algDT2 = RecCon 2 "bar"
algDT3 = 3 :^: "baz"

prim :: Prim
prim = Prim 'a'# 1.0## 1# 1.0# 1##

df1, df2 :: DataFam Int Char ()
df1 = DF1 1
df2 = DF2 'a'

df3 :: DataFam Char () ()
df3 = DF3 'b'
