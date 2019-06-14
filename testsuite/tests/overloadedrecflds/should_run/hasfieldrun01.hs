{-# LANGUAGE DataKinds
           , DatatypeContexts
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , TypeFamilies
           , TypeApplications
  #-}
{-# OPTIONS_GHC -dcore-lint #-}

import GHC.Records (HasField(..), getField, setField)

data S a where
  MkS :: { soo :: Either p q } -> S (p,q)

type family B where B = Bool

data T = MkT { foo :: Int, bar :: B }

data U a b = MkU { baf :: a }
  deriving Show

data family V a b c d
data instance V x Int y [z] = MkVInt { baz :: (x, y, z, Bool) }

data W a where
  MkW :: { woo :: a } -> W [a]

data Eq a => X a = MkX { xoo :: a }
  deriving Show
data Y a = Eq a => MkY { yoo :: a }

data Z = MkZ1 { partial :: Int, total :: Bool }
       | MkZ2 { total :: Bool }
  deriving Show

s :: S ((), Bool)
s = MkS (Right True)

t = MkT 42 True

u :: U Char Char
u = MkU 'x'

v = MkVInt (42, 'x', True, False)

w = MkW True

x = MkX True

y = MkY True

z = MkZ2 False

-- A virtual foo field for U
instance HasField "foo" (U a b) [Char] where
  hasField r = (const r, "virtual")

main = do print (getField @"soo" s)
          print (getField @"foo" t)
          print (getField @"foo" (setField @"foo" t 11))
          print (getField @"bar" t)
          print (getField @"bar" (setField @"bar" t False))
          print (getField @"baf" u)
          print (setField @"baf" u 'y')
          print (getField @"foo" u)
          print (setField @"foo" u "ignored")
          print (getField @"baz" v)
          print (getField @"baz" (setField @"baz" v (40 :: Int, 'y', False, True)))
          print (getField @"woo" w)
          print (getField @"woo" (setField @"woo" w False))
          print (getField @"xoo" x)
          print (setField @"xoo" x False)
          print (getField @"yoo" y)
          print (getField @"yoo" (setField @"yoo" y False))
          print (getField @"total" (setField @"total" z True))
          print (setField @"partial" z 42)  -- Should throw a "No match" error
