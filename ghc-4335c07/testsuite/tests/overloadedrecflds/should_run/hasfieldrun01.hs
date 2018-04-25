{-# LANGUAGE DataKinds
           , DatatypeContexts
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , TypeFamilies
           , TypeApplications
  #-}

import GHC.Records (HasField(..))

type family B where B = Bool

data T = MkT { foo :: Int, bar :: B }

data U a b = MkU { baf :: a }

data family V a b c d
data instance V x Int y [z] = MkVInt { baz :: (x, y, z, Bool) }

data W a where
  MkW :: { woo :: a } -> W [a]

data Eq a => X a = MkX { xoo :: a }
data Y a = Eq a => MkY { yoo :: a }

t = MkT 42 True

u :: U Char Char
u = MkU 'x'

v = MkVInt (42, 'x', True, False)

w = MkW True

x = MkX True

y = MkY True

-- A virtual foo field for U
instance HasField "foo" (U a b) [Char] where
  getField _ = "virtual"

main = do print (getField @"foo" t)
          print (getField @"bar" t)
          print (getField @"baf" u)
          print (getField @"foo" u)
          print (getField @"baz" v)
          print (getField @"woo" w)
          print (getField @"xoo" x)
          print (getField @"yoo" y)
