{-# LANGUAGE DataKinds
           , FlexibleInstances
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

t = MkT 42 True

u :: U Char Char
u = MkU 'x'

v = MkVInt (42, 'x', True, False)

-- A virtual foo field for U
instance HasField "foo" (U a b) [Char] where
  fromLabel _ = "virtual"

main = do print (fromLabel @"foo" t)
          print (fromLabel @"bar" t)
          print (fromLabel @"baf" u)
          print (fromLabel @"foo" u)
          print (fromLabel @"baz" v)
