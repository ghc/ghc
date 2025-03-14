{-# LANGUAGE TypeFamilies #-}
module T3990b where

type family F a

data D1 = MkD1 {-# UNPACK #-} !(F Int)
    -- This should actually get unpacked

data D2 = MkD2 {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int

type instance F Int = D2

test_case :: D1
test_case = MkD1 (MkD2 1 1)
