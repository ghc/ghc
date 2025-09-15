{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PolyKinds, DataKinds       #-}

module T5716 where

import Data.Kind (Type)

data family DF a
data instance DF Int = DFInt

data U = U1 (DF Int)

data I :: U -> Type where I1 :: I (U1 DFInt)
