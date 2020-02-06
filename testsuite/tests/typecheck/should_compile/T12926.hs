{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module A where

import GHC.Base
import qualified Data.Vector.Unboxed.Base
import qualified Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import qualified Data.Vector.Generic.Mutable.Base
import Data.Vector.Generic (fromList)

data A = A Int Int Int

instance Data.Vector.Unboxed.Base.Unbox A

newtype instance Data.Vector.Unboxed.Base.MVector s_a4iX A
  = MV_A (Data.Vector.Unboxed.Base.MVector s_a4iX (Int, Int, Int))

instance MVector Data.Vector.Unboxed.Base.MVector A where
  basicLength (MV_A v) =
    basicLength v
  basicUnsafeSlice idx len (MV_A v) =
    MV_A (basicUnsafeSlice idx len v)
  basicUnsafeNew len =
    MV_A `liftM` (basicUnsafeNew len)
  basicUnsafeWrite (MV_A v) idx val_a4iW =
    basicUnsafeWrite v idx ((\ (A a_a4iT b_a4iU c_a4iV) -> (a_a4iT, b_a4iU, c_a4iV)) val_a4iW)

newtype instance Data.Vector.Unboxed.Base.Vector A =
  V_A (Data.Vector.Unboxed.Base.Vector (Int, Int, Int))

instance Data.Vector.Generic.Base.Vector Data.Vector.Unboxed.Base.Vector A where

mkA :: Data.Vector.Unboxed.Base.Vector A
mkA = fromList []
