
-- | Distribution of values of primitive types.
module Data.Array.Parallel.Unlifted.Distributed.Data.Ordering
where
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DPrim
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import qualified Data.Vector.Unboxed.Mutable                    as MV
import Prelude as P


instance DPrim Ordering where
  mkDPrim               = DOrdering
  unDPrim (DOrdering a) = a

  mkMDPrim                = MDOrdering
  unMDPrim (MDOrdering a) = a


instance DT Ordering where
  data Dist  Ordering   = DOrdering  !(V.Vector    Ordering)
  data MDist Ordering s = MDOrdering !(MV.STVector s Ordering)

  indexD         = primIndexD
  newMD          = primNewMD
  readMD         = primReadMD
  writeMD        = primWriteMD
  unsafeFreezeMD = primUnsafeFreezeMD
  sizeD          = primSizeD
  sizeMD         = primSizeMD
