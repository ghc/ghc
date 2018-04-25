{-# OPTIONS -fno-warn-orphans #-}
-- | `DPrim` and `DT` instances for scalar types. 
module Data.Array.Parallel.Unlifted.Distributed.Data.Scalar.Base
where
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DPrim
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Data.Word
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import qualified Data.Vector.Unboxed.Mutable                    as MV
import Prelude as P


-- Integer -----------------------------------------------------------------------
-- FIXME: fake instances
instance DPrim Integer
instance DT Integer


-- Char -----------------------------------------------------------------------
instance DPrim Char where
  mkDPrim               = DChar
  unDPrim (DChar a)     = a

  mkMDPrim              = MDChar
  unMDPrim (MDChar a)   = a


instance DT Char where
  data Dist  Char       = DChar  !(V.Vector    Char)
  data MDist Char s     = MDChar !(MV.STVector s Char)

  indexD                = primIndexD
  newMD                 = primNewMD
  readMD                = primReadMD
  writeMD               = primWriteMD
  unsafeFreezeMD        = primUnsafeFreezeMD
  sizeD                 = primSizeD
  sizeMD                = primSizeMD


-- Int ------------------------------------------------------------------------
instance DPrim Int where
  mkDPrim               = DInt
  unDPrim (DInt a)      = a

  mkMDPrim              = MDInt
  unMDPrim (MDInt a)    = a


instance DT Int where
  data Dist  Int        = DInt  !(V.Vector    Int)
  data MDist Int s      = MDInt !(MV.STVector s Int)

  indexD                = primIndexD
  newMD                 = primNewMD
  readMD                = primReadMD
  writeMD               = primWriteMD
  unsafeFreezeMD        = primUnsafeFreezeMD
  sizeD                 = primSizeD
  sizeMD                = primSizeMD

  measureD n = "Int " P.++ show n


-- Word8 ----------------------------------------------------------------------
instance DPrim Word8 where
  mkDPrim               = DWord8
  unDPrim (DWord8 a)    = a

  mkMDPrim              = MDWord8
  unMDPrim (MDWord8 a)  = a


instance DT Word8 where
  data Dist  Word8      = DWord8  !(V.Vector    Word8)
  data MDist Word8 s    = MDWord8 !(MV.STVector s Word8)

  indexD                = primIndexD
  newMD                 = primNewMD
  readMD                = primReadMD
  writeMD               = primWriteMD
  unsafeFreezeMD        = primUnsafeFreezeMD
  sizeD                 = primSizeD
  sizeMD                = primSizeMD


-- Float ----------------------------------------------------------------------
instance DPrim Float where
  mkDPrim               = DFloat
  unDPrim (DFloat a)    = a

  mkMDPrim              = MDFloat
  unMDPrim (MDFloat a)  = a


instance DT Float where
  data Dist  Float      = DFloat  !(V.Vector    Float)
  data MDist Float s    = MDFloat !(MV.STVector s Float)

  indexD                = primIndexD
  newMD                 = primNewMD
  readMD                = primReadMD
  writeMD               = primWriteMD
  unsafeFreezeMD        = primUnsafeFreezeMD
  sizeD                 = primSizeD
  sizeMD                = primSizeMD


-- Double ---------------------------------------------------------------------
instance DPrim Double where
  mkDPrim               = DDouble
  unDPrim (DDouble a)   = a

  mkMDPrim              = MDDouble
  unMDPrim (MDDouble a) = a


instance DT Double where
  data Dist  Double     = DDouble  !(V.Vector    Double)
  data MDist Double s   = MDDouble !(MV.STVector s Double)

  indexD                = primIndexD
  newMD                 = primNewMD
  readMD                = primReadMD
  writeMD               = primWriteMD
  unsafeFreezeMD        = primUnsafeFreezeMD
  sizeD                 = primSizeD
  sizeMD                = primSizeMD

