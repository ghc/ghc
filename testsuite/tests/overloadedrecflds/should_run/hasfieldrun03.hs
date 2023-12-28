{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnliftedDatatypes #-}

import GHC.Exts (Int#, Int(I#), RuntimeRep(..), UnliftedType, TYPE, (+#))
import GHC.Records (HasField(..))

data U = MkU { f1 :: Int# }

instance a ~ Int# => HasField "f2" U a where
  getField u = f1 u +# 1#

type V :: TYPE IntRep -> UnliftedType
data V x = MkV { g1 :: x }

instance (a ~ Int#, x ~ Int#) => HasField "g2" (V x) a where
  getField u = g1 u +# 3#

main = do print (I# (getField @"f1" (MkU 42#)))
          print (I# (getField @"f2" (MkU 42#)))
          print (I# (getField @"g1" (MkV 100#)))
          print (I# (getField @"g2" (MkV 100#)))
          let u = MkU 1#
          let v = MkV 0#
          print (I# (u.f1))
          print (I# (u.f2))
          print (I# (v.g1))
          print (I# (v.g2))
